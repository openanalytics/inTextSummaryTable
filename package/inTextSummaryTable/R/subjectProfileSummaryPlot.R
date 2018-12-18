#' Plot subject summary profile
#' @param data Data.frame with summary statistics to represent in the plot,
#' e.g. produced with the \code{\link{computeSummaryStatisticsTable}}.
#' @param xVar String, variable of \code{data} with variable for the x-axis.
#' @param xLab String with label for the \code{xVar}.
#' @param meanVar String, variable of \code{data} with the mean variable.
#' @param seVar String, variable of \code{data} with the standard error.
#' @param yLab String with label for the y-axis.
#' @param facetVar String, variable of \code{data} for facetting.
#' @param facetScale String with type of scale used for facetting, 'free_y' by default
#' (fixed scale in the x-axis and free in the y-axis).
#' @param colorVar String, variable of \code{data} for coloring.
#' @param colorLab String, label for \code{colorVar}, used in the legend.
#' @param colorPalette (named) Vector with color palette.
#' @param useLinetype Logical, if TRUE (FALSE by default) use also linetype
#' to differenciate the variable specified via \code{colorVar} in the mean line.
#' @param linetypePalette Vector with linetype(s), in case \code{useLinetype} is TRUE.
#' @param title String with title for the plot.
#' @param labelVars Named string with variable labels (names are the variable code).
#' @param jitter Numeric with jitter for the x-axis, only used if \code{colorVar} specified.
#' @param label Logical or expression.
#' Points are labelled with \code{meanVar} if set to TRUE,
#' or with the specified expression if \code{label} is an expression.
#' @param sizePoint Size for the point.
#' @param sizeLine Size for the line.
#' @param sizeLabel Size for the label, only used if \code{label} is not NULL.
#' @param xAxisLabs (optional) Named character vector with labels for the x-axis.
#' @return \code{\link[ggplot2]{ggplot}} object
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGColorPalette getGLPGShapePalette getLabelVar
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
subjectProfileSummaryPlot <- function(data,
	xVar = NULL, xLab = getLabelVar(xVar, labelVars = labelVars),
	meanVar = "Mean", seVar = if("SE" %in% colnames(data))	"SE", 
	yLab = paste(meanVar, if(!is.null(seVar))	paste("+-", seVar)),
	facetVar = NULL, facetScale = "free_y",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	labelVars = NULL,
	useLinetype = TRUE,
	linetypePalette = NULL,
	useShape = TRUE,
	shapePalette = NULL,
	jitter = NULL,
	title = NULL,
	ylim = NULL, xlim = NULL,
	xAxisLabs = NULL,
	sizePoint = GeomPoint$default_aes$size,
	sizeLine = GeomLine$default_aes$size,
	sizeLabel = GeomText$default_aes$size,
	label = FALSE){

	varNotInData <- setdiff(c(meanVar, seVar), colnames(data))
	if(length(varNotInData) > 0)
		stop("Variable(s): ", toString(varNotInData), "are not in data.")

	if(!is.null(xVar) & is.null(jitter))
		jitter <- ifelse(is.numeric(data[, xVar]), 
			ifelse(n_distinct(data[, xVar]) == 1, 0.1, 0.8), 0.3)
	
	pd <- position_dodge(jitter) # move them .05 to the left and right

	# compute minimum and maximum limits for the error bars
	includeEB <- !is.null(seVar)
	if(includeEB)
		data[, c("ymin", "ymax")] <- data[, meanVar] + data[, seVar] %*% t(c(-1, 1))
	
	# in case variable contain spaces or other character not parsed by ggplot2
	data[, c("xVar", "meanVar")] <- data[, c(xVar, meanVar)]
	if(!is.null(colorVar))
		data$colorVar <- data[, colorVar]
	
	if(!(is.logical(label) && !label)){
		data$textLabel <- if(is.expression(label)){
			eval(expr = label, envir = data)
		}else data[, meanVar]
	}
		
	# base plot
	aesBase <- c(
		if(!is.null(xVar))	list(x = "xVar"),
		if(!is.null(colorVar))	list(color = "colorVar"),
		if(!is.null(colorVar) & useShape)	list(shape = "colorVar")
	)
	aesLine <- c(
		list(y = "meanVar"),
		list(group = ifelse(!is.null(colorVar), "colorVar", 1)),
		if(!is.null(colorVar) & useLinetype)	list(linetype = "colorVar")
	)
	gg <- ggplot(data = data, mapping = do.call(aes_string, aesBase)) +
		geom_line(do.call(aes_string, aesLine), position = pd, size = sizeLine) +
		geom_point(aes(y = meanVar), position = pd, size = sizePoint)

	if(!(is.logical(label) && !label))
		gg <- gg + geom_text_repel(aes(label = textLabel, y = meanVar), 
			position = pd, size = sizeLabel)

	if(includeEB)
		gg <- gg + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"), position = pd)

	# facetting
	if(!is.null(facetVar))
		gg <- gg + facet_wrap(aes_string(facetVar), scales = facetScale)
	
	# palettes
	if(!is.null(colorVar)){
		if(is.null(colorPalette))
			colorPalette <- getGLPGColorPalette(x = data[, colorVar])
		gg <- gg + scale_color_manual(name = colorLab, values = colorPalette)
	}
		
	if(useLinetype){
		if(is.null(linetypePalette))
			linetypePalette <- getGLPGLinetypePalette(x = data[, colorVar])
		gg <- gg + scale_linetype_manual(name = colorLab, values = linetypePalette)			
	}
	
	if(useShape){
		if(is.null(shapePalette))
			shapePalette <- getGLPGShapePalette(x = data[, colorVar])
		gg <- gg + scale_shape_manual(name = colorLab, values = shapePalette)			
	}		
	
	# labels for the axes/title
	argsLab <- list(x = xLab, y = yLab, title = title)
	argsLab <- argsLab[!sapply(argsLab, is.null)]
	if(length(argsLab) > 0)
		gg <- gg + do.call(labs, argsLab)
	
	gg <- gg + theme_classic() + theme(legend.position = "bottom")
	
	# set limits for the axes
	if((!is.null(xlim)) | (!is.null(ylim))){
		argsCoordCart <- list(xlim = xlim, ylim = ylim)
		gg <- gg + do.call(coord_cartesian, argsCoordCart)
	}
	
	if(!is.null(xAxisLabs)){
		fctScaleX <- match.fun(paste0("scale_x_", 
			ifelse(is.numeric(data[, xVar]), "continuous", "discrete"))
		)
		gg <- gg + fctScaleX(breaks = xAxisLabs)
	}
	
	return(gg)
	
}

