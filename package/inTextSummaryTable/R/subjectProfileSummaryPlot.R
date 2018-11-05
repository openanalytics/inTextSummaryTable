#' Plot subject summary profile
#' @param data Data.frame with summary statistics to represent in the plot,
#' e.g. produced with the \code{\link{computeSummaryStatistics}}.
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
#' @return \code{\link[ggplot2]{ggplot}} object
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getPatientColorPalette getLabelVar
#' @import ggplot2
#' @export
subjectProfileSummaryPlot <- function(data,
	xVar = NULL, xLab = getLabelVar(xVar, labelVars = labelVars),
	meanVar = "Mean", seVar = "SE", yLab = paste(meanVar, "+-", seVar),
	facetVar = NULL, facetScale = "free_y",
	colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
	colorPalette = NULL,
	labelVars = NULL,
	useLinetype = FALSE,
	linetypePalette = NULL,
	jitter = NULL,
	title = NULL){

	if(!is.null(xVar) & is.null(jitter))
		jitter <- ifelse(is.numeric(data[, xVar]), 
			ifelse(n_distinct(data[, xVar]) == 1, 0.1, 0.8), 0.3)
	
	pd <- position_dodge(jitter) # move them .05 to the left and right

	# compute minimum and maximum limits for the error bars
	data[, c("ymin", "ymax")] <- data[, meanVar] + data[, seVar] %*% t(c(-1, 1))
	
	# base plot
	aesBase <- c(
		if(!is.null(xVar))	list(x = xVar),
		if(!is.null(colorVar))	list(color = colorVar)
	)
	aesLine <- c(
		list(y = meanVar),
		list(group = ifelse(!is.null(colorVar), colorVar, 1))
	)
	gg <- ggplot(data = data, mapping = do.call(aes_string, aesBase)) +
		geom_line(do.call(aes_string, aesLine), position = pd) +
		geom_point(aes_string(y = meanVar), position = pd) + 
		geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"), position = pd)

	# facetting
	if(!is.null(facetVar))
		gg <- gg + facet_wrap(aes_string(facetVar), scales = facetScale)
	
	# palettes
	if(!is.null(colorVar)){
		if(is.null(colorPalette))
			colorPalette <- getPatientColorPalette(x = data[, colorVar])
		gg <- gg + scale_color_manual(name = "", values = colorPalette)
	}
		
	if(useLinetype){
		gg <- gg + if(!is.null(linetypePalette))
			scale_linetype_manual(name = "", values = linetypePalette)	else
			scale_linetype_discrete(name = "")				
	}		
	
	# labels for the axes/title
	argsLab <- list(x = xLab, y = yLab, title = title)
	argsLab <- argsLab[!sapply(argsLab, is.null)]
	if(length(argsLab) > 0)
		gg <- gg + do.call(labs, argsLab)
	
	gg <- gg + theme_classic() + theme(legend.position = "bottom")
	
	return(gg)
	
}

