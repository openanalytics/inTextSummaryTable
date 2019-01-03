#' Plot subject summary profile
#' @param data Data.frame with summary statistics to represent in the plot,
#' e.g. produced with the \code{\link{computeSummaryStatisticsTable}}.
#' @param xLab String with label for the \code{xVar}.
#' @param meanVar String, variable of \code{data} with the mean variable.
#' @param seVar String, variable of \code{data} with the standard error.
#' @param yLab String with label for the y-axis.
#' @param facetVar String, variable of \code{data} for facetting.
#' @param facetScale String with type of scale used for facetting, 'free_y' by default
#' (fixed scale in the x-axis and free in the y-axis).
#' @param useLinetype Logical, if TRUE (FALSE by default) use also linetype
#' to differenciate the variable specified via \code{colorVar} in the mean line.
#' @param linetypePalette Vector with linetype(s), in case \code{useLinetype} is TRUE.
#' @param title String with title for the plot.
#' @param jitter Numeric with jitter for the x-axis, only used if \code{colorVar} specified.
#' @param label Logical or expression.
#' Points are labelled with \code{meanVar} if set to TRUE,
#' or with the specified expression if \code{label} is an expression.
#' @param sizePoint Size for the point.
#' @param sizeLine Size for the line.
#' @param sizeLabel Size for the label, only used if \code{label} is not NULL.
#' @param xAxisLabs (optional) Named character vector with labels for the x-axis.
#' @param yLim Vector of the length 2 with limits for the y-axis.
#' @param tableText (optional) Character vector with colname of \code{data}
#' or expression from colnames of \code{data} to be represented in
#' the table below the plot.
#' By default, no table is displayed.
#' @param tableLabel Character vector with label for \code{tableText},
#' used as label for the x-axis of the table.
#' @param tableHeight Numeric of length 1 with height for the table.
#' @param byVar Variable(s) of \code{data} for which separated plot(s)
#' should be created.
#' @param hLine (optional) numeric with y-intercept of dashed line to be added.
#' If different thresholds should be used for different elements of the 
#' \code{byVar} or \code{facetVar} variables, the vector should be named
#' with each corresponding element.
#' @param useShape Logical, if TRUE (by default) \code{colorVar} is also used for the shape.
#' @param widthErrorBar Numeric vector of length 1 with width of error bar.
#' @param shapePalette Named vector with shape palette for \code{colorVar}.
#' @inheritParams subjectProfileSummaryTable
#' @return \code{\link[ggplot2]{ggplot}} object or list of such
#' objects of \code{byVar} is specified.
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGColorPalette getGLPGShapePalette getGLPGLinetypePalette getLabelVar
#' @import ggplot2
#' @import cowplot
#' @importFrom ggrepel geom_text_repel
#' @export
subjectProfileSummaryPlot <- function(data,
	xVar = NULL, xLab = getLabelVar(xVar, labelVars = labelVars),
	meanVar = "statMean", seVar = if("statSE" %in% colnames(data))	"statSE", 
	yLab = paste(sub("^stat", "", meanVar), 
		if(!is.null(seVar))	paste("+-", sub("^stat", "", seVar))),
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
	yLim = NULL, xLim = NULL,
	xAxisLabs = NULL,
	sizePoint = GeomPoint$default_aes$size,
	sizeLine = GeomLine$default_aes$size,
	sizeLabel = GeomText$default_aes$size,
	widthErrorBar = GeomErrorbar$default_aes$width,
	tableText = NULL, tableLabel = NULL, tableHeight = 0.2,
	label = FALSE,
	byVar = NULL,
	hLine = NULL,
	style = "report",
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10)){

	if(!is.null(facetVar) & !is.null(tableText)){
		warning("Table cannot be used in combination with 'facetVar', no table is included.")
		tableText <- NULL
	}

	if(!is.null(byVar)){
		inputParams <- as.list(environment())
		if(!byVar %in% colnames(data)){
			warning("'byVar' is not available in the 'data' so is not used.")
			byVar <- FALSE
		}else{
			res <- dlply(data, byVar, function(dataBy){
				byEl <- as.character(unique(dataBy[, byVar]))
				inputParamsBy <- inputParams
				inputParamsBy$data <- dataBy
				inputParamsBy$byVar <- NULL
				inputParamsBy$yLab <- paste(inputParamsBy$yLab, byEl)
				if(!is.null(inputParams$hLine) && byEl %in% names(inputParams$hLine)){
					inputParamsBy$hLine <- inputParams$hLine[[byEl]]
				}else	inputParamsBy$hLine <- NULL
				do.call(subjectProfileSummaryPlot, inputParamsBy)		
			})	
			return(res)
		}
	}

	varNotInData <- setdiff(c(meanVar, seVar), colnames(data))
	if(length(varNotInData) > 0)
		stop("Variable(s): ", toString(varNotInData), "are not in data.")

	if(!is.null(xVar) & is.null(jitter))
		jitter <- if(is.numeric(data[, xVar])){
			# only one element
			if(n_distinct(data[, xVar]) == 1){
				0.1
			# multiple element, take 10% of the min diff
			}else{
				min(diff(sort(unique(data[, xVar]))))*0.1
			}
		}else	0.3
	
	pd <- position_dodge(jitter) # move them .05 to the left and right

	# compute minimum and maximum limits for the error bars
	includeEB <- !is.null(seVar)
	if(includeEB)
		data[, c("ymin", "ymax")] <- data[, meanVar] + data[, seVar] %*% t(c(-1, 1))
	
	# in case variable contain spaces or other character not parsed by ggplot2
	if(!is.null(xVar))
		data$xVar <- data[, xVar]
	if(!is.null(meanVar))
		data$meanVar <- data[, meanVar]
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
	# base plot
	gg <- ggplot(data = data, mapping = do.call(aes_string, aesBase))
	
	# horizontal line(s)
	if(!is.null(hLine)){
		gg <- gg + if(!is.null(facetVar) && !is.null(names(hLine))){
			dataHLine <- setNames(data.frame(names(hLine), hLine), c(facetVar, "yintercept"))
			geom_hline(data = dataHLine, aes(yintercept = yintercept))
		}else{
			geom_hline(yintercept = hLine)
		}
	}
	
	# line + points
	gg <- gg +
		geom_line(do.call(aes_string, aesLine), position = pd, size = sizeLine) +
		geom_point(aes(y = meanVar), position = pd, size = sizePoint)

	if(!(is.logical(label) && !label))
		gg <- gg + geom_text_repel(aes(label = textLabel, y = meanVar), 
			position = pd, size = sizeLabel)

	if(includeEB)
		gg <- gg + geom_errorbar(
			aes_string(ymin = "ymin", ymax = "ymax"), 
			position = pd, width = widthErrorBar
		)

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
	
	gg <- gg + theme_classic() + theme(
		legend.position = "bottom",
		text = element_text(family = fontname, size = fontsize)
	)
	
	# set limits for the axes
	if((!is.null(xLim)) | (!is.null(yLim))){
		argsCoordCart <- list(xlim = xLim, ylim = yLim)
		gg <- gg + do.call(coord_cartesian, argsCoordCart)
	}
	
	# set limits in the x-axis
	# even if not specified, to have correct alignment with table
	if(is.null(xAxisLabs))
		xAxisLabs <- if(is.factor(data[, xVar]))
			levels(data[, xVar])	else	unique(data[, xVar])
	fctScaleX <- if(is.numeric(data[, xVar])){
		# limits should take the jitter into account!
		scaleXLim <- max(jitter, GeomErrorbar$default_aes$width/2)
		scale_x_continuous(
			breaks = unname(xAxisLabs), 
			limits = range(xAxisLabs) + c(-1, 1)*scaleXLim, 
			labels = names(xAxisLabs)
		)
	}else	scale_x_discrete(breaks = unname(xAxisLabs), labels = names(xAxisLabs), drop = FALSE)
	gg <- gg + fctScaleX
	
	res <- if(!is.null(tableText)){
		
		# if xLab is specified only consider
		# the corresponding elements in xVar for the table
		dataTable <- if(!is.null(xAxisLabs)){
			data[data[, xVar] %in% xAxisLabs, ]
		}else	data

		# create plot with table
		ggTable <- subjectProfileSummaryTable(
			data = dataTable, xVar = xVar, 
			text = tableText,
			xLim = xLim,
			xLab = xLab,
			colorVar = colorVar,
			colorPalette = colorPalette,
			showLegend = FALSE, yAxisLabs = !is.null(colorVar),
			style = style,
			fontname = fontname,
			fontsize = fontsize
		)
		
		ggTable <- ggTable + fctScaleX

		# remove legend and title x-axis for base plot (will be included in table plot)
		gg <- gg + theme(legend.position = "none", axis.title.x = element_blank())
		
		# combine base and table plot
		if(tableHeight > 1 | tableHeight < 0)
			stop("Table height should be between 0 and 1.")
		res <- plot_grid(gg, ggTable, ncol = 1,
			align = "hv", axis = "lr",
			rel_heights = c(1-tableHeight, tableHeight)
		)
		
	}else gg
	
	return(res)
	
}

#' Plot a table with \code{ggplot} of a variable of interest 
#' @param data Data.frame with data for the table.
#' @param xVar String, variable of \code{data} with variable for the x-axis.
#' @param text Character vector with colnames of \code{data}
#' or expression based on colnames of \code{data} to extract
#' the text label.
#' @param xLim Vector of the length 2 with limits for the x-axis.
#' @param colorVar String, variable of \code{data} for coloring.
#' @param colorLab String, label for \code{colorVar}, used in the legend.
#' @param colorPalette (named) Vector with color palette.
#' @param xLab String with label for the x-axis.
#' @param textSize Size for the text.
#' @param labelVars Named string with variable labels (names are the variable code).
#' @param showLegend Logical, should the legend be displayed? TRUE by default.
#' @param yAxisLabs Logical, if TRUE include the labels in the y-axis.
#' @param xAxisLabs Vector with labels for the x-axis if \code{xVar}
#' is discrete or vector with limits if continuous.
#' @param style String with subject profile style, either 'report' or 'presentation'.
#' @param fontname String with font name.
#' @param fontsize Numeric vector with font size.
#' @return \code{\link[ggplot2]{ggplot}} object
#' @import ggplot2
#' @author Laure Cougnaud
subjectProfileSummaryTable <- function(
	data, xVar, text, 
	xLim = NULL, 
	colorVar = NULL, colorPalette = NULL,
	colorLab = getLabelVar(colorVar, labelVars = labelVars),
	xLab = NULL,
	textSize = GeomText$default_aes$size,
	labelVars = NULL,
	showLegend = TRUE,
	yAxisLabs = FALSE,
	xAxisLabs = NULL,
	style = "report",
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10)){
	
	data$tableTextLabel <- if(is.expression(text)){
		eval(expr = text, envir = data)
	}else if(is.character(text)){
		if(!text %in% colnames(data)){
			stop(paste0("'", text, "' should be among the columns of 'data'."))
		}else{
			data[, text]
		}
	}else stop("'text' should be an expression of character.")

	# aesthetics
	aesTablePlot <- c(
		list(
			x = xVar,
			y = ifelse(!is.null(colorVar), colorVar, 1),
			label = "tableTextLabel"
		),
		if(!is.null(colorVar)) list(color = colorVar)
	)
	# arguments for geom_text
	argsGeomText <- list(
		mapping = do.call("aes_string", aesTablePlot),
		size = textSize
	)

	# base plot
	ggTable <- ggplot(data = data) + do.call("geom_text", argsGeomText)

	# axis limits
	if(!is.null(xLim))
		ggTable <- coord_cartesian(xlim = xLim)
	
	# labels
	if(!is.null(xLab))
		ggTable <- ggTable + labs(x = xLab)
	
	# color palette
	if(!is.null(colorVar)){
		if(is.null(colorPalette))
			colorPalette <- getGLPGColorPalette(x = data[, colorVar])
		ggTable <- ggTable + scale_color_manual(name = colorLab, values = colorPalette)
	}
	
	if(!is.null(xAxisLabs)){
		ggTable <- if(is.numeric(data[, xVar])){
			ggTable + scale_x_continuous(limits = xAxisLabs)
		}else ggTable + scale_x_discrete(breaks = xAxisLabs)
	}

	# theme
	argsTheme <- c(
		list(
			axis.ticks.y = element_blank(),
			axis.title.y = element_blank(),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank(),
			text = element_text(family = fontname, size = fontsize)
		),
		if(is.null(xLab))	list(axis.title.x = element_blank()),
		if(!yAxisLabs){
			list(axis.text.y = element_blank())
		}else if(!is.null(colorVar)){
			list(axis.text.y = element_text(colour = colorPalette))
		},
		if(!showLegend)	list(legend.position = "none")
	)
	ggTable <- ggTable + theme_classic() + do.call(theme, argsTheme)
	
	return(ggTable)

}
