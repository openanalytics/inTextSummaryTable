#' Plot subject summary profile.
#' 
#' The user can either specify a variable for the standard error
#' (\code{seVar}),
#' or directly the variables for the minimum and maximum values for the error
#' bars (\code{minVar}, \code{maxVar}).
#' @param data Data.frame with summary statistics to represent in the plot,
#' e.g. produced with the \code{\link{computeSummaryStatisticsTable}}.
#' @param xLab String with label for the \code{xVar}.
#' @param xGap (optional) Numeric vector of length 2 for which
#' a gap should be created in the x-axis.
#' Only available if \code{xVar} is specified and a numeric variable.
#' Records with \code{xVar} within \code{xGap} are filtered from the plot,
#' vertical lines are included at the min/max of the gap,
#' and the gap is represented as '//' in the x-axis of the plot.
#' @param xGapDiffNew Numeric vector of length 2 with new range
#' of the \code{xGap}. If not specified, the minimum difference between
#' consecutive x elements in the data is used.
#' @param meanVar String, variable of \code{data} with the mean variable.
#' @param seVar String, variable of \code{data} with the standard error.
#' @param minVar,maxVar String, variables of \code{data} with minimum and
#' maximum value for error bar. 
#' If both are specified, \code{seVar} is ignored.
#' @param yLab String with label for the y-axis.
#' If different labels should be used for different elements of
#' \code{byVar} variable, the vector should be named
#' with each corresponding element (collapsed with '.' if multiple).
#' @param facetVar Character vector, variable(s) of \code{data} for facetting.
#' @param facetScale String with type of scale used for facetting, 'free_y' by default
#' (fixed scale in the x-axis and free in the y-axis).
#' @param useLinetype Logical, if TRUE (FALSE by default) use also linetype
#' to differenciate the variable specified via \code{colorVar} in the mean line.
#' @param linetypePalette Vector with linetype(s), in case \code{useLinetype} is TRUE.
#' @param title String with title for the plot.
#' If different labels should be used for different elements of
#' \code{byVar} variable, the vector should be named
#' with each corresponding element (collapsed with '.' if multiple).
#' @param jitter Numeric with jitter for the x-axis, only used if \code{colorVar} specified.
#' @param label Logical or expression or list of expression.
#' Points are labelled with \code{meanVar} if set to TRUE,
#' or with the specified expression if \code{label} is an expression.
#' If a list is specified, 'textLabel' (required) 
#' should contain expression to extract label, 
#' and 'textHjust' and 'textVjust' (optional) may contain expression 
#' specifying horizontal and vertical adjustment of the label.
#' @param labelPadding Amount of padding (space) between each point
#' and its \code{label}, 1.5 lines by default.
#' See parameter \code{point.padding} of the
#' \code{\link[ggrepel]{geom_text_repel}} function.
#' @param sizePoint Size for the point.
#' @param sizeLine Size for the line linking means and error bars.
#' @param sizeLabel Size for the label, only used if \code{label} is not NULL.
#' @param xAxisLabs (optional) Named character vector with labels for the x-axis.
#' @param xAxisExpand Object passed to the 'expand' parameter of:
#' \code{\link[ggplot2]{scale_x_continuous}},
#' (\code{\link[ggplot2]{waiver}} by default).
#' @param yLim Vector of the length 2 with limits for the y-axis.
#' @param yAxisExpand Expansion constants for the limits for the y-axis.
#' See the documentation of the \code{expand} parameter of the 
#' \code{\link[ggplot2]{scale_y_continuous}} function
#' for the available values for this parameter.
#' @param yLimExpand This parameter is deprecated, use \code{yAxisExpand}
#' instead.
#' @param tableText (optional) Character vector with colname of \code{data}
#' or expression from colnames of \code{data} to be represented in
#' the table below the plot.
#' By default, no table is displayed.
#' @param tableTextFontface Font face for the text included
#' in the table.
#' @param tableYAxisLabs Logical, if TRUE (by default)
#' the labels of the \code{colorVar} are included
#' in the y-axis of the table.
#' @param tableHeight Numeric of length 1 with height for the table.
#' @param byVar Variable(s) of \code{data} for which separated plot(s)
#' should be created.
#' @param hLine (optional) numeric with y-intercept of line(s) to be added.
#' If different thresholds should be used for different elements of the 
#' \code{byVar} or \code{facetVar} variables, the vector should be named
#' with each corresponding element (collapsed with '.' if multiple).
#' @param hLineColor String with color for \code{hLine}, 'black' by default.
#' @param hLineLty String with linetype for \code{hLine}, 'solid' by default.
#' @param vLine (optional) numeric with x-intercept of line(s) to be added.
#' If different thresholds should be used for different elements of the 
#' \code{byVar} or \code{facetVar} variables, the vector should be named
#' with each corresponding element (collapsed with '.' if multiple).
#' @param vLineColor String with color for \code{vLine}, 'black' by default.
#' @param vLineLty String with linetype for \code{vLine}, 'solid' by defaul
#' @param useShape Logical, if TRUE (by default) \code{colorVar} is also used for the shape.
#' @param widthErrorBar Numeric vector of length 1 with width of error bar.
#' @param shapePalette Named vector with shape palette for \code{colorVar}.
#' @param themeIncludeVerticalGrid Logical, if TRUE (by default)
#' include theme vertical grid lines (if present in \code{themeFct}).
#' @param ggExtra Extra \code{ggplot} call to be added in main plot.
#' If different calls should be used for different elements of the 
#' \code{byVar} variable, the vector should be named
#' with each corresponding element (collapsed with '.' if multiple).
#' @param yTrans (optional) String with transformation for the y-axis.
#' Currently only 'log10' (or NULL, default) is available.
#' In case error bars go in the negative, their values are set to a 'small enough' value for plotting:
#' \code{min(data)/10} or \code{yLim[1]} if \code{yLim} is specified.
#' @param legendPosition String with legend position.
#' By default, 'bottom' of \code{tableText} is not specified,
#' 'none' otherwise.
#' @param tablePlotMargin Margin between the plot and the table,
#' expressed as \code{\link[grid]{unit}}, 0 by default.
#' @param ... Additional parameters for \code{\link[ggrepel]{geom_text_repel}} or
#' \code{\link[ggplot2]{geom_text}}
#' used for the \code{label}.
#' @inheritParams subjectProfileSummaryTable
#' @return \code{\link[ggplot2]{ggplot}} object or list of such
#' objects of \code{byVar} is specified.
#' @author Laure Cougnaud
#' @importFrom clinUtils getColorPalette getShapePalette getLinetypePalette
#' @importFrom clinUtils getLabelVar
#' @import ggplot2
#' @import cowplot
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales trans_new
#' @export
subjectProfileSummaryPlot <- function(data,
    xVar = NULL, xLab = getLabelVar(xVar, labelVars = labelVars), 
    xAxisExpand = waiver(),
    xGap = NULL, xGapDiffNew = NULL,
    meanVar = "statMean", seVar = if("statSE" %in% colnames(data))	"statSE", 
    minVar = NULL, maxVar = NULL,
    yLab = paste(c(
            sub("^stat", "", meanVar), 
            if(!is.null(minVar) & !is.null(maxVar)){
                  paste0(
                      "(", sub("^stat", "", minVar), ", ", 
                      sub("^stat", "", maxVar), ")"
                  )
                }else	if(!is.null(seVar))	paste("+-", sub("^stat", "", seVar))
        ), collapse = " "),
    facetVar = NULL, facetScale = "free_y",
    colorVar = NULL, colorLab = getLabelVar(colorVar, labelVars = labelVars),
    colorPalette = NULL,
    labelVars = NULL,
    useLinetype = TRUE,
    linetypePalette = NULL,
    useShape = TRUE,
    shapePalette = NULL,
    jitter = NULL,
    title = NULL, caption = NULL,
    yTrans = NULL, yLim = NULL, xLim = NULL,
    yAxisExpand = c(0.05, 0.05),
    yLimExpand = NULL,
    xAxisLabs = NULL,
    sizePoint = GeomPoint$default_aes$size,
    sizeLine = GeomLine$default_aes$size,
    sizeLabel = GeomText$default_aes$size,
    widthErrorBar = GeomErrorbar$default_aes$width,
    tableText = NULL, tableTextFontface = 1,
    tableHeight = 0.1, 
    tableYAxisLabs = !is.null(colorVar),
    tablePlotMargin = unit(0, "pt"),
    label = FALSE, labelPadding = unit(1, "lines"), 
    byVar = NULL,
    hLine = NULL, hLineColor = "black", hLineLty = "solid",
    vLine = NULL, vLineColor = "black", vLineLty = "solid",
    style = "report",
    fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
    fontsize = switch(style, 'report' = 8, 'presentation' = 10),
    themeFct = switch(style, 'report' = theme_classic, 'presentation' = theme_bw),
    themeIncludeVerticalGrid = TRUE,
    ggExtra = NULL,
    legendPosition = ifelse(!is.null(tableText), "none", "bottom"),
    ...){
  
  if(!is.null(yLimExpand)){
    .Deprecated(new = "yAxisExpand", old = "yLimExpand")
    yAxisExpand <- yLimExpand
  }
  
  useMinMax <- !is.null(minVar) & !is.null(maxVar)
  if(!is.null(minVar) & is.null(maxVar))
    warning("'minVar' is not used because 'maxVar' is not specified.")
  if(!is.null(maxVar) & is.null(minVar))
    warning("'maxVar' is not used because 'minVar' is not specified.")
  
  if(!is.null(facetVar) & !is.null(tableText)){
    warning("Table cannot be used in combination with 'facetVar', no table is included.")
    tableText <- NULL
  }
  
  if(!is.null(byVar)){
    inputParams <- c(as.list(environment()), list(...))
    if(!all(byVar %in% colnames(data))){
      warning("'byVar' is not available in the 'data' so is not used.")
      byVar <- FALSE
    }else{
      
      # get parameter in case possibility to have it by 'byVar' element
      # only works if default is NULL
      getParamByEl <- function(x, el, default = x){
        if(!is.null(x) && !is.null(names(x))){
          if(el %in% names(x))	x[[el]]	else	NULL
        }else	default
      }
      
      res <- dlply(data, byVar, function(dataBy){
            byVarEl <- as.matrix(unique(dataBy[, byVar, drop = FALSE]))
            byElST <- paste0(byVarEl, collapse = " ")
            byEl <- paste0(byVarEl, collapse = ".")
            inputParamsBy <- inputParams
            inputParamsBy$data <- dataBy
            inputParamsBy$byVar <- NULL
            inputParamsBy$yLab <- getParamByEl(x = inputParamsBy$yLab, el = byEl, default = paste(inputParamsBy$yLab, byElST))
            inputParamsBy$hLine <- getParamByEl(x = inputParamsBy$hLine, el = byEl)
            inputParamsBy$vLine <- getParamByEl(x = inputParamsBy$vLine, el = byEl)
            inputParamsBy$title <- getParamByEl(x = inputParamsBy$title, el = byEl)
            inputParamsBy$ggExtra <- getParamByEl(x = inputParamsBy$ggExtra, el = byEl)
            do.call(subjectProfileSummaryPlot, inputParamsBy)		
          })	
      return(res)
    }
  }
  
  if(!is.null(yTrans) && !(is.character(yTrans) && yTrans == "log10")){
    warning("Currently 'yTrans' can only have value: 'log10', so this parameter is ignored.")
    yTrans <- NULL
  }
  
  if(!is.null(xGap)){
    if(!is.null(xVar)){
      if(!is.numeric(data[, xVar])){
        warning("'xGap' should only be specified for continuous x-variable, 'xGap' is ignored.")
        xGap <- NULL
      }else	if(length(xGap) != 2){
        warning("'xGap' should be of length 2, 'xGap' is ignored.")
        xGap <- NULL
      }
    }else{
      warning("'xGap' should only be specified if 'xVar' is specified, 'xGap' is ignored.")
      xGap <- NULL
    }
    
  }
  
  varNotInData <- setdiff(c(meanVar, seVar, minVar, maxVar), colnames(data))
  if(length(varNotInData) > 0)
    stop(paste("Variable(s):", toString(varNotInData), "are not in data."))
  
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
  includeEB <- !is.null(seVar) | useMinMax
  if(includeEB){
    
    if(useMinMax){
      dataYMinYMax <- data[, c(minVar, maxVar)]
    }else{
      dataYMinYMax <- data[, meanVar] + data[, seVar] %*% t(c(-1, 1))
    }
    
    # for log scale, set negative values (if any) to a small positive value
    # otherwise the entire error bar is not displayed in ggplot2
    if(!is.null(yTrans) && yTrans == "log10"){
      idxNeg <- which(dataYMinYMax < 0, arr.ind = TRUE)
      if(nrow(idxNeg) > 0){
        yMin <- if(!is.null(yLim))	yLim[1]
        if(!(!is.null(yMin) && !is.na(yMin)))
          yMin <- min(dataYMinYMax[dataYMinYMax > 0])/10
        warning(paste(nrow(idxNeg), "negative value(s) in the error bars, these values",
                "are set to:", prettyNum(yMin), "for plotting."
            ))
        dataYMinYMax[idxNeg] <- yMin
      }
    }
    
    data[, c("ymin", "ymax")] <- dataYMinYMax
    
  }
  
  # in case variable contain spaces or other character not parsed by ggplot2
  if(!is.null(xVar))
    data$xVar <- data[, xVar]
  if(!is.null(meanVar))
    data$meanVar <- data[, meanVar]
  if(!is.null(colorVar))
    data$colorVar <- data[, colorVar]
  
  if(!(is.logical(label) && !label)){
    if(is.list(label)){
      if(!all(sapply(label, is.language))){
        stop("If 'label' is a list, should be a list of expressions.")
      }else{
        if(!"textLabel" %in% names(label))
          stop("If 'label' is a list, should contain at least 'textLabel'.")
        for(labelI in names(label))
          data[, labelI] <- eval(expr = label[[labelI]], envir = data)
      }
    }else	if(is.language(label)){
      data$textLabel <- eval(expr = label, envir = data)
    }else	if(is.logical(label)){
      data$textLabel <- data[, meanVar]
    }
  }
  
  # base plot
  gg <- ggplot()
  
  ## horizontal line(s)
  setLines <- function(gg, inputLine, typeLine = c("hline", "vline"), color, linetype){
    typeLine <- match.arg(typeLine)
    paramName <- switch(typeLine, "hline" = "yintercept", "vline" = "xintercept")
    geomFct <- match.fun(paste0("geom_", typeLine))
    dataLine <- data.frame(line = inputLine, color = color, linetype = linetype, stringsAsFactors = FALSE)
    if(!is.null(facetVar) & !is.null(names(inputLine))){
      facetLine <- names(inputLine)
      if(is.factor(data[, facetVar])){
        facetLine <- factor(facetLine, levels = levels(data[, facetVar]))
      }
      dataLine <- cbind(
          dataLine,
          setNames(data.frame(facetLine, stringsAsFactors = FALSE), facetVar)
      )
      
    }
    for(i in seq_len(nrow(dataLine)))
      gg <- gg + do.call(geomFct, 
          list(
              data = dataLine[i, ], 
              do.call(aes_string, setNames(list("line"), paramName)),
              color = dataLine[i, "color"],
              linetype = dataLine[i, "linetype"]
          )
      )
    return(gg)
  }
  if(!is.null(hLine))
    gg <- setLines(
        gg = gg,
        inputLine = hLine, 
        typeLine = "hline", color = hLineColor,
        linetype = hLineLty
    )
  if(!is.null(vLine))
    gg <- setLines(
        gg = gg,
        inputLine = vLine, 
        typeLine = "vline", color = vLineColor,
        linetype = vLineLty
    )
  
  # if break in the x-axis: remove data within breaks +
  # create 'group' variable used in aesthetic of geom_line
  if(!is.null(xGap)){
    
    # remove data contained within the gaps:
    idxRetained <- which(data$xVar <= xGap[1] | data$xVar >= xGap[2])
    data <- data[idxRetained, ]
    
    # separated lines with the: 'group' aesthetic:
    lineGroup <- c(
        list(ifelse(data[, xVar] < xGap[2], "Group1", "Group2")),
        if(!is.null(colorVar))	list(data$colorVar)
    )
    data$lineGroup <- do.call(interaction, lineGroup)
    
    ## create a transformation on the x-axis 
    # to 'shift' the elements > xGap[2]
    if(is.null(xGapDiffNew)){
      xGapDiffNew <- min(diff(sort(unique(data$xVar))))
    }
    xShift <- xGap[2] - (xGap[1] + xGapDiffNew)
    if(xShift < 0)	xShift <- xGap[2]
    
    # create programmatically transform and inverse functions
    # 1) create empty functions 2) fill function body
    transInvFct <- transFct <- function(x){}
    body(transFct) <- bquote(ifelse(x >= .(xGap[2]), x - .(xShift), x))
    body(transInvFct) <- bquote(ifelse(x >= .(xGap[2]), x + .(xShift), x))
    xTrans <- trans_new(name = "xGap", transform = transFct, inverse = transInvFct)
    
    ## horizontal lines:
    gg <- gg + geom_vline(xintercept = xGap, linetype = 2, color = "grey")
    
    ## add symbol x-axis:
    # vjust not properly placed with: geom = 'text', so use geom_label
    xGapPos <- xGap[1] + xGapDiffNew/2
#		yGapPos <- ifelse(is.null(yLim), min(data$meanVar, na.rm = TRUE), yLim[1]) -
#			yLimExpand[1] * ifelse(is.null(yLim), diff(range(data$meanVar, na.rm = TRUE)), diff(yLim))
    dataGapSym <- data.frame(y = -Inf, x = xGapPos, label = "//")
    gg <- gg + geom_text(
        data = dataGapSym, aes_string(x = "x", y = "y", label = "label"),
        show.legend = FALSE, inherit.aes = FALSE,
        size = sizeLabel,
        hjust = "center", vjust = 1
    )	
#	gg <- gg + annotate(geom = "label", 
#			y = yGapPos, x = xGapPos, label = "//", 
#			hjust = 0.5, vjust = 1,
#			size = sizeLabel,
#			label.size = 0
#		)
    
  }else xTrans <- NULL
  
  # line + points
  # base plot
  aesBase <- c(
      if(!is.null(xVar))	list(x = "xVar"),
      if(!is.null(colorVar))	list(color = "colorVar")
  )
  aesLine <- c(
      aesBase,
      list(y = "meanVar"),
      list(group = ifelse(
              !is.null(xGap), "lineGroup",
              ifelse(!is.null(colorVar), "colorVar", 1)
          )),
      if(!is.null(colorVar) & useLinetype)	list(linetype = "colorVar")
  )
  gg <- gg +
      geom_line(
          mapping = do.call(aes_string, aesLine), 
          position = pd, size = sizeLine, data = data
      )
  gg <- gg +
      geom_point(
          mapping = do.call(aes_string, 
              c(
                  aesBase, 
                  list(y = "meanVar"),
                  if(!is.null(colorVar) & useShape)	list(shape = "colorVar")
              )
          ), 
          position = pd, size = sizePoint, data = data
      )
  
  if(!(is.logical(label) && !label)){
    
    # aes parameters
    aesJust <- setNames(c("textHjust", "textVjust"), c("hjust", "vjust"))
    aesJust <- aesJust[c("textHjust", "textVjust") %in% names(label)]
    aesArgs <- c(aesBase, list(label = "textLabel", y = "meanVar"))
    geomTextFct <- "geom_text_repel"
    if(length(aesJust) > 0){
      geomTextFct <- "geom_text"
      aesArgs <- c(aesArgs, aesJust)
    }
    
    # geom_text(_repel) parameters
    geomTextArgs <- c(
        list(
            mapping = do.call(aes_string, aesArgs), 
            data = data,
            position = pd, size = sizeLabel,
            show.legend = FALSE,
            ...
        ),
        if(geomTextFct == "geom_text_repel")
          list(
              seed = 123, 
              point.padding = labelPadding
          )
    )
    gg <- gg + do.call(geomTextFct, geomTextArgs)
  }
  
  if(includeEB)
    gg <- gg + geom_errorbar(
        mapping = do.call(aes_string, c(aesBase, list(ymin = "ymin", ymax = "ymax"))), 
        data = data,
        size = sizeLine,
        position = pd, width = widthErrorBar
    )
  
  # facetting
  if(!is.null(facetVar))
    gg <- gg + facet_wrap(facetVar, scales = facetScale)
  
  # palettes
  if(!is.null(colorVar)){
    if(is.null(colorPalette)) 
      
      colorPalette <- getColorPalette(
          x = data[, colorVar],
          palette = getOption("inTextSummaryTable.colors.plot")
      )
    gg <- gg + scale_color_manual(name = colorLab, values = colorPalette)
  }
  
  if(useLinetype){
    if(is.null(linetypePalette))     
      linetypePalette <- getLinetypePalette(
          x = data[, colorVar],
          palette = getOption("inTextSummaryTable.linetypes.plot")
      )
    gg <- gg + scale_linetype_manual(name = colorLab, values = linetypePalette)			
  }
  
  if(useShape){
    if(is.null(shapePalette))
      shapePalette <- getShapePalette(
          x = data[, colorVar],
          palette = getOption("inTextSummaryTable.shapes.plot")
      )
    gg <- gg + scale_shape_manual(name = colorLab, values = shapePalette)			
  }	
  
  # labels for the axes/title
  argsLab <- list(
      x = xLab, 
      y = yLab, 
      title = title, 
      caption = if(is.null(tableText))	caption
  )
  argsLab <- argsLab[!sapply(argsLab, is.null)]
  if(length(argsLab) > 0)
    gg <- gg + do.call(labs, argsLab)
  
  argsTheme <- c(
      list(
          legend.position = legendPosition,
          text = element_text(family = fontname, size = fontsize)
      ),
      if(!themeIncludeVerticalGrid)
        list(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
  )
  gg <- gg + themeFct() + do.call(theme, argsTheme)
  
  # y-axis:
  argsYScale <- c(
      if(!is.null(yAxisExpand))	list(expand = yAxisExpand),
      if(!is.null(yTrans))	list(trans = yTrans)
  )
  if(length(argsYScale) > 0)
    gg <- gg + do.call(scale_y_continuous, argsYScale)
  
  # limits/clipping
  argsCoordCart <- c(
      if(!is.null(xLim))	list(xlim = xLim),
      if(!is.null(yLim))	list(ylim = yLim),
      if(!is.null(xGap))	list(clip = "off")
  )
  if(length(argsCoordCart) > 0)
    gg <- gg + do.call(coord_cartesian, argsCoordCart)
  
  # set limits in the x-axis
  # even if not specified, to have correct alignment with table
  if(is.null(xAxisLabs)){
    xAxisLabs <- if(is.factor(data[, xVar]))
          levels(data[, xVar])	else	unique(data[, xVar])
    names(xAxisLabs) <- xAxisLabs
  }
  fctScaleX <- if(is.numeric(data[, xVar])){
        
        # limits should take the jitter into account!
        scaleXLim <- max(jitter, GeomErrorbar$default_aes$width/2)
        
        argsScaleXCont <- c(
            list(
                breaks = unname(xAxisLabs), 
                limits = range(xAxisLabs) + c(-1, 1)*scaleXLim, 
                labels = names(xAxisLabs),
                expand = xAxisExpand
            ),
            if(!is.null(xTrans))	list(trans = xTrans)
        )
        do.call(scale_x_continuous, argsScaleXCont)
      }else{
        scale_x_discrete(
            breaks = unname(xAxisLabs), 
            labels = names(xAxisLabs), 
            drop = FALSE,
            expand = xAxisExpand
        )
      }
  gg <- gg + fctScaleX
  
  if(!is.null(ggExtra))	gg <- gg + ggExtra
  
  res <- if(!is.null(tableText)){
        
        # if xLab is specified only consider
        # the corresponding elements in xVar for the table
        dataTable <- if(!is.null(xAxisLabs)){
              data[data[, xVar] %in% xAxisLabs, ]
            }else	data
        
        # create plot with table
        ggTable <- subjectProfileSummaryTable(
            data = dataTable, xVar = xVar, 
            text = tableText, fontface = tableTextFontface,
            xLim = xLim,
            xLab = xLab,
            colorVar = colorVar, 
            colorLab = colorLab,
            colorPalette = colorPalette,
            legendPosition = legendPosition,
            yAxisLabs = tableYAxisLabs,
            caption = caption,
            style = style,
            fontname = fontname,
            fontsize = fontsize,
            themeFct = themeFct,
            xTrans = xTrans
        )
        
        ggTable <- ggTable + fctScaleX
        
        # remove legend and title x-axis for base plot (will be included in table plot)
        plotMargin <- themeFct()$plot.margin
		plotMargin[3] <- tablePlotMargin # set bottom margin
		gg <- gg + theme(
			legend.position = "none", 
			axis.title.x = element_blank(),
			plot.margin = plotMargin
		)  
		
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

#' Plot a table with \code{ggplot} of a text variable of interest.
#' 
#' The labels extracted based on the \code{text} parameter 
#' and displayed at the x-position based on \code{xVar} and the y-position based on
#' \code{colorVar}.
#' Each group specified in the color variables are displayed in different
#' lines in the plot.
#' @param data Data.frame (in long format) with data for the table.
#' @param xVar String, variable of \code{data} with variable for the x-axis.
#' @param text Character vector with colnames of \code{data}
#' or expression based on colnames of \code{data} to extract
#' the text label.
#' @param xLim Vector of the length 2 with limits for the x-axis.
#' @param colorVar String, variable of \code{data} for coloring.
#' @param colorLab String, label for \code{colorVar}, used in the legend.
#' @param fontface Numeric, fontface for the text.
#' @param colorPalette (named) Vector with color palette.
#' @param xLab String with label for the x-axis.
#' @param textSize Size for the text.
#' @param labelVars Named string with variable labels (names are the variable code).
#' @param showLegend Logical, should the legend be displayed? TRUE by default.
#' @param yAxisLabs Logical, if TRUE include the labels in the y-axis.
#' @param xAxisLabs Vector with labels for the x-axis if \code{xVar}
#' is discrete or vector with limits if continuous.
#' @param style String with subject profile style.
#' This affects the parameters: \code{fontname}, \code{fontsize}
#' and \code{themeFct}.
#' @param fontname String with font name,
#' by default 'Times' if \code{style} is 'report' and
#' 'Tahoma' if \code{style} is 'presentation'.
#' @param fontsize Numeric vector of length 1 with font size,
#' by default 8 if \code{style} is 'report' and
#' 10 if \code{style} is 'presentation'
#' @param pointSize Numeric indicating the size of points in the legend, 1.5 by default
#' @param themeFct Function with ggplot2 theme,
#' by default \code{\link[ggplot2]{theme_classic}} if \code{style} is 'report' and
#' \code{\link[ggplot2]{theme_bw}} if \code{style} is 'presentation'.
#' @param xTrans (optional) ggplot2 transformation
#' for the x-axis.
#' @param legendPosition String with legendPosition,
#' 'right' by default.
#' @param caption String with caption for the plot,
#' NULL by default.
#' @return \code{\link[ggplot2]{ggplot}} object
#' @import ggplot2
#' @importFrom utils packageVersion 
#' @importFrom clinUtils getLabelVar getColorPalette
#' @author Laure Cougnaud and Michela Pasetto
#' @export
subjectProfileSummaryTable <- function(
    data, xVar, text, 
    xLim = NULL, 
    colorVar = NULL, colorPalette = NULL,
    colorLab = getLabelVar(colorVar, labelVars = labelVars),
    fontface = 1,
    xLab = NULL,
    labelVars = NULL,
    caption = NULL,
    showLegend = TRUE,
    legendPosition = ifelse(showLegend, "right", "none"),
    yAxisLabs = FALSE,
    xAxisLabs = NULL,
    style = "report",
    fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
    fontsize = switch(style, 'report' = 8, 'presentation' = 10), # pt
    pointSize = 1.5,
    themeFct = switch(style, 'report' = theme_classic, 'presentation' = theme_bw),
    textSize = fontsize/ggplot2:::.pt,
    xTrans = NULL
){
  
  data$tableTextLabel <- if(is.language(text)){
        eval(expr = text, envir = data)
      }else if(is.character(text)){
        if(!text %in% colnames(data)){
          stop(paste0("'", text, "' should be among the columns of 'data'."))
        }else{
          data[, text]
        }
      }else stop("'text' should be an expression of character.")
  
  if(yAxisLabs & is.null(colorVar))
    warning("Labels for the y-axis are not included because color variable is not specified.")
  
  if(!is.null(colorVar)){
    
    # convert variable to a factor to set correct order of elements in the y-axis
    if(!is.factor(data[, colorVar]))
      data[, colorVar] <- as.factor(data[, colorVar])
    
    # order y rows with first level on top (max y), last level on the bottom (min y)
    data$tableY <- factor(data[, colorVar], levels = rev(levels(data[, colorVar])))
    
  }
  
  # aesthetics
  aesTablePlot <- c(
      list(
          x = xVar,
          y = ifelse(!is.null(colorVar), "tableY", 1),
          label = "tableTextLabel",
          fontface = fontface
      ),
      if(!is.null(colorVar)) list(color = colorVar)
  )
  # arguments for geom_text
  argsGeomText <- list(
      mapping = do.call("aes_string", aesTablePlot),
      size = textSize,			
      show.legend = FALSE
  )
  # arguments for geom_point
  # geom_point is used with size = NA to avoid the 'a' in the legend
  # this workaround will show points in the legend
  idxFontface <- which(names(aesTablePlot) == "fontface")
  aesPoint <- aesTablePlot[- idxFontface]
  aesPoint[["label"]] <- NULL # remove 'label' not used for geom_point
  argsGeomPoint <- list(
      mapping = do.call("aes_string", aesPoint),
      size = NA
  )
  
  # base plot
  # returns warnings because size of points = NA
  ggTable <- ggplot(data = data) + 
      do.call("geom_text", argsGeomText) + 
      do.call("geom_point", argsGeomPoint) +
      guides(colour = guide_legend(override.aes = list(size = pointSize)))
  
  # axis limits
  if(!is.null(xLim))
    ggTable <- ggTable + coord_cartesian(xlim = xLim)
  
  # labels
  # if no x-label, set labs(x = NULL) to remove bottom margin
  ggTable <- ggTable + 
      labs(
          x = if(!is.null(xLab))	xLab,
          caption = caption
      )
  
  # color palette
  if(!is.null(colorVar)){
    if(is.null(colorPalette))
      colorPalette <- getColorPalette(
          x = data[, colorVar],
          palette = getOption("inTextSummaryTable.colors.plot")
      )
    ggTable <- ggTable + scale_color_manual(name = colorLab, values = colorPalette)
  }
  
  if(!is.null(xAxisLabs)){
    ggTable <- if(is.numeric(data[, xVar])){
          argsScaleXCont <- c(
              list(limits = xAxisLabs),
              if(!is.null(xTrans))	list(trans = xTrans)
          )
          ggTable + do.call(scale_x_continuous, argsScaleXCont)
        }else{
          ggTable + scale_x_discrete(breaks = xAxisLabs)
        }
  }
  
  # default: expand by 0.6 units on each side
  # cowplot cut labels if change expand_scale
  if(packageVersion("ggplot2") >= 3.3) {
    ggTable <- ggTable + scale_y_discrete(expand = expansion(add = 0.2))
  } else ggTable <- ggTable + scale_y_discrete(expand = expand_scale(add = 0.2))
  
  
  # theme
  argsTheme <- c(
      list(
          legend.position = legendPosition,
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(family = fontname, size = fontsize),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0,0,0,0), "mm")
      ),
      if(is.null(xLab))	list(axis.title.x = element_blank()),
      if(!yAxisLabs){
            list(axis.text.y = element_blank())
          }else if(!is.null(colorVar)){
            # consider levels of color variable factor to
            # set correct palette in case color palette specified for some elements
            # without data (not represented in the plot)
            paletteEl <- levels(droplevels(data[, "tableY"]))
            colorPaletteAxes <- unname(colorPalette[paletteEl])
            list(axis.text.y = element_text(colour = colorPaletteAxes))
          }
  )
  ggTable <- ggTable + themeFct() + do.call(theme, argsTheme)
  
  return(ggTable)
  
}
