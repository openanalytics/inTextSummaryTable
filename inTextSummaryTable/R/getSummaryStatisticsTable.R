#' Get summary statistics table
#' @inheritParams computeSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTable
#' @inheritParams inTextSummaryTable-common-args
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	## parameters for summary statistics
	var = NULL, varFlag = NULL, varLab = NULL, 
	varLabInclude = length(var) > 1,
	varInclude0 = FALSE,
	varIgnore = NULL,
	varGeneralLab = "Variable", varSubgroupLab = "Variable group",
	varIncludeTotal = FALSE, varTotalInclude = FALSE,
	varTotalInSepRow = FALSE,
	## row variables
	rowVar = NULL, rowVarLab = NULL,
	rowVarDataLevels = NULL, 
	rowOrder = "auto", rowOrderTotalFilterFct = NULL, rowOrderCatLast = NULL,
	rowVarInSepCol = NULL,
	rowVarFormat = NULL,
	# total
	rowVarTotalInclude = NULL, 
	rowVarTotalByVar = NULL,
	rowVarTotalInSepRow = NULL,
	rowTotalLab = NULL,
	rowInclude0 = FALSE, 
	rowAutoMerge = TRUE,
	emptyValue = "-",
	rowVarTotalPerc = NULL,
	## column variable
	colVar = NULL, 
	colVarTotal = colVar, colVarTotalPerc = colVarTotal, 
	colInclude0 = FALSE,
	colVarDataLevels = NULL, 
	colTotalInclude = FALSE, colTotalLab = "Total",
	## stats
	stats = NULL, 
	statsExtra = NULL, 
	statsVarBy = NULL,
	statsPerc = c("statN", "statm"),
	statsGeneralLab = "Statistic",
	statsValueLab = "StatisticValue",
	statsLabInclude = NULL,
	## extra
	subjectVar = "USUBJID",
	filterFct = NULL,
	dataTotal = NULL, dataTotalPerc = dataTotal, 
	dataTotalRow = NULL, dataTotalCol = NULL,
	type = "auto",
	byVar = NULL, byVarLab = NULL,
	checkVarDiffBySubj = "error",
	labelVars = NULL, 
	outputType = "flextable",
	statsLayout = ifelse("DT" %in% outputType, "col", "row"),
	landscape = (style == "presentation"), 	margin = 1, rowPadBase = 14.4, 
	title = NULL, footer = NULL,
	file = NULL,
	style = "report", 
	colorTable = getColorPaletteTable(style = style),
	colHeaderTotalInclude = TRUE, colHeaderMerge = TRUE,
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", hline = "auto", 
	pageDim = NULL, columnsWidth = NULL,
	# DT
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	...){

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		## variables
		var = var, varFlag = varFlag, varLab = varLab, 
		varLabInclude = varLabInclude, varInclude0 = varInclude0,
		varIgnore = varIgnore,
		varGeneralLab = varGeneralLab, varSubgroupLab = varSubgroupLab, 
		varIncludeTotal = varIncludeTotal,
		varTotalInclude = varTotalInclude,
		varTotalInSepRow = varTotalInSepRow,
		## columns
		colVar = colVar, 
		colVarTotal = colVarTotal, colVarTotalPerc = colVarTotalPerc, 
		colInclude0 = colInclude0,
		colVarDataLevels = colVarDataLevels,
		colTotalInclude = colTotalInclude, 
		colTotalLab = colTotalLab,
		## rows
		rowVar = rowVar, rowInclude0 = rowInclude0,
		rowVarDataLevels = rowVarDataLevels,
		rowVarLab = rowVarLab,
		rowOrder = rowOrder, 
		rowOrderTotalFilterFct = rowOrderTotalFilterFct,
		rowOrderCatLast = rowOrderCatLast,
		# total
		rowVarTotalInclude = rowVarTotalInclude,
		rowVarTotalByVar = rowVarTotalByVar,
		rowVarTotalPerc = rowVarTotalPerc,
		rowVarTotalInSepRow = rowVarTotalInSepRow,
		## extra
		type = type,
		subjectVar = subjectVar,
		stats = stats, statsExtra = statsExtra,
		statsPerc = statsPerc,
		statsVarBy = statsVarBy,
		statsGeneralLab = statsGeneralLab,
		filterFct = filterFct,
		dataTotal = dataTotal, dataTotalPerc = dataTotalPerc, 
		dataTotalRow = dataTotalRow, dataTotalCol = dataTotalCol,
		labelVars = labelVars,
		byVar = byVar, byVarLab = byVarLab,
		checkVarDiffBySubj = checkVarDiffBySubj
	)

	if(is.null(summaryTable))	return(invisible())
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVarFormat = rowVarFormat,
		# total
		rowTotalLab = rowTotalLab,
		rowVarInSepCol = rowVarInSepCol,
		rowAutoMerge = rowAutoMerge,
		colHeaderTotalInclude = colHeaderTotalInclude,
		colHeaderMerge = colHeaderMerge,
		statsValueLab = statsValueLab,
		statsLabInclude = statsLabInclude,
		title = title, footer = footer,
		labelVars = labelVars,
		emptyValue = emptyValue,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase,
		outputType = outputType,
		statsLayout = statsLayout,
		expandVar = expandVar, noEscapeVar = noEscapeVar, barVar = barVar,
		style = style, colorTable = colorTable,
		fontsize = fontsize,
		fontname = fontname,
		vline = vline, hline = hline,
		pageDim = pageDim, columnsWidth = columnsWidth,
		...
	)
	
	return(ft)

}

