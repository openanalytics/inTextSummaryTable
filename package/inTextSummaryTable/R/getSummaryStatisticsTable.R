#' Get summary statistics table
#' @param varLab Label for the \code{var} variable.
#' @inheritParams computeSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTable
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	## parameters for summary statistics
	var = NULL, varLab = getLabelVar(var, data = data, labelVars = labelVars), 
	varIgnore = NULL,
	varGeneralLab = "Variable", varSubgroupLab = NULL,
	varIncludeTotal = FALSE,
	## row variables
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarDataLevels = NULL, 
	rowOrder = "auto", rowOrderTotalFilterFct = NULL, rowOrderCatLast = "Other",
	rowVarInSepCol = NULL, 
	# total
	rowVarTotalInclude = NULL, 
	rowVarTotalByVar = NULL,
	rowVarTotalInSepRow = NULL,
	rowTotalLab = NULL,
	rowInclude0 = FALSE, 
	rowAutoMerge = TRUE,
	emptyValue = NULL,
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
	statsGeneralLab = "Statistic",
	statsValueLab = "StatisticValue",
	## extra
	subjectVar = "USUBJID",
	filterFct = NULL,
	dataTotal = NULL, dataTotalPerc = dataTotal, dataTotalRow = NULL,
	type = "auto",
	labelVars = NULL, 
	landscape = (style == "presentation"), 	margin = 1, rowPadBase = 14.4, 
	title = NULL, footer = NULL,
	file = NULL,
	outputType = c("flextable", "data.frame"),
	statsLayout = c("row", "col", "rowInSepCol"),
	style = "report", 
	colorTable = getColorTable(style = style),
	byVar = NULL, byVarLab = getLabelVar(byVar, data = data, labelVars = labelVars),
	colHeaderTotalInclude = TRUE,
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", pageDim = NULL){

	statsLayout <- match.arg(statsLayout)

	outputType <- match.arg(outputType)

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		## variables
		var = var, varLab = varLab,
		varIgnore = varIgnore,
		varGeneralLab = varGeneralLab, varSubgroupLab = varSubgroupLab, 
		varIncludeTotal = varIncludeTotal,
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
		## extra
		type = type,
		subjectVar = subjectVar,
		stats = stats, statsExtra = statsExtra,
		statsVarBy = statsVarBy,
		statsGeneralLab = statsGeneralLab,
		filterFct = filterFct,
		dataTotal = dataTotal, dataTotalPerc = dataTotalPerc, dataTotalRow = dataTotalRow,
		labelVars = labelVars,
		byVar = byVar, byVarLab = byVarLab
	)

	if(is.null(summaryTable))	return(invisible())
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		# total
		rowTotalLab = rowTotalLab,
		rowVarTotalInSepRow = rowVarTotalInSepRow,
		rowVarInSepCol = rowVarInSepCol,
		rowAutoMerge = rowAutoMerge,
		colHeaderTotalInclude = colHeaderTotalInclude,
		statsValueLab = statsValueLab,
		title = title, footer = footer,
		labelVars = labelVars,
		emptyValue = emptyValue,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase,
		outputType = outputType,
		statsLayout = statsLayout,
		style = style, colorTable = colorTable,
		fontsize = fontsize,
		fontname = fontname,
		vline = vline, pageDim = pageDim
	)
	
	return(ft)

}

