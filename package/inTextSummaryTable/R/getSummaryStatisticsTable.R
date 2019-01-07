#' Get summary statistics table
#' @param varLab Label for the \code{var} variable.
#' @inheritParams computeSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTable
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	# parameters for summary statistics
	var = NULL, varLab = getLabelVar(var, data = data, labelVars = labelVars), 
	varIgnore = NULL,
	varLabGeneral = "Variable", varLabSubgroup = NULL,
	varIncludeTotal = FALSE,
	# row variables
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarDataLevels = NULL, 
	rowOrder = "auto", rowOrderTotalFilterFct = NULL,
	rowVarInSepCol = NULL, 
	rowTotalInclude = FALSE, rowTotalLab = NULL, 
	rowSubtotalInclude = FALSE,
	rowInclude0 = FALSE, 
	# column variable
	colVar = NULL, colInclude0 = FALSE,
	colVarDataLevels = NULL, 
	colTotalInclude = FALSE,
	subjectVar = "USUBJID",
	stats = NULL, statsExtra = NULL,
	filterFct = NULL,
	dataTotal = NULL, 
	type = "auto",
	labelVars = NULL, 
	landscape = (style == "presentation"), 	margin = 1, rowPadBase = 4, 
	title = NULL, footer = NULL,
	file = NULL,
	outputType = c("flextable", "data.frame"),
	statsLayout = c("row", "col", "rowInSepCol"),
	style = "report",
	byVar = NULL,
	colHeaderTotalInclude = TRUE,
	fontsize = switch(style, 'report' = 8, 'presentation' = 10)){

	statsLayout <- match.arg(statsLayout)

	outputType <- match.arg(outputType)

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		var = var, varLab = varLab,
		varIgnore = varIgnore,
		varLabGeneral = varLabGeneral, varLabSubgroup = varLabSubgroup, 
		varIncludeTotal = varIncludeTotal,
		colVar = colVar, colInclude0 = colInclude0,
		colVarDataLevels = colVarDataLevels,
		colTotalInclude = colTotalInclude, 
		rowVar = rowVar, rowInclude0 = rowInclude0,
		rowVarDataLevels = rowVarDataLevels,
		rowVarInSepCol = rowVarInSepCol,
		rowVarLab = rowVarLab,
		rowOrder = rowOrder, rowOrderTotalFilterFct = rowOrderTotalFilterFct,
		rowTotalInclude = rowTotalInclude,
		rowSubtotalInclude = rowSubtotalInclude,
		type = type,
		subjectVar = subjectVar,
		stats = stats, statsExtra = statsExtra,
		filterFct = filterFct,
		dataTotal = dataTotal,
		labelVars = labelVars,
		byVar = byVar
	)

	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable, 
		rowTotalLab = rowTotalLab,
		colHeaderTotalInclude = colHeaderTotalInclude,
		title = title, footer = footer,
		labelVars = labelVars,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase,
		outputType = outputType,
		statsLayout = statsLayout,
		style = style,
		fontsize = fontsize
	)
	
	return(ft)

}

