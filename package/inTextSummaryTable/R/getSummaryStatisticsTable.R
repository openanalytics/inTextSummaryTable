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
	varLabGeneral = "Variable", varLabSubgroup = "Subgroup",
	# row variables
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowOrder = "auto", rowOrderTotalFilterFct = NULL,
	rowVarInSepCol = NULL, 
	rowTotalInclude = FALSE, rowTotalLab = NULL, 
	rowSubtotalInclude = FALSE,
	rowInclude0 = FALSE, 
	# column variable
	colVar = NULL, colInclude0 = FALSE,
	colTotalInclude = FALSE,
	subjectVar = "USUBJID",
	stats = NULL, filterFct = NULL,
	dataTotal = NULL, 
	type = "auto",
	labelVars = NULL, 
	landscape = FALSE, 	margin = 1, rowPadBase = 2, 
	title = "Table: Descriptive statistics", footer = NULL,
	file = "summaryStatisticsTable.docx",
	outputType = c("flextable", "data.frame"),
	statsLayout = c("row", "col", "rowInSepCol"),
	byVar = NULL){

	statsLayout <- match.arg(statsLayout)

	outputType <- match.arg(outputType)

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		var = var, varLab = varLab,
		varIgnore = varIgnore,
		varLabGeneral = varLabGeneral, varLabSubgroup = varLabSubgroup, 
		colVar = colVar, colInclude0 = colInclude0,
		colTotalInclude = colTotalInclude, 
		rowVar = rowVar, rowInclude0 = rowInclude0,
		rowVarInSepCol = rowVarInSepCol,
		rowVarLab = rowVarLab,
		rowOrder = rowOrder, rowOrderTotalFilterFct = rowOrderTotalFilterFct,
		rowTotalInclude = rowTotalInclude,
		rowSubtotalInclude = rowSubtotalInclude,
		type = type,
		subjectVar = subjectVar,
		stats = stats, 
		filterFct = filterFct,
		dataTotal = dataTotal,
		labelVars = labelVars,
		byVar = byVar
	)

	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable, 
		rowVar = rowVar,
		rowVarLab = rowVarLab,
#		rowVar = attributes(summaryTable)$rowVar, 
#		rowVarLab = attributes(summaryTable)$rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
#		rowSubtotalInclude = attributes(summaryTable)$rowSubtotalInclude,
		rowSubtotalInclude = rowSubtotalInclude,
		colVar = colVar,
		title = title, footer = footer,
		labelVars = labelVars,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase,
		outputType = outputType,
		statsLayout = statsLayout
	)
	
	return(ft)

}

