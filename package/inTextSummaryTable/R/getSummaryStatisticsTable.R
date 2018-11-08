#' Get summary statistics table
#' @param varLab Label for the \code{var} variable.
#' @inheritParams computeSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTable
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	var = NULL, varLab = getLabelVar(var, labelVars = labelVars), varIgnore = NULL,
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL, 
	rowTotalInclude = FALSE, rowTotalLab = NULL,
	rowSubtotalInclude = FALSE,
	colVar = NULL, 
	subjectVar = "USUBJID",
	stats = NULL, filterFct = NULL,
	dataTotal = NULL, 
	type = "summaryTable",
	labelVars = NULL, 
	landscape = FALSE, 	margin = 1, rowPadBase = 2, 
	title = "Table: Descriptive statistics", footer = NULL,
	file = "summaryStatisticsTable.docx"){

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		var = var, varIgnore = varIgnore,
		colVar = colVar,
		rowVar = rowVar, 
		rowTotalInclude = rowTotalInclude,
		rowSubtotalInclude = rowSubtotalInclude,
		type = type,
		subjectVar = subjectVar,
		stats = stats, 
		filterFct = filterFct,
		dataTotal = dataTotal
	)

	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable, 
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		rowSubtotalInclude = rowSubtotalInclude,
		colVar = colVar,
		title = title, footer = footer,
		labelVars = labelVars,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase
	)
	
	return(ft)

}

