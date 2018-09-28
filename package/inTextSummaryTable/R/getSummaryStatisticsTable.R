#' Get summary statistics table
#' @param varLab label for the \code{var} variable 
#' @inheritParams computeSummaryStatistics
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
	colVar = NULL, 
	subjectVar = "USUBJID",
	stats = NULL, dataTotal = NULL, nType = "subject",
	type = "summaryTable",
	labelVars = NULL, 
	landscape = FALSE, 	margin = 1, rowPadBase = 2, 
	title = "Table: Descriptive statistics", footer = NULL,
	file = "summaryStatisticsTable.docx"){

	summaryTable <- computeSummaryStatistics(
		data = data,  
		var = var, varIgnore = varIgnore,
		colVar = colVar,
		rowVar = rowVar, 
		rowTotalInclude = rowTotalInclude,
		type = type,
		nType = nType, subjectVar = subjectVar,
		stats = stats, 
		dataTotal = dataTotal
	)

	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable, 
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		colVar = colVar,
		title = title, footer = footer,
		labelVars = labelVars,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase
	)
	
	return(ft)

}

