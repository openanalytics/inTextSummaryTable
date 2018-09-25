#' Get summary statistics table
#' @inheritParams computeSummaryStatistics
#' @inheritParams exportSummaryStatisticsTable
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	var = "AVAL", 
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	colVar = NULL, 
	subjectVar = "USUBJID",
	labelVars = NULL, 
	landscape = FALSE, 
	title = "Table: Descriptive statistics",
	file = "summaryStatisticsTable.docx"){

	summaryTable <- computeSummaryStatistics(
		data = data,  
		var = var, 
		colVar = colVar,
		rowVar = rowVar,
		subjectVar = subjectVar
	)
	
	ft <- exportSummaryStatisticsTable(
		data = summaryTable, 
		rowVar = rowVar, rowVarLab = rowVarLab,
		colVar = colVar,
		title = title,
		labelVars = labelVars,
		file = file, landscape = landscape
	)
	
	return(ft)

}

