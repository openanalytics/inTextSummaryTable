#' Get summary statistics table
#' @param varLab label for the \code{var} variable 
#' @inheritParams computeSummaryStatistics
#' @inheritParams exportSummaryStatisticsTable
#' @inherit exportSummaryStatisticsTable return
#' @author Laure Cougnaud
#' @export
getSummaryStatisticsTable <- function(
	data, 
	var = "AVAL", varLab = getLabelVar(var, labelVars = labelVars), varIgnore = NULL,
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL,
	colVar = NULL, 
	subjectVar = "USUBJID",
	stats = NULL, dataTotal = NULL, nType = "subject",
	type = ifelse(is.numeric(data[, var]), "summaryTable", "countTable"),
	labelVars = NULL, 
	landscape = FALSE, 	margin = 1, rowPadBase = 2, 
	title = "Table: Descriptive statistics", footer = NULL,
	file = "summaryStatisticsTable.docx"){

	summaryTable <- computeSummaryStatistics(
		data = data,  
		var = var, varIgnore = varIgnore,
		colVar = colVar,
		rowVar = rowVar,
		type = type,
		nType = nType, subjectVar = subjectVar,
		stats = stats, 
		dataTotal = dataTotal
	)
	
	# in case of a count table, 'var' is a column
	if(inherits(summaryTable, "countTable")){
		rowVarInSepCol <- c(rowVarInSepCol, var)
		rowVar <- c(rowVar, var)
		rowVarLab <- c(rowVarLab, varLab)
	}
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable, 
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		colVar = colVar,
		title = title, footer = footer,
		labelVars = labelVars,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase
	)
	
	return(ft)

}

