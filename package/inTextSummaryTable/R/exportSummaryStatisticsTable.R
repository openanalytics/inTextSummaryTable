#' Export a summary table in \code{docx} format.

#' @param outputType String with output type, 'data.frame' or 'flextable'.
#' @param style string with table style in case \code{outputType} is 'flextable',
#'  either 'report' or 'presentation'
#' @inheritParams formatSummaryStatisticsTable
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @return Depending on the \code{outputType}:
#' \itemize{
#' \item{'flextable': }{\code{\link[flextable]{flextable}} object with summary table}
#' \item{'data.frame': }{data.frame with summary table}
#' }
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of corresponding summary tables in long format.
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @export
exportSummaryStatisticsTable <- function(
	summaryTable, 
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = getAttribute(summaryTable, "rowVarInSepCol"), 
	rowTotalInclude = getAttribute(summaryTable, "rowTotalInclude", default = FALSE), 
	rowTotalLab = NULL,
	rowSubtotalInclude = getAttribute(summaryTable, "rowSubtotalInclude", FALSE),
	colVar = getAttribute(summaryTable, "colVar"), 
	colHeaderTotalInclude = TRUE,
	statsValueLab = "StatisticValue",
	labelVars = NULL, 
	file = NULL, landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 4,
	title = NULL,
	footer = NULL,
	outputType = c("flextable", "data.frame"),
	statsLayout = c("row", "col", "rowInSepCol"),
	style = "report",
	fontsize = switch(style, 'report' = 8, 'presentation' = 10)){

	outputType  <- match.arg(outputType)
	
	statsLayout <- match.arg(statsLayout)
	
	
	## format table
	summaryTableLong <- formatSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		rowSubtotalInclude = rowSubtotalInclude,
		colVar = colVar,
		statsLayout = statsLayout,
		colHeaderTotalInclude = colHeaderTotalInclude,
		statsValueLab = statsValueLab
	)
	
	if(outputType == "flextable" | !is.null(file)){
		
		# create flextable only with header to extract dimensions header
		summaryTableFt <- convertSummaryStatisticsTableToFlextable(
			summaryTable = summaryTableLong,
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer,
			style = style, fontsize = fontsize,
			file = file
		)
		
	}
	
	result <- switch(outputType,
		'data.frame' = summaryTableLong,	
		'flextable' = summaryTableFt
	)
	
	return(result)
	
}