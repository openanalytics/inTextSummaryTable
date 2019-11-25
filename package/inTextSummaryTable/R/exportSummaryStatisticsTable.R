#' Export a summary table in \code{docx} format.

#' @param outputType String with output type, 'data.frame' or 'flextable'.
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
#' @importFrom glpgStyle getColorTable
#' @export
exportSummaryStatisticsTable <- function(
	summaryTable, 
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = getAttribute(summaryTable, "rowVarInSepCol"), 
	rowVarFormat = NULL,
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
	rowTotalLab = NULL,
	rowVarTotalInSepRow = getAttribute(summaryTable, "rowVarTotalInSepRow"),
	rowAutoMerge = TRUE,
	colVar = getAttribute(summaryTable, "colVar"), 
	colHeaderTotalInclude = TRUE,
	statsValueLab = "StatisticValue",
	emptyValue = "-",
	labelVars = NULL, 
	file = NULL, landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL,
	footer = NULL,
	outputType = c("flextable", "DT", "data.frame"),
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = c("row", "col", "rowInSepCol"),
	style = "report", colorTable = getColorTable(style = style),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", pageDim = NULL){

	outputType  <- match.arg(outputType)
	
	statsLayout <- match.arg(statsLayout)
	
	## format table
	summaryTableLong <- formatSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowVarTotalInSepRow = rowVarTotalInSepRow,
		rowVarTotalInclude = rowVarTotalInclude,
		rowTotalLab = rowTotalLab,
		rowAutoMerge = rowAutoMerge,
		rowVarFormat = rowVarFormat,
		colVar = colVar,
		statsVar = statsVar,
		statsLayout = statsLayout,
		colHeaderTotalInclude = colHeaderTotalInclude,
		statsValueLab = statsValueLab,
		emptyValue = emptyValue,
		vline = vline,
		outputType = outputType
	)
	
	createFt <- outputType == "flextable" | (!is.null(file) & file_ext(file) == "docx")
	if(createFt){
		
		# create flextable only with header to extract dimensions header
		summaryTableFt <- convertSummaryStatisticsTableToFlextable(
			summaryTable = summaryTableLong,
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer,
			style = style, fontsize = fontsize,
			file = file,
			fontname = fontname,
			colorTable = colorTable,
			pageDim = pageDim
		)
		
	}
	
	createDT <- outputType == "DT"
	if(createDT == "DT"){
		
		summaryTableDT <- convertSummaryStatisticsTableToDT(
			summaryTable = summaryTableLong,
			rowVar = rowVar,
			rowVarInSepCol = rowVarInSepCol
		)
		
	}
	
	result <- switch(outputType,
		'data.frame' = summaryTableLong,	
		'flextable' = summaryTableFt,
		'DT' = summaryTableDT
	)
	
	return(result)
	
}