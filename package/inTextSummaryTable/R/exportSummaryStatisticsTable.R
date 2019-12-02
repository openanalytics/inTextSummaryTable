#' Export a summary table in \code{docx} format.
#' @param outputType String with output type, 'data.frame' or 'flextable'.
#' @param pageDim Numeric vector of length 2 with page width and height
#' depending on \code{outputType}:
#' \itemize{
#' \item{'flextable': }{in inches in portrait format}
#' \item{'DT': }{number of rows in the table (currently only
#' the height is used (e.g. \code{c(NA, 4)})
#' }}
#' @inheritParams formatSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTableToFlextable
#' @inheritParams exportSummaryStatisticsTableToDT
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
#' @importFrom tools file_ext
#' @export
exportSummaryStatisticsTable <- function(
	summaryTable, 
	# row
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = getAttribute(summaryTable, "rowVarInSepCol"), 
	rowVarFormat = NULL,
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
	rowTotalLab = NULL,
	rowVarTotalInSepRow = getAttribute(summaryTable, "rowVarTotalInSepRow"),
	rowAutoMerge = TRUE,
	# column
	colVar = getAttribute(summaryTable, "colVar"), 
	colTotalLab = getAttribute(summaryTable, "colTotalLab", default = "Total"),
	colHeaderTotalInclude = TRUE,
	# stats
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = c("row", "col", "rowInSepCol"),
	statsValueLab = "StatisticValue",
	emptyValue = "-",
	# extra
	labelVars = NULL, 
	file = NULL, landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL,
	outputType = c("flextable", "DT", "data.frame"),
	# flextable-specific
	footer = NULL,
	style = "report", colorTable = getColorTable(style = style),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", pageDim = NULL,
	# DT-specific
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL){

	outputType  <- match.arg(outputType)
	
	statsLayout <- match.arg(statsLayout)
	
	summaryTableLong <- formatSummaryStatisticsTable(
		summaryTable,
		# row
		rowVar = rowVar,
		# column
		colVar = colVar,
		colTotalLab = colTotalLab,
		colHeaderTotalInclude = colHeaderTotalInclude,
		# stats
		statsVar = statsVar,
		statsLayout = statsLayout,
		statsValueLab = statsValueLab,
		emptyValue = emptyValue
	)
	
	createFt <- outputType == "flextable" | (!is.null(file) && file_ext(file) == "docx")
	if(createFt){
		
		# create flextable only with header to extract dimensions header
		summaryTableFt <- exportSummaryStatisticsTableToFlextable(
			# for 'format' function
			summaryTable = summaryTableLong,
			rowVar = rowVar,
			rowVarInSepCol = rowVarInSepCol,
			rowVarLab = rowVarLab,
			rowVarTotalInSepRow = rowVarTotalInSepRow,
			rowVarTotalInclude = rowVarTotalInclude,
			statsLayout = statsLayout,
			statsVar = statsVar,
			vline = vline,
			rowAutoMerge = rowAutoMerge,
			rowVarFormat = rowVarFormat,
			rowTotalLab = rowTotalLab,
			# for 'convert' function
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer,
			style = style, fontsize = fontsize,
			file = file,
			fontname = fontname,
			colorTable = colorTable,
			pageDim = pageDim,
			labelVars = labelVars
		)
		
	}
	
	if(outputType == "DT"){
		
		summaryTableDT <- exportSummaryStatisticsTableToDT(
			summaryTable = summaryTableLong,
			rowVar = rowVar,
			rowVarInSepCol = rowVarInSepCol,
			statsVar = statsVar,
			statsLayout = statsLayout, 
			statsValueLab = statsValueLab,
			expandVar = expandVar,
			noEscapeVar = noEscapeVar,
			barVar = barVar,
			pageDim = pageDim,
			title = title,
			labelVars = labelVars
		)
		
	}
	
	result <- switch(outputType,
		'data.frame' = summaryTableLong,	
		'flextable' = summaryTableFt,
		'DT' = summaryTableDT
	)
	
	return(result)
	
}