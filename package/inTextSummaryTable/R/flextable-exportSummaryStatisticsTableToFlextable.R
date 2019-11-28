#' Convert summary table to flextable
#' @param summaryTable summary table as provided by teh \code{\link{formatSummaryStatisticsTable}}
#' @param pageDim Numeric vector of length 2 with page width and height,
#' in number of rows (currently only
#' the height is used (e.g. \code{c(NA, 4)})
#' @inheritParams formatSummaryStatisticsTableFlextable
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG
#' @export
exportSummaryStatisticsTableToFlextable <- function(
	# for 'format' function
	summaryTable,
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarInSepCol = NULL, 
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
	statsLayout = c("row", "col", "rowInSepCol"), 
	statsVar = getAttribute(summaryTable, "statsVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarTotalInSepRow = NULL,
	vline = c("none", "auto"),
	rowAutoMerge = TRUE,
	rowVarFormat = NULL,
	rowTotalLab = NULL,
	# for 'convert' function
	landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL, 
	footer = NULL,
	style = "report",
	colorTable = getColorTable(style = style),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	file = NULL, pageDim = NULL,
	labelVars = NULL){

	statsLayout <- match.arg(statsLayout)
	vline <- match.arg(vline)

	# custom formatting for flextable
	summaryTableLong <- formatSummaryStatisticsTableFlextable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarInSepCol = rowVarInSepCol, rowVarTotalInclude = rowVarTotalInclude,
		statsLayout = statsLayout, statsVar = statsVar, 
		rowVarLab = rowVarLab,
		vline = vline,
		rowAutoMerge = rowAutoMerge, rowVarFormat = rowVarFormat,
		rowVarTotalInSepRow = rowVarTotalInSepRow,
		rowTotalLab = rowTotalLab,
		labelVars = labelVars
	)
	
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
	
	return(summaryTableFt)
	
}