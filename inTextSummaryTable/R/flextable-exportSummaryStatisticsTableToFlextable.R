#' Export summary table to a flextable object,
#' e.g. to be exported in Word or PowerPoint.
#' @param summaryTable Summary table as provided by the 
#' \code{\link{formatSummaryStatisticsTable}}
#' @inheritParams inTextSummaryTable-common-args
#' @inheritParams inTextSummaryTable-flextable-args
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom clinUtils getLabelVar
#' @export
exportSummaryStatisticsTableToFlextable <- function(
	# for 'format' function
	summaryTable,
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarInSepCol = NULL, 
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
	statsLayout = getAttribute(summaryTable, "statsLayout", default = "row"), 	
	statsVar = getAttribute(summaryTable, "statsVar"), 
	statsLabInclude = getAttribute(summaryTable, "statsLabInclude", default = length(statsVar) > 1),
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarTotalInSepRow = NULL,
	vline = c("none", "auto"),
	hline = c("auto", "none"),
	rowAutoMerge = TRUE,
	rowVarFormat = NULL,
	rowTotalLab = NULL,
	# for 'convert' function
	landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL, 
	footer = NULL,
	style = "report",
	colorTable = getColorPaletteTable(style = style),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	file = NULL, pageDim = NULL,
	labelVars = NULL){

	statsLayout <- match.arg(statsLayout, choices = c("row", "col", "rowInSepCol"))
	vline <- match.arg(vline)
	hline <- match.arg(hline)

	# custom formatting for flextable
	summaryTableLong <- formatSummaryStatisticsTableFlextable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarInSepCol = rowVarInSepCol, rowVarTotalInclude = rowVarTotalInclude,
		statsLayout = statsLayout, statsVar = statsVar, statsLabInclude = statsLabInclude,
		rowVarLab = rowVarLab,
		vline = vline,
		hline = hline,
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