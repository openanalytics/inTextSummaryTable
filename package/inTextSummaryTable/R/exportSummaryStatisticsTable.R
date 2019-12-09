#' Export a summary table in \code{docx} format.
#' @param outputType String with output type, 'data.frame' or 'flextable'.
#' @param rowVarInSepCol Character vector with \code{rowVar}
#' that should be included in multiple columns.
#' By default, if \code{outputType} is: 
#' \itemize{
#' \item{'flextable': }{NULL, the row variables are nested in the first column 
#' of the table}
#' \item{otherwise: }{\code{rowVar}, the row variables are included in
#' separated columns}
#' }
#' @param pageDim Numeric vector of length 2 with page width and height
#' depending on \code{outputType}:
#' \itemize{
#' \item{'flextable': }{in inches in portrait format}
#' \item{'DT': }{number of rows in the table (currently only
#' the height is used (e.g. \code{c(NA, 4)})
#' }}
#' @param statsLayout String with layout for the statistics names 
#' (in case more than one statistic is included), among:
#' \itemize{
#' \item{row (by default when \code{outputType} is: 'flextable'): }{
#' All statistics are included in different rows 
#' in the first column of the table
#' }
#' \item{'col' (by default when \code{outputType} is: 'DT'): }{
#' Statistics are included in separated columns (last row of the header).
#' This option is not compatible with categorical variable(s).
#' }
#' \item{'rowInSepCol': }{
#' Statistics are included in different rows, but in a separated column than
#' the \code{rowVar} variable(s)
#' }
#' }
#' @inheritParams formatSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTableToFlextable
#' @inheritParams exportSummaryStatisticsTableToDT
#' @return Depending on the \code{outputType}:
#' \itemize{
#' \item{'data.frame-base': }{input summary table in a long format with
#' all computed statistics}
#' \item{'data.frame': }{summary table in a wide format (
#' different columns for each \code{colVar}), with specified labels}
#' \item{'flextable' (by default): }{\code{\link[flextable]{flextable}}
#'  object with summary table}
#' \item{'DT': }{\code{\link[DT]{datatable}} object with summary table}
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
	rowVarInSepCol = if(outputType != "flextable")	rowVar, 
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
	statsLayout = ifelse(outputType == "flextable", "row", "col"),
	statsValueLab = "StatisticValue",
	emptyValue = "-",
	# extra
	labelVars = NULL, 
	file = NULL, landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL,
	outputType = "flextable",
	# flextable-specific
	footer = NULL,
	style = "report", colorTable = getColorTable(style = style),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", pageDim = NULL,
	# DT-specific
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	...){

	outputType  <- match.arg(outputType, 
		choices = c("flextable", "DT", "data.frame", "data.frame-base")
	)
	
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
	
	createFt <- "flextable" %in% outputType | (!is.null(file) && file_ext(file) == "docx")
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
	
	if("DT" %in% outputType){
		
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
			labelVars = labelVars,
			...
		)
		
	}
	
	result <- sapply(outputType, function(type)
		switch(type, 
			'data.frame-base' = summaryTable,
			'data.frame' = summaryTableLong,	
			'flextable' = summaryTableFt,
			'DT' = summaryTableDT
		)
	, simplify = FALSE)

	if(length(outputType) == 1)
		result <- result[[1]]
		
	return(result)
	
}