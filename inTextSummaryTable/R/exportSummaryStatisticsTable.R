#' Export an object
#' @param ... Extra parameters for the corresponding
#' method.
#' @seealso \code{\link{export.summaryTable}}
#' to export \code{\link{summaryTable}} objects.
#' @export
export <- function(...) UseMethod("export")

#' @method export summaryTable
#' @describeIn exportSummaryStatisticsTable export \code{\link{summaryTable}} object
#' @export
export.summaryTable <- function(...){
	exportSummaryStatisticsTable(...)
}

#' Export a summary table to \code{docx}, \code{pptx} 
#' or \code{html} format (interactive table)
#' 
#' The use of \code{export} is recommended.\cr
#' \code{exportSummaryStatisticsTable} is retained for
#' back-compatibility.
#' @param outputType String with output type:
#' \itemize{
#' \item{'flextable' (by default): }{\code{\link[flextable]{flextable}} object, with format for
#' CSR, compatible with Word/PowerPoint export}
#' \item{'DT': }{\code{\link[DT]{datatable}} interactive table,
#' compatible with html export}
#' \item{'data.frame': }{data.frame in wide format (with elements in 
#' \code{colVar} in different columns)}
#' \item{'data.frame-base'}{data.frame in long format (with elements in 
#' \code{colVar} in different rows), useful for QC}
#' }
#' @param file (Optional) Name of the file the table should be exported to, 
#' either:
#' \itemize{
#' \item{string (of length 1). In this case, depending on the
#' file extension, the following is exported: }{
#' \itemize{
#' \item{'txt': summary table in long format ('data.frame-base' \code{outputType})}
#' \item{'docx': summary table in final format is exported ('flextable' \code{outputType})}
#' \item{'html': interactive summary table is exported ('DT' \code{outputType})}
#' }}
#' \item{named character vector in case of multiple exports.
#' The names should correspond to the options in \code{outputType}:}{
#' \itemize{
#' \item{for 'data.frame-base' and 'data.frame': }{filename with 'txt' extension}
#' \item{for 'flextable': }{filename with 'docx' extension}
#' \item{for 'DT': }{filename with 'html' extension}
#' }}
#' }
#' If NULL (by default), the summary table is not exported but only returned as output.
#' If \code{byVar} is specified, each table is exported to a separated
#' file with the suffix: 'file_[i].[ext]' with i the index of the file.
#' @inheritParams inTextSummaryTable-common-args
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
#' If multiple \code{outputType} are specified, a list of those objects, named
#' by \code{outputType}.\cr
#' If \code{byVar} is specified, each object consists of a list of tables,
#' one for each element in \code{byVar}.
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom clinUtils getLabelVar
#' @importFrom tools file_ext
#' @export
exportSummaryStatisticsTable <- function(
	summaryTable, 
	# row
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = NULL, 
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
	statsLayout = getAttribute(
		summaryTable, "statsLayout", 
		default = ifelse("DT" %in% outputType, "col", "row")
	),
	statsValueLab = "StatisticValue",
	statsLabInclude = NULL,
	emptyValue = "-",
	# extra
	labelVars = NULL, 
	file = NULL, 
	title = NULL,
	outputType = "flextable",
	pageDim = NULL,
	columnsWidth = NULL,
	# flextable-specific
	landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	footer = NULL,
	style = "report",
    colorTable = getColorPaletteTable(style = style),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", hline = "auto", 
	# DT-specific
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	...){

	outputType  <- match.arg(outputType, 
		choices = c("flextable", "DT", "data.frame", "data.frame-base"),
		several.ok = TRUE
	)
	
	# check if table should be exported for a specified output type
	getFileForOutputType <- function(outputType, fileExt = NULL){
		fileOutputType <- if(is.null(names(file)) & !is.null(fileExt)){
			idxFile <- which(file_ext(file) == fileExt)
			if(length(idxFile) > 0){
				file[idxFile[1]]
			}
		}else	if(!is.null(names(file)) & outputType %in% names(file)){
			file[outputType]
		}
		return(fileOutputType)
	}
	
	# export input summary table (if required)
	summaryTableFile <- getFileForOutputType("data.frame-base", fileExt = "txt")
	if(!is.null(summaryTableFile))
		writeTable(summaryTable, file = summaryTableFile)
	
	# convert to long format
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
		statsLabInclude = statsLabInclude,
		statsLayout = statsLayout,
		statsValueLab = statsValueLab,
		emptyValue = emptyValue
	)
	# export summary table in long format (if required)
	summaryTableLongFile <- getFileForOutputType("data.frame")
	if(!is.null(summaryTableLongFile)){
		writeTable(summaryTableLong, file = summaryTableLongFile)
	}
	
	summaryTableFtFile <- getFileForOutputType("flextable", fileExt = "docx")
	createFt <- "flextable" %in% outputType | !is.null(summaryTableFtFile)
	if(createFt){
		
		# create flextable only with header to extract dimensions header
		argsExport <- list(
			# for 'format' function
			summaryTable = summaryTableLong,
			rowVar = rowVar,
			rowVarInSepCol = rowVarInSepCol,
			rowVarLab = rowVarLab,
			rowVarTotalInSepRow = rowVarTotalInSepRow,
			rowVarTotalInclude = rowVarTotalInclude,
			statsVar = statsVar,
			vline = vline, hline = hline,
			rowAutoMerge = rowAutoMerge,
			rowVarFormat = rowVarFormat,
			rowTotalLab = rowTotalLab,
			# for 'convert' function
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer,
			style = style, fontsize = fontsize,
			file = summaryTableFtFile,
			fontname = fontname,
			colorTable = colorTable,
			pageDim = pageDim, columnsWidth = columnsWidth,
			labelVars = labelVars
		)
			
		summaryTableFt <- do.call(exportSummaryStatisticsTableToFlextable, argsExport)
		
	}
	
	summaryTableDTFile <- getFileForOutputType("DT", fileExt = "html")
	createDT <- "DT" %in% outputType | !is.null(summaryTableDTFile)
	if(createDT){
		
		summaryTableDT <- exportSummaryStatisticsTableToDT(
			summaryTable = summaryTableLong,
			rowVar = rowVar,
			rowVarLab = rowVarLab,
			rowVarInSepCol = rowVarInSepCol,
			statsVar = statsVar, 
			statsValueLab = statsValueLab,
			expandVar = expandVar,
			noEscapeVar = noEscapeVar,
			barVar = barVar,
			pageDim = pageDim,
			title = title,
			file = summaryTableDTFile,
			columnsWidth = columnsWidth,
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