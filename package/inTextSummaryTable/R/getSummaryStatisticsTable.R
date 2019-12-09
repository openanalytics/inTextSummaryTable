#' Get summary statistics table
#' @param varLab Label for the \code{var} variable.
#' @inheritParams computeSummaryStatisticsTable
#' @inheritParams exportSummaryStatisticsTable
#' @return Depending on the \code{outputType}:
#' \itemize{
#' \item{'data.frame-base': }{input summary table in a long format with
#' all computed statistics}
#' \item{'data.frame': }{summary table in a wide format (
#' different columns for each \code{colVar}), with specified labels}
#' \item{'flextable' (by default): }{\code{\link[flextable]{flextable}}
#'  object with summary table}
#' \item{'DT': }{\code[DT]{datatable} object with summary table}
#' }
#' If multiple \code{outputType} are specified, a list of those objects, named
#' by \code{outputType}.
#' If \code{byVar} is specified, each object consists of a list of tables,
#' one for each element in \code{byVar}.
#' @author Laure Cougnaud
#' @importFrom glpgStyle getColorTable
#' @export
getSummaryStatisticsTable <- function(
	data, 
	## parameters for summary statistics
	var = NULL, varFlag = NULL, varLab = getLabelVar(var, data = data, labelVars = labelVars), 
	varInclude0 = FALSE,
	varIgnore = NULL,
	varGeneralLab = "Variable", varSubgroupLab = NULL,
	varIncludeTotal = FALSE, varTotalInclude = FALSE,
	varTotalInSepRow = FALSE,
	## row variables
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarDataLevels = NULL, 
	rowOrder = "auto", rowOrderTotalFilterFct = NULL, rowOrderCatLast = "Other",
	rowVarInSepCol = if(outputType != "flextable")	rowVar,
	rowVarFormat = NULL,
	# total
	rowVarTotalInclude = NULL, 
	rowVarTotalByVar = NULL,
	rowVarTotalInSepRow = NULL,
	rowTotalLab = NULL,
	rowInclude0 = FALSE, 
	rowAutoMerge = TRUE,
	emptyValue = "-",
	rowVarTotalPerc = NULL,
	## column variable
	colVar = NULL, 
	colVarTotal = colVar, colVarTotalPerc = colVarTotal, 
	colInclude0 = FALSE,
	colVarDataLevels = NULL, 
	colTotalInclude = FALSE, colTotalLab = "Total",
	## stats
	stats = NULL, 
	statsExtra = NULL, 
	statsVarBy = NULL,
	statsGeneralLab = "Statistic",
	statsValueLab = "StatisticValue",
	## extra
	subjectVar = "USUBJID",
	filterFct = NULL,
	dataTotal = NULL, dataTotalPerc = dataTotal, 
	dataTotalRow = NULL, dataTotalCol = NULL,
	type = "auto",
	labelVars = NULL, 
	landscape = (style == "presentation"), 	margin = 1, rowPadBase = 14.4, 
	title = NULL, footer = NULL,
	file = NULL,
	outputType = "flextable",
	statsLayout = ifelse(outputType == "flextable", "row", "col"),
	style = "report", 
	colorTable = getColorTable(style = style),
	byVar = NULL, byVarLab = getLabelVar(byVar, data = data, labelVars = labelVars),
	colHeaderTotalInclude = TRUE,
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	vline = "none", pageDim = NULL,
	# DT
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	...){

	summaryTable <- computeSummaryStatisticsTable(
		data = data,  
		## variables
		var = var, varFlag = varFlag, varLab = varLab, varInclude0 = varInclude0,
		varIgnore = varIgnore,
		varGeneralLab = varGeneralLab, varSubgroupLab = varSubgroupLab, 
		varIncludeTotal = varIncludeTotal,
		varTotalInclude = varTotalInclude,
		varTotalInSepRow = varTotalInSepRow,
		## columns
		colVar = colVar, 
		colVarTotal = colVarTotal, colVarTotalPerc = colVarTotalPerc, 
		colInclude0 = colInclude0,
		colVarDataLevels = colVarDataLevels,
		colTotalInclude = colTotalInclude, 
		colTotalLab = colTotalLab,
		## rows
		rowVar = rowVar, rowInclude0 = rowInclude0,
		rowVarDataLevels = rowVarDataLevels,
		rowVarLab = rowVarLab,
		rowOrder = rowOrder, 
		rowOrderTotalFilterFct = rowOrderTotalFilterFct,
		rowOrderCatLast = rowOrderCatLast,
		# total
		rowVarTotalInclude = rowVarTotalInclude,
		rowVarTotalByVar = rowVarTotalByVar,
		rowVarTotalPerc = rowVarTotalPerc,
		rowVarTotalInSepRow = rowVarTotalInSepRow,
		## extra
		type = type,
		subjectVar = subjectVar,
		stats = stats, statsExtra = statsExtra,
		statsVarBy = statsVarBy,
		statsGeneralLab = statsGeneralLab,
		filterFct = filterFct,
		dataTotal = dataTotal, dataTotalPerc = dataTotalPerc, 
		dataTotalRow = dataTotalRow, dataTotalCol = dataTotalCol,
		labelVars = labelVars,
		byVar = byVar, byVarLab = byVarLab
	)

	if(is.null(summaryTable))	return(invisible())
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVarFormat = rowVarFormat,
		# total
		rowTotalLab = rowTotalLab,
		rowVarInSepCol = rowVarInSepCol,
		rowAutoMerge = rowAutoMerge,
		colHeaderTotalInclude = colHeaderTotalInclude,
		statsValueLab = statsValueLab,
		title = title, footer = footer,
		labelVars = labelVars,
		emptyValue = emptyValue,
		file = file, landscape = landscape,
		margin = margin, rowPadBase = rowPadBase,
		outputType = outputType,
		statsLayout = statsLayout,
		expandVar = expandVar, noEscapeVar = noEscapeVar, barVar = barVar,
		style = style, colorTable = colorTable,
		fontsize = fontsize,
		fontname = fontname,
		vline = vline, pageDim = pageDim,
		...
	)
	
	return(ft)

}

