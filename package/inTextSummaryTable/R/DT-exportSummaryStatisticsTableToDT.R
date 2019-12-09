#' Convert summary table to DT
#' @param expandVar Character vector with variables of the summary table which
#' should be expanded in the data (only for 'DT' output).
#' @param pageDim Numeric vector of length 2 with page width and height,
#' in number of rows (currently only
#' the height is used (e.g. \code{c(NA, 4)})
#' @param noEscapeVar Character vector with variables of \code{summaryTable}
#' which shouldn't be escaped in the table (e.g. containing URLs).
#' @param barVar Character vector with variables of \code{summaryTable}
#' that should be represented as a bar.
#' @param rowVarInSepCol Character vector with \code{rowVar}
#' that should be included in multiple columns.
#' By default, same as \code{rowVar}.
#' @param statsLayout String with layout for the statistics names 
#' (in case more than one statistic is included), among:
#' \itemize{
#' \item{row: }{
#' All statistics are included in different rows 
#' in the first column of the table
#' }
#' \item{'col' (by default): }{
#' Statistics are included in separated columns (last row of the header).
#' This option is not compatible with categorical variable(s).
#' }
#' \item{'rowInSepCol': }{
#' Statistics are included in different rows, but in a separated column than
#' the \code{rowVar} variable(s)
#' }
#' }
#' @inheritParams formatSummaryStatisticsTable
#' @inheritParams formatSummaryStatisticsTableFlextable
#' @inheritParams exportSummaryStatisticsTableToFlextable
#' @inherit glpgUtilityFct::toDTGLPG return
#' @author Laure Cougnaud
#' @importFrom utils head
#' @importFrom glpgUtilityFct toDTGLPG
#' @export
exportSummaryStatisticsTableToDT <- function(
	summaryTable, 
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = rowVar, 
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = "col",
	statsValueLab = "StatisticValue",
	title = NULL,
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	pageDim = NULL,
	labelVars = NULL){

	statsLayout <- match.arg(statsLayout, choices = c("row", "col", "rowInSepCol"))
	
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(seq_along(summaryTable), function(i){
			summaryTableI <- summaryTable[[i]]
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			inputParamsBy$title <- if(length(title) > 1)
				inputParams$title[i]	else	c(
					inputParams$title, 
					strsplit(names(summaryTable)[i], split = "\n")[[1]]
				)
			do.call(exportSummaryStatisticsTableToDT, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)
		
		return(res)
		
	}
	
	# important: sort data.frame with specified row variables!
	summaryTable <- ddply(summaryTable, rowVar)
	
	# nesting in row variables specified via: 'rowGroup' parameter for DT
	rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
	if(statsLayout == "row" & "Statistic" %in% colnames(summaryTable))
		rowVarInRow <- c(rowVarInRow, "Statistic")
	rowGroup <- head(rowVarInRow, -1)
	if(length(rowGroup) == 0)	rowGroup <- NULL
	if(length(rowGroup) > 1){
		rowGroup <- head(rowGroup, 1)
		warning(paste0("Currently multi-level row grouping row variable not available in the 'DT' package",
			", so the rows are grouped by ", rowGroup, " only."))
	}
	
	expandIdx <- expandVarDT <- NULL
	if(!is.null(expandVar)){
		
		# extract expandVar which are in the column names
		expandVarDT <- intersect(colnames(summaryTable), expandVar)
		
		# in case expandVar is one statistic,
		# extract indices if statistics are in rows
		expandVarStats <- intersect(statsVar, expandVar)
		if(length(expandVarStats) > 0){
			if(statsLayout == "row" & "Statistic" %in% colnames(summaryTable)){
				idxRow <- which(summaryTable$Statistic %in% expandVarStats)
				colsInit <- sub("(.+)(\n\\(N=\\d{1,}\\))", "\\1", colnames(summaryTable))
				idxCol <- which(colsInit == statsValueLab)
				if(length(idxCol) != 1)	stop("Issue with extraction statistic value column during the formatting 'expandVar'.")
				expandIdx <- cbind(row = idxRow, col = idxCol)
			}
		}
		
	}
	
	escape <- if(!is.null(noEscapeVar)){
		
		if(is.character(noEscapeVar)){
		
			escape <- which(!colnames(summaryTable) %in% noEscapeVar)
			if(length(escape) == 0)	{
				TRUE
			}else	escape
			
		}else	noEscapeVar
		
	}else	TRUE
	
	# set row variable labels
	colnamesDT <- colnames(summaryTable)
	rowVarLabs <- c(
		rowVarLab[setdiff(rowVar, "Statistic")], 
		if(statsLayout != "col" && "Statistic" %in% colnames(summaryTable))	rowVarLab["Statistic"]
	)
	rowVarLabs <- rowVarLabs[!is.na(rowVarLabs)]
	colnamesDT <- if(length(rowVarLabs) > 0)
		setNames(names(rowVarLabs), rowVarLabs)
	
	# page length
	pageLength <- if(!is.null(pageDim) && !is.na(pageDim[2])){
		pageDim[2]
	}
	
	# bar
	barVar <- intersect(barVar, colnames(summaryTable))
	if(length(barVar) == 0) barVar <- NULL
	
	argsDT <- c(
		list(
			data = summaryTable,
			rowGroup = rowGroup,
			caption = title,
			escape = escape,
			barVar = barVar
		),
		if(!is.null(pageLength))	list(pageLength = pageLength),
		if(length(expandIdx) > 0)	list(expandIdx = expandIdx),
		if(length(expandVarDT) > 0)	list(expandVar = expandVarDT),
		if(length(colnamesDT) > 0)	list(colnames = colnamesDT)
	)
	
	# create DT
	res <- do.call(toDTGLPG, argsDT)
	
	return(res)
	
}