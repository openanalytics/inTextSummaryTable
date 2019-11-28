#' Convert summary table to DT
#' @param expandVar Character vector with variables of the summary table which
#' should be expanded in the data (only for 'DT' output).
#' @param pageDim Numeric vector of length 2 with page width and height,
#' in number of rows (currently only
#' the height is used (e.g. \code{c(NA, 4)})
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
	rowVarInSepCol = getAttribute(summaryTable, "rowVarInSepCol"), 
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = c("row", "col", "rowInSepCol"),
	statsValueLab = "StatisticValue",
	title = NULL,
	expandVar = NULL,
	pageDim = NULL,
	labelVars = NULL){
	
	# set row variable labels
	rowVarLabs <- c(
		rowVarLab[setdiff(rowVar, "Statistic")], 
		if(statsLayout != "col")	rowVarLab["Statistic"]
	)
	colNames <- setNames(colnames(dataLong), colnames(dataLong))
	idx <- match(names(rowVarLabs), colNames)
	colNames[na.omit(idx)] <- rowVarLabs[!is.na(idx)]
	colnames(dataLong) <- colNames

	statsLayout <- match.arg(statsLayout)
	
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
	
	if(!is.null(pageDim)){
		pageLength <- pageDim[2]
		if(is.na(pageLength))	pageLength <- Inf
	}else	pageLength <- Inf
	
	# create DT
	res <- toDTGLPG(
		data = summaryTable,
		rowGroup = rowGroup,
		caption = title,
		expandIdx = if(length(expandIdx) > 0)	expandIdx,
		expandVar = if(length(expandVarDT) > 0)	expandVarDT,
		pageLength = pageLength
	)
	
	return(res)
	
}