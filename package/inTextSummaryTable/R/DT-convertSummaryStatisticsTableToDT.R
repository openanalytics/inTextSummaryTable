#' Convert summary table to DT
#' @param expandVar Character vector with variables of the summary table which
#' should be expanded in the data (only for 'DT' output).
#' @inheritParams formatSummaryStatisticsTableFt
#' @return \code{\link[DT]{data.tables}}
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG
#' @export
convertSummaryStatisticsTableToDT <- function(summaryTable, rowVar, 
	statsLayout = c("row", "col", "rowInSepCol"),
	statsValueLab = "StatisticValue",
	rowVarInSepCol = NULL, title = NULL,
	statsVar = NULL,
	expandVar = NULL){

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
			do.call(convertSummaryStatisticsTableToDT, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)
		
		return(res)
		
	}
	
	# important: sort data.frame with specified row variables!
	summaryTable <- ddply(summaryTable, rowVar)
	
	# nesting in row variables specified via: 'rowGroup' parameter for DT
	rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
	if(statsLayout == "row" & "Statistic" %in% colnames(data))
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
		expandVarDT <- intersect(colnames(data), expandVar)
		
		# in case expandVar is one statistic,
		# extract indices if statistics are in rows
		expandVarStats <- intersect(statsVar, expandVar)
		if(length(expandVarStats) > 0){
			if(statsLayout == "row" & "Statistic" %in% colnames(summaryTable)){
				idxRow <- which(summaryTable$Statistic %in% expandVarStats)
				idxCol <- grep(statsValueLab, colnames(summaryTable))
				if(length(idxCol) > 0)	stop("Issue with extraction value column.")
				expandIdx <- cbind(row = idxRow, col = idxCol)
			}
		}
		
	}
	
	# create DT
	res <- toDTGLPG(
		data = summaryTable,
		rowGroup = rowGroup,
		caption = title,
		expandIdx = if(length(expandIdx) > 0)	expandIdx,
		expandVar = if(length(expandVarDT) > 0)	expandVarDT
	)
	
	return(res)
	
}