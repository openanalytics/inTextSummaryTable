#' Convert summary table to DT
#' @inheritParams formatSummaryStatisticsTableFt
#' @return \code{\link[DT]{data.tables}}
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct toDTGLPG
#' @export
convertSummaryStatisticsTableToDT <- function(summaryTable, rowVar, rowVarInSepCol = NULL){
	
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
	summaryTableLong <- ddply(summaryTableLong, rowVar)
	
	# nesting in row variables specified via: 'rowGroup' parameter for DT
	rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
	rowGroup <- head(rowVarInRow, -1)
	if(length(rowGroup) == 0)	rowGroup <- NULL
	
	# create DT
	res <- toDTGLPG(
		data = summaryTableLong,
		rowGroup = rowGroup,
		caption = title
	)
	
	return(res)
	
}