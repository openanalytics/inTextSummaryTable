#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowVar variable(s) of \code{data} used for
#' grouping in row in the final table.
#' @param colVar variable(s) of \code{data} used 
#' for grouping in column in the final table. The total 
#' for each subgroup across \code{rowVar} is computed.
#' @inheritParams getSummaryStatistics
#' @return data.frame of class 'countTable' or 'summaryTable',
#' depending on the 'type' parameter with statistics in columns,
#' either if \code{type} is:
#' \itemize{
#' \item{'summaryTable': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'Mean': }{mean of \code{var}}
#' \item{'SD': }{standard deviation of \code{var}}
#' \item{'SE': }{standard error of \code{var}}
#' \item{'Median': }{median of \code{var}}
#' \item{'Min': }{minimum of \code{var}}
#' \item{'Max': }{maximum of \code{var}}
#' \item{'Perc': }{percentage of subjects}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'Perc': }{percentage of subjects}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill
#' @export
computeSummaryStatistics <- function(data,  
	var = "AVAL", varIgnore = NULL,
	colVar = NULL,
	rowVar = NULL,
	subjectVar = "USUBJID",
	type = ifelse(is.numeric(data[, var]), "summaryTable", "countTable")
){
	
	# ignore certain elements
	if(!is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(rowVar, colVar),function(x){
		getSummaryStatistics(data = x, var = var, type = type)
	})
	
	# get statistics for the entire dataset
	if(!is.null(colVar)){
		
		# total per column variable
		switch(type,
			'summaryTable' = {
				summaryTableTotal <- ddply(data, colVar, function(x)
					getSummaryStatistics(data = x, var = var, type = type)
				)
			},
			'countTable' = {
				summaryTableTotal <- getSummaryStatistics(data = data, var = colVar, type = type)
				rowVar <- c(rowVar, var)
			}
		)
		summaryTableTotal[, rowVar] <- "Total"
		summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
		
		# percentage of subjects
		summaryTable <- ddply(summaryTable, colVar, function(x){
			idxTotal <- which(rowSums(x[, rowVar] == "Total") == length(rowVar))
			cbind(x, Perc = x$N/x[idxTotal, "N"])			
		})
		
	}
	
	class(summaryTable) <- c(type, class(summaryTable))
	
	return(summaryTable)
	
}

#' Get summary statistics of interest
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable to use,
#' 'AVAL' by default
#' @param subjectVar string, variable of \code{data} with subject ID,
#' 'USUBJID' by default
#' @param type string with type of summary table: 'summaryTable' 
#' (by default if \code{var} is numeric) or 'countTable'
#' @return data.frame with summary statistics in columns,
#' depending if \code{type} is:
#' \itemize{
#' \item{'summary': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'Mean': }{mean of \code{var}}
#' \item{'SD': }{standard deviation of \code{var}}
#' \item{'SE': }{standard error of \code{var}}
#' \item{'Median': }{median of \code{var}}
#' \item{'Min': }{minimum of \code{var}}
#' \item{'Max': }{maximum of \code{var}}
#' }
#' }
#' \item{'count': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom stats na.omit median sd
#' @export
getSummaryStatistics <- function(data, var,
	subjectVar = "USUBJID",
	type = ifelse(is.numeric(data[, var]), "summaryTable", "countTable")){
	
	val <- data[, var]
	
	getNSubjects <- function(x)
		as.integer(n_distinct(x[, subjectVar]))

	res <- switch(type,
		'summaryTable' = data.frame(
			N = getNSubjects(data),
			Mean = mean(val),
			SD = sd(val),
			SE = sd(val)/sqrt(length(val)),
			Median = median(val),
			Min = min(val),
			Max = max(val)
		),
		'countTable' = if(!is.null(var)){
			ddply(data, var, function(x)
				data.frame(N = getNSubjects(x))
			)
		}else	data.frame(N = getNSubjects(data))
	)
	
	return(res)
	
}
