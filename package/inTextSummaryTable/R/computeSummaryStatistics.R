#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowVar variable(s) of \code{data} used for
#' grouping in row in the final table.
#' @param colVar variable(s) of \code{data} used 
#' for grouping in column in the final table. The total 
#' for each subgroup across \code{rowVar} is computed.
#' @param stats (optional) named list of expression of summary statistics of interest.
#' The following statistics are recognized, if: 
#' \itemize{
#' \item{\code{type} is a 'summaryTable':}{'N', 'Mean', 'SD', 'SE', 'Median',
#' 'Min', 'Max', 'Perc'}
#' \item{\code{type} is a 'countTable':}{'N','Perc'}
#' }
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.
#' @param varIgnore vector with elements to ignore in the \code{var} variable
#' @param dataTotal data.frame used to extract the Total count, indicated
#' in 'N' in column header and used for the computation of the percentage ('Perc') parameter.
#' It should contain the variables specified by \code{colVar}.
#' @inheritParams getSummaryStatistics
#' @return data.frame of class 'countTable' or 'summaryTable',
#' depending on the 'type' parameter; with statistics in columns,
#' either if \code{type} is:
#' \itemize{
#' \item{'summaryTable': }{
#' \itemize{
#' \item{'N': }{number of subjects orrecords (depending on the \code{nType} parameter)}
#' \item{'Mean': }{mean of \code{var}}
#' \item{'SD': }{standard deviation of \code{var}}
#' \item{'SE': }{standard error of \code{var}}
#' \item{'Median': }{median of \code{var}}
#' \item{'Min': }{minimum of \code{var}}
#' \item{'Max': }{maximum of \code{var}}
#' \item{'Perc': }{percentage of subjects or records (depending on the \code{nType} parameter)}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'N': }{number of subjects or records (depending on the \code{nType} parameter)}
#' \item{'Perc': }{percentage of subjects or records (depending on the \code{nType} parameter)}
#' }
#' }
#' }
#' The computed summary statistics are stored in the 'statsVar' attribute.
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill
#' @export
computeSummaryStatistics <- function(data,  
	var = "AVAL", varIgnore = NULL,
	colVar = NULL,
	rowVar = NULL,
	type = ifelse(is.numeric(data[, var]), "summaryTable", "countTable"),
	nType = "subject",
	subjectVar = "USUBJID",	
	dataTotal = NULL,
	stats = NULL
){
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")
	
	# ignore certain elements
	if(!is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	getSummaryStatisticsCustom <- function(...)
		getSummaryStatistics(..., subjectVar = subjectVar, nType = nType)
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(rowVar, colVar),function(x){
		getSummaryStatisticsCustom(data = x, var = var, type = type)
	})
	
	# get statistics for the entire dataset
	if(!is.null(colVar) & !is.null(rowVar)){
		
		if(!is.null(dataTotal)){
			
			summaryTableTotal <- getSummaryStatisticsCustom(data = dataTotal, var = colVar, type = "countTable")
			rowVar <- c(rowVar, var)
			
		}else{
		
			# total per column variable
			switch(type,
				'summaryTable' = {
					summaryTableTotal <- ddply(data, colVar, function(x)
						getSummaryStatisticsCustom(data = x, var = var, type = type)
					)
				},
				'countTable' = {
					summaryTableTotal <- getSummaryStatisticsCustom(data = data, var = colVar, type = type)
					rowVar <- c(rowVar, var)
				}
			)
			
		}
		summaryTableTotal[, rowVar] <- "Total"
		summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
		
		# ensure that levels of factor are preserved
		summaryTable[, rowVar] <- sapply(rowVar, function(var) 
			if(is.factor(data[, var])){
				factor(summaryTable[, var], levels = c(levels(data[, var]), "Total"))
			}else summaryTable[, var]
		, simplify = FALSE)
		
		# percentage of subjects
		summaryTable <- ddply(summaryTable, colVar, function(x){
			idxTotal <- which(rowSums(x[, rowVar, drop = FALSE] == "Total") == length(rowVar))
			cbind(x, Perc = x$N/x[idxTotal, "N"]*100)			
		})
		
	}
	
	# remove statistics from data
	statsVar <- if(!is.null(stats)){
		
		# compute specified metrics
		if(length(stats) > 1 & is.null(names(stats)))
			stop("'statsFct' should be named.")
		statsDf <- sapply(stats, function(expr)
			eval(expr = expr, envir = summaryTable)
		, simplify = FALSE)
		if(is.null(names(statsDf)))	names(statsDf) <- "Statistic"
		
		# save in summaryTable
		summaryTable <- cbind(summaryTable, statsDf)
		
		if(is.null(names(statsDf)))	"Statistic"	else	names(statsDf)

	}else	c("N", 
				if(type == "summaryTable") c("Mean", "SD", "SE", "Median", "Min", "Max"), 
				if(!is.null(colVar) & !is.null(rowVar))	"Perc"
			)
	
	attributes(summaryTable)$statsVar <- statsVar
	
	class(summaryTable) <- c(type, class(summaryTable))
	
	return(summaryTable)
	
}

#' Get summary statistics of interest
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable to compute statistics on,
#' 'AVAL' by default
#' @param subjectVar string, variable of \code{data} with subject ID,
#' 'USUBJID' by default
#' @param nType string with type of count, either: 
#' \itemize{
#' \item{'subject' :}{count number of subjects in the \code{subjectVar} dataset}
#' \item{'record' :}{count number of records}
#' }
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
	type = ifelse(is.numeric(data[, var]), "summaryTable", "countTable"),
	nType = c("subject", "records")){

	nType <- match.arg(nType)
	type <- match.arg(type, choices = c("summaryTable", "countTable"))
	
	val <- na.omit(data[, var])
	
	getN <- switch(nType,
		'subject' = function(x)	as.integer(n_distinct(x[, subjectVar])),
		'record' = function(x) nrow(x)
	)

	res <- switch(type,
		'summaryTable' = data.frame(
			N = getN(data),
			Mean = mean(val),
			SD = sd(val),
			SE = sd(val)/sqrt(length(val)),
			Median = median(val),
			Min = min(val),
			Max = max(val)
		),
		'countTable' = if(!is.null(var)){
			ddply(data, var, function(x)
				data.frame(N = getN(x))
			)
		}else	data.frame(N = getN(data))
	)
	
	return(res)
	
}
