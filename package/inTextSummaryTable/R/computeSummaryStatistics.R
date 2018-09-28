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
#' \item{'N': }{number of subjects}
#' \item{'Mean': }{mean of \code{var}}
#' \item{'SD': }{standard deviation of \code{var}}
#' \item{'SE': }{standard error of \code{var}}
#' \item{'Median': }{median of \code{var}}
#' \item{'Min': }{minimum of \code{var}}
#' \item{'Max': }{maximum of \code{var}}
#' \item{'Perc': }{percentage of subjects}
#' \item{'m': }{number of records}
#' \item{'Percm': }{percentage of records}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'PercN': }{percentage of subjects}
#' \item{'m': }{number of records}
#' \item{'Percm': }{percentage of records}
#' }
#' }
#' }
#' The computed summary statistics are stored in the 'statsVar' attribute.
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill
#' @export
computeSummaryStatistics <- function(data,  
	var = NULL, varIgnore = NULL,
	colVar = NULL,
	rowVar = NULL,
	rowTotalInclude = FALSE,
	type = "summaryTable",
	subjectVar = "USUBJID",	
	dataTotal = NULL,
	stats = NULL
){
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	getSummaryStatisticsCustom <- function(...)
		getSummaryStatistics(..., subjectVar = subjectVar)
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(rowVar, colVar),function(x){
		getSummaryStatisticsCustom(data = x, var = var, type = type,
			filterEmptyVar = (type == "summaryTable")
		)
	})

	if(rowTotalInclude){
		if(!is.null(rowVar)){
			summaryTableTotalData <- ddply(
				.data = data, 
				.variables = colVar, 
				.fun = function(x)
					getSummaryStatisticsCustom(
						data = x, type = type, 
						filterEmptyVar = (type == "summaryTable"),
						var = var
					)
			)
			summaryTableTotalData[, rowVar] <- "Total"
			summaryTable <- rbind.fill(summaryTable, summaryTableTotalData)
			summaryTable[, rowVar] <- colwise(function(x)	
					factor(x, levels = unique(c("Total", if(is.factor(x))	levels(x)	else	sort(unique(x)))))
			)(summaryTable[, rowVar, drop = FALSE])
		}else warning("The row 'total' is not included because no 'rowVar' is specified.")
	}
	
	# get counts for the entire dataset
	if(!is.null(dataTotal)){
		# to have specified order for colVar in case different order 'dataTotal'
		if(!is.null(colVar)){
			dataTotal[, colVar] <- lapply(colVar, function(var)
				if(is.factor(summaryTable[, var])){
					factor(dataTotal[, colVar], levels = levels(summaryTable[, var]))
				}else dataTotal[, colVar]
			)
		}
	}
	summaryTableTotal <- ddply(
		.data = if(!is.null(dataTotal))	dataTotal	else	data, 
		.variables = colVar, 
		.fun = function(x)
			getSummaryStatisticsCustom(
				data = x, type = "countTable", filterEmptyVar = FALSE
			)
	)
	summaryTableTotal$isTotal <- TRUE
	summaryTable$isTotal <- FALSE
	summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	
	# compute percentages
	summaryTable <- ddply(summaryTable, colVar, function(x){
		idxTotal <- which(x$isTotal)
		cbind(x, 
			PercN = x$N/x[idxTotal, "N"]*100,
			Percm = x$m/x[idxTotal, "m"]*100
		)			
	})
	
	# compute specified metrics and extract statistic names
	statsVar <- if(!is.null(stats)){
		
		if(length(stats) > 1 & is.null(names(stats)))
			stop("'statsFct' should be named.")
		statsDf <- sapply(stats, function(expr)
			eval(expr = expr, envir = summaryTable)
		, simplify = FALSE)
		if(is.null(names(statsDf)))	names(statsDf) <- "Statistic"
		
		# save in summaryTable
		summaryTable <- cbind(summaryTable, statsDf, stringsAsFactors = FALSE)
		
		if(is.null(names(statsDf)))	"Statistic"	else	names(statsDf)

	}else	c("N", "m", 
				if(type == "summaryTable") c("Mean", "SD", "SE", "Median", "Min", "Max"), 
				"PercN", "Percm"
			)

	if(".id" %in% colnames(summaryTable))
		summaryTable <- summaryTable[, -which(colnames(summaryTable) == ".id")]
	
	attributes(summaryTable)$statsVar <- statsVar
	
	class(summaryTable) <- c(type, class(summaryTable))
	
	return(summaryTable)
	
}

#' Get summary statistics of interest
#' @param data data.frame with data
#' @param var string, variable of \code{data} with variable to compute statistics on,
#' only used (and required) if \code{type} is 'summaryTable'.
#' Missing values, if present, are filtered.
#' @param subjectVar string, variable of \code{data} with subject ID,
#' 'USUBJID' by default
#' @param filterEmptyVar logical (TRUE by default), should the summary statistics be filtered
#' in case \code{var} is empty.
#' @param type string with type of summary table: 'summaryTable' 
#' (by default) or 'countTable'
#' @return data.frame with summary statistics in columns,
#' depending if \code{type} is:
#' \itemize{
#' \item{'summary': }{
#' \itemize{
#' \item{'N': }{number of subjects }
#' \item{'m': }{number of records}
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
#' \item{'m': }{number of records}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom stats na.omit median sd
#' @export
getSummaryStatistics <- function(data, 
	var = NULL,
	subjectVar = "USUBJID",
	type = "summaryTable",
	filterEmptyVar = TRUE){

	## checks parameters

	type <- match.arg(type, choices = c("summaryTable", "countTable"))
	
	switch(type,
		'summaryTable' = if(is.null(var)){
			stop("Variable of interest should be specified via the 'var' parameter for a summary table.")
		}else if(!is.numeric(data[, var])){
			stop("Variable of interest: 'var' should be numeric in case type is set to 'summaryTable'.")
		},
		'countTable' = if(!is.null(var))
			warning("'var' is not used for count table. ",
				"You might want to specify this variable via the 'rowVar'/'rowVarInSepCol' parameters.")
	)
	
	if(!is.null(var))
		data <- data[!is.na(data[, var]), ]
			
	
	getNSubjects <- function(x)	as.integer(n_distinct(x[, subjectVar]))
	getNRecords <- function(x) nrow(x)

	res <- switch(type,
		'summaryTable' = {
			val <- data[, var]
			if(!(filterEmptyVar & length(val) == 0)){
				data.frame(
					N = getNSubjects(data),
					m = getNRecords(data),
					Mean = ifelse(is.null(var), NA, mean(val)),
					SD = ifelse(is.null(var), NA, sd(val)),
					SE = ifelse(is.null(var), NA, sd(val)/sqrt(length(val))),
					Median = ifelse(is.null(var), NA, median(val)),
					Min = ifelse(is.null(var), NA, min(val)),
					Max = ifelse(is.null(var), NA, max(val))
				)
			}
		},
		'countTable' = data.frame(
			N = getNSubjects(data),
			m = getNRecords(data)
		)
	)
	
	return(res)
	
}
