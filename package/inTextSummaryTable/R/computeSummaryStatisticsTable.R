#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowVar Variable(s) of \code{data} used for
#' grouping in row in the final table.
#' @param rowOrder String or function of named list with method used to order the rows,
#' see the \code{method} parameter of \code{\link{convertVarToFactorWithOrder}}.
#' If a string, the same method is used for all \code{rowVar},
#' otherwise the list is named with the \code{rowVar} variable, to
#' specify a different ordering method for each variable.
#' @param colVar Variable(s) of \code{data} used 
#' for grouping in column in the final table. The total 
#' for each subgroup across \code{rowVar} is computed.
#' @param stats (optional) Named list of expression of summary statistics of interest.
#' The following statistics are recognized, if: 
#' \itemize{
#' \item{\code{type} is a 'summaryTable':}{'N', 'Mean', 'SD', 'SE', 'Median',
#' 'Min', 'Max', 'Perc'}
#' \item{\code{type} is a 'countTable':}{'N','Perc'}
#' }
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.
#' @param varIgnore Vector with elements to ignore in the \code{var} variable
#' @param dataTotal Data.frame used to extract the Total count, indicated
#' in 'N' in column header and used for the computation of the percentage ('Perc') parameter.
#' It should contain the variables specified by \code{colVar}.
#' @param rowTotalInclude Logical, if TRUE (FALSE by default) include the total
#' across rows in a separated row.
#' @param filterFct (optional) Function based on computed statistics of
#' \code{rowVar}/code{colVar} which returns a subset of the summary table of interest.
#' @inheritParams computeSummaryStatistics
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
computeSummaryStatisticsTable <- function(data,  
	var = NULL, varIgnore = NULL,
	colVar = NULL,
	rowVar = NULL,
	rowOrder = "auto",
	rowTotalInclude = FALSE,
	rowSubtotalInclude = FALSE,
	type = "summaryTable",
	subjectVar = "USUBJID",	
	dataTotal = NULL,
	stats = NULL, filterFct = NULL
){
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	computeSummaryStatisticsCustom <- function(...)
		computeSummaryStatistics(..., subjectVar = subjectVar)
	
	# get general statistics (by group if specified)
	summaryTable <- ddply(data, c(rowVar, colVar), function(x){
		computeSummaryStatisticsCustom(
			data = x, var = var, type = type,
			filterEmptyVar = (type == "summaryTable")
		)
	}, .drop = FALSE)

	if(rowTotalInclude){
		if(!is.null(rowVar)){
			summaryTableTotalData <- ddply(
				.data = data, 
				.variables = colVar, 
				.fun = function(x)
					computeSummaryStatisticsCustom(
						data = x, type = type, 
						filterEmptyVar = (type == "summaryTable"),
						var = var
					)
			, .drop = FALSE)
			summaryTableTotalData[, rowVar] <- "Total"
		}else{
			warning("The row 'total' is not included because no 'rowVar' is specified.")
			rowTotalInclude <- FALSE
		}
	}
	
	if(rowSubtotalInclude){
		
		if(length(rowVar) <= 1){
			warning("The row 'sub-total' is not included because only",
				"one or no variable is specified in 'rowVar'."
			)
			rowSubtotalInclude <- FALSE
		}else{
			
			rowVarSubTotal <- rowVar[-length(rowVar)]
			summaryTableSubtotalData <- ddply(
				.data = data, 
				.variables = c(colVar, rowVarSubTotal),
				.fun = function(x)
					computeSummaryStatisticsCustom(
						data = x, type = type, 
						filterEmptyVar = (type == "summaryTable"),
						var = var
				),
			.drop = FALSE)
			summaryTableSubtotalData[, rowVar[length(rowVar)]] <- "Total"
		}
		
	}
	
	if(rowTotalInclude | rowSubtotalInclude){
		
		rowVarLevels <- sapply(
			summaryTable[, rowVar, drop = FALSE], function(x)
				if(is.factor(x))	levels(x)	else	sort(unique(x)),
			simplify = FALSE
		)
		
		if(rowTotalInclude)
			summaryTable <- rbind.fill(summaryTable, summaryTableTotalData)
		
		if(rowSubtotalInclude)
			summaryTable <- rbind.fill(summaryTable, summaryTableSubtotalData)
		
		summaryTable[, rowVar] <- lapply(rowVar, function(x)
			factor(summaryTable[, x], levels = unique(c("Total", rowVarLevels[[x]])))
		)
		
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
			computeSummaryStatisticsCustom(
				data = x, type = "countTable", filterEmptyVar = FALSE
			)
	, .drop = FALSE)
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
	}, .drop = FALSE)

	if(!is.null(filterFct))
		summaryTable <- filterFct(summaryTable)
	
	if(!is.null(rowOrder)){
		summaryTable[, rowVar] <- lapply(rowVar, function(var){
			methodVar <- if(var %in% names(rowOrder))	rowOrder[[var]]	else rowOrder
			convertVarToFactorWithOrder(
				data = summaryTable, var = var, 
				method = methodVar, 
				otherVars = setdiff(rowVar, var)
			)
		})
	}
	
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

#' Get summary statistics of interest of an unique variable of interest.
#' @param data Data.frame with data.
#' @param var String, variable of \code{data} with variable to compute statistics on,
#' only used (and required) if \code{type} is 'summaryTable'.
#' Missing values, if present, are filtered.
#' @param subjectVar String, variable of \code{data} with subject ID,
#' 'USUBJID' by default.
#' @param filterEmptyVar Logical (TRUE by default), should the summary statistics be filtered
#' in case \code{var} is empty.
#' @param type String with type of summary table: 'summaryTable' 
#' (by default) or 'countTable'.
#' @return Data.frame with summary statistics in columns,
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
computeSummaryStatistics <- function(data, 
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
				"You might want to specify this variable via the",
				"'rowVar'/'rowVarInSepCol' parameters of the 'computeSummaryStatisticsTable' function.")
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

#' Convert a variable to a factor with levels in specified order.
#' @param data Data.frame with data.
#' @param var String with variable of \code{data} to sort.
#' @param otherVars Character vector with other variable(s) of \code{data}
#' considered in the specific dimension.
#' @param method Ordering method to use, either:
#' \itemize{
#' \item{String among:}{
#' \itemize{
#' \item{'auto': }{if \code{var} is a factor, keep its order, otherwise alphabetically}
#' \item{'alphabetical': }{\code{var} is order in alphabetical order}
#' \item{'total': }{\code{var} is ordered based on the \code{totalVar} variable, in decreasing order.
#' The total count is extracted from the rows with all \code{otherVars} equal to 'Total'.
#' If none, consider all rows.}
#' }
#' }
#' \item{Function to be applied on each subset to get the order elements of the variable}
#' }
#' @param totalVar String with variable of \code{data} considered in case \code{type} is 'total'.
#' @return Factor \code{var} variable of \code{data} with specified order.
#' @importFrom plyr daply
#' @author Laure Cougnaud
convertVarToFactorWithOrder <- function(
	data, var, otherVars = NULL, 
	method = c("auto", "alphabetical", "total"),
	totalVar = "N"){

	if(!is.function(method)){
		
		method <- match.arg(method)
	
		res <- switch(method,
			'auto' = if(is.factor(data[, var]))
				data[, var]	else	sortVar(data = data, var = var, method = "alphabetical"),
			'alphabetical' = {
				varLevels <- c(
					if("Total" %in% data[, var])	"Total", 
					sort(setdiff(unique(data[, var]), "Total"), decreasing = TRUE)
				)
				factor(data[, var])
			},
			'total' = {
				if(is.null(otherVars)){
					warning("The variable: ", var, "cannot be sorted based on total count ",
							"because no total count is available. You might want to set: ",
							"'rowSubtotalInclude' to TRUE."
					)
					sortVar(data = data, var = var, method = "auto")
				}else{
					# remove total for this variable
					dataForTotal <- data[which(data[, var] != "Total"), ]
					if(!is.null(otherVars)){
						# consider rows with subtotal for this variable (if any)
						idxRowTotal <- which(rowSums(dataForTotal[, otherVars, drop = FALSE] == "Total") == length(otherVars))
						if(length(idxRowTotal) > 0)
							dataForTotal <- dataForTotal[idxRowTotal, ]
						totalPerVar <- daply(dataForTotal, var, function(x) sum(x[, totalVar], na.rm = TRUE))
					}
					varLevels <- c(
						if("Total" %in% data[, var])	"Total", 
						setdiff(names(sort(totalPerVar, decreasing = TRUE)), "Total")
					)
					varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
					factor(data[, var], levels = varLevels)
				}
			}
		)
		
	}else{
		
		varLevels <- method(data)
		res <- factor(data[, var], levels = varLevels)
		
	}
	
	return(res)
		
}

