#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowVarInSepCol Variable(s) of \code{rowVar} which should be 
#' included in separated column in the table.
#' NULL by default. 
#' This is only available if \code{rowVar} if not NULL.
#' @param rowOrder String or function of named list with method used to order the rows,
#' see the \code{method} parameter of \code{\link{convertVarToFactorWithOrder}}.
#' If a string, the same method is used for all \code{rowVar},
#' otherwise the list is named with the \code{rowVar} variable, to
#' specify a different ordering method for each variable.
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @param rowOrderTotalFilterFct Function used to filter the data used to order the rows
#' based on total counts (in case \code{rowOrder} is 'Total').
#' @param stats (optional) Named list of expression or call object of summary statistics of interest.
#' The following variables are recognized, if: 
#' \itemize{
#' \item{\code{type} is a 'summaryTable':}{'N', 'Mean', 'SD', 'SE', 'Median',
#' 'Min', 'Max', 'Perc'}
#' \item{\code{type} is a 'countTable':}{'N','Perc'}
#' }
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.
#' @param varLabGeneral String with general label for variable specified in \code{var}.
#' In case of multiple variable in \code{var}, this will be included in the table header
#' (see 'rowVarLab' attribute of the output).
#' @param varLabSubgroup String with general label for sub-group, in case
#' \code{var} is specified for a count table.
#' This will be included in the table header (see 'rowVarLab' attribute of the output).
#' @param varIgnore Vector with elements to ignore in the \code{var} variable
#' @param dataTotal Data.frame used to extract the Total count, indicated
#' in 'N' in column header and used for the computation of the percentage ('Perc') parameter.
#' It should contain the variables specified by \code{colVar}.
#' @param rowTotalInclude Logical, if TRUE (FALSE by default) include the total
#' counts across rows in a separated row.
#' @param filterFct (optional) Function based on computed statistics of
#' \code{rowVar}/code{colVar} which returns a subset of the summary table of interest.
#' @param colTotalInclude Logical, if TRUE (FALSE by default) include the summary 
#' statistics across columns in a separated column.
#' @inheritParams computeSummaryStatisticsByRowColVar
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
#' The output contains additional the following attributes:
#' \itemize{
#' \item{'statsVar': }{column name(s) of summary table with statistics
#' included in the final table}
#' \item{'rowVar': column name(s) of summary table with row variable
#' included in the final table
#' }
#' \item{'rowVarLab': labels corresponding to the 'rowVar' attribute
#' }
#' }
#' The computed summary statistics are stored in the 'statsVar' attribute.
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill
#' @export
computeSummaryStatisticsTable <- function(data,  
	var = NULL, 
	varLab = getLabelVar(var, data = data, labelVars = labelVars),
	varLabGeneral = "Variable", varLabSubgroup = "Subgroup",
	varIgnore = NULL,
	colVar = NULL,
	colTotalInclude = FALSE,
	rowVar = NULL, 
	rowVarLab = getLabelVar(rowVar,  data = data, labelVars = labelVars),
	rowVarInSepCol = NULL,
	rowOrder = "auto", rowOrderTotalFilterFct = NULL,
	rowTotalInclude = FALSE,
	rowSubtotalInclude = FALSE,
	type = "auto",
	subjectVar = "USUBJID",	
	dataTotal = NULL,
	stats = NULL, filterFct = NULL,
	rowInclude0 = FALSE, colInclude0 = TRUE,
	labelVars = NULL
){
	
	if(nrow(data) == 0)
		stop("The specified dataset is empty.")
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")

	if(colTotalInclude & is.null(colVar)){
		warning("Column 'total' is not included because no column variable is specified.")
		colTotalInclude <- FALSE
	}
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	# get general statistics (by group if specified)
	summaryTable <- computeSummaryStatisticsByRowColVar(
		data = data, 
		var = var, varLab = varLab, type = type,
		rowVar = rowVar, rowInclude0 = rowInclude0,
		colVar = colVar, colInclude0 = colInclude0,
		subjectVar = subjectVar, labelVars = labelVars
	)
	
	if(colTotalInclude){
		
		summaryTableColTotal <- computeSummaryStatisticsByRowColVar(
			data = data, 
			var = var, varLab = varLab, type = type,
			rowVar = rowVar, rowInclude0 = rowInclude0,	
			subjectVar = subjectVar, labelVars = labelVars
		)
		summaryTableColTotal[, colVar] <- "Total"
		
	}
	
	# get total by row (if specified)
	if(rowTotalInclude){
		if(!is.null(rowVar)){
			
			summaryTableRowTotal <- computeSummaryStatisticsByRowColVar(
				data = data, 
				var = var, type = type,
				colVar = colVar, colInclude0 = colInclude0,
				subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
			)
			summaryTableRowTotal[, rowVar] <- "Total"
			
		}else{
			warning("The row 'total' is not included because no 'rowVar' is specified.")
			rowTotalInclude <- FALSE
		}
	}
	
	if(rowSubtotalInclude){
		
		# consider all row variables excepted the last one
		rowVarForSubTotal <- setdiff(rowVar, rowVarInSepCol)
		rowVarSubTotal <- rowVarForSubTotal[-length(rowVarForSubTotal)]
		
		if(length(rowVarSubTotal) < 1){
			
			warning("The row 'sub-total' is not included because only",
				"one or no variable is specified in 'rowVar' (without row variable in separated column)."
			)
			rowSubtotalInclude <- FALSE
			
		}else{
		
			# compute sub-total for each specified rowVar (excepted the last one)
			summaryTableRowSubtotal <- data.frame()
			while(length(rowVarSubTotal) > 0){
				# compute statistics
				summaryTableRowSubtotalVar <- computeSummaryStatisticsByRowColVar(
					data = data, 
					var = var, type = type,
					rowVar = rowVarSubTotal, rowInclude0 = rowInclude0,
					colVar = colVar, colInclude0 = colInclude0,
					subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
				)
				# set other row variables to 'Total'
				summaryTableRowSubtotalVar[, setdiff(rowVarForSubTotal, rowVarSubTotal)] <- "Total"
				# save results
				summaryTableRowSubtotal <- rbind.fill(summaryTableRowSubtotal, summaryTableRowSubtotalVar)
				# consider the next variable
				rowVarSubTotal <- rowVarSubTotal[-length(rowVarSubTotal)]
			}
			
		}
		
	}
	
	# bind the df with row total and subtotal to the summary table
	# ensure that the order of the levels of the variable are retained
	if(rowTotalInclude | rowSubtotalInclude){
		
		rowVarLevels <- sapply(
			summaryTable[, rowVar, drop = FALSE], function(x)
				if(is.factor(x))	levels(x)	else	sort(unique(x)),
			simplify = FALSE
		)
		
		if(rowTotalInclude)
			summaryTable <- rbind.fill(summaryTable, summaryTableRowTotal)
		
		if(rowSubtotalInclude)
			summaryTable <- rbind.fill(summaryTable, summaryTableRowSubtotal)
		
		summaryTable[, rowVar] <- lapply(rowVar, function(x)
			factor(summaryTable[, x], levels = unique(c("Total", rowVarLevels[[x]])))
		)
		
	}
	
	## get counts for the entire dataset
	
	# data considered to compute the total
	if(!is.null(dataTotal)){
		# to have specified order for colVar in case different order 'dataTotal'
		if(!is.null(colVar)){
			dataTotal[, colVar] <- lapply(colVar, function(var)
				if(is.factor(summaryTable[, var])){
					factor(dataTotal[, var], levels = levels(summaryTable[, var]))
				}else dataTotal[, var]
			)
		}
	}else dataTotal <- data
	
	# get counts
	summaryTableTotal <- computeSummaryStatisticsByRowColVar(
		data = dataTotal, 
		type = "countTable", 
		colVar = colVar, colInclude0 = colInclude0,
		subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
	)
	
	## column total
	if(colTotalInclude){
		
		colVarLevels <- sapply(
			summaryTable[, colVar, drop = FALSE], function(x)
				if(is.factor(x))	levels(x)	else	sort(unique(x)),
			simplify = FALSE
		)
		summaryTable <- rbind.fill(summaryTable, summaryTableColTotal)
		summaryTable[, colVar] <- lapply(colVar, function(x)
			factor(summaryTable[, x], levels = unique(c(colVarLevels[[x]], "Total")))
		)
		
		# compute also the total count acros columns
		summaryTableTotalAllCols <- computeSummaryStatisticsByRowColVar(
			data = dataTotal, 
			type = "countTable", 
			colInclude0 = colInclude0,
			subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
		)
		summaryTableTotalAllCols[, colVar] <- "Total"
		summaryTableTotal <- rbind.fill(summaryTableTotal, summaryTableTotalAllCols)
		
	}
	# save total or not in the 'isTotal' column
	summaryTableTotal$isTotal <- TRUE
	summaryTable$isTotal <- FALSE
	# bind to the summary table
	summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	
	# compute percentages
	summaryTable <- ddply(summaryTable, colVar, function(x){
		idxTotal <- which(x$isTotal)
		if(length(idxTotal) > 0){
			PercN <- x$N/x[idxTotal, "N"]*100
			Percm <- x$m/x[idxTotal, "m"]*100
		}else PercN <- Percm <- NA
		cbind(x, PercN = PercN, Percm = Percm)
	})

	# filter records if any 'filterFct' is specified
	if(!is.null(filterFct))
		summaryTable <- filterFct(summaryTable)
	
	# order the row variables as specified
	if(!is.null(rowOrder)){
		summaryTable[, rowVar] <- lapply(rowVar, function(var){
			methodVar <- if(var %in% names(rowOrder))	rowOrder[[var]]	else rowOrder
			convertVarToFactorWithOrder(
				data = summaryTable, var = var, 
				method = methodVar,
				totalFilterFct = rowOrderTotalFilterFct,
				otherVars = setdiff(rowVar, var)
			)
		})
	}
	
	# compute specified metrics and extract statistic names
	if(!is.null(stats)){
		
		if(length(stats) > 1 & is.null(names(stats)))
			stop("'statsFct' should be named.")
		
		# add specified custom statistics in summaryTable
		addStats <- function(sumTable, stats){
			
			statsDf <- sapply(stats, function(expr)
				eval(expr = expr, envir = sumTable)
			, simplify = FALSE)
			if(is.null(names(statsDf)))	names(statsDf) <- "Statistic"
			
			# save in summaryTable
			sumTable <- cbind(sumTable, statsDf, stringsAsFactors = FALSE)
			
			return(sumTable)
			
		}
		
		getStatColName <- function(stats)
			if(is.null(names(stats)))	"Statistic"	else	names(stats)
		
		# if statistics specified for each variable:
		if(length(var) > 1 & any(names(stats) %in% var)){
	
			summaryTable <- ddply(summaryTable, "variable", function(x){
				varI <- unique(x$variableInit)
				if(varI %in% names(stats)){
					addStats(sumTable = x, stats = stats[[varI]])
				}else x
			})
			statsVar <- unname(unique(unlist(lapply(stats, getStatColName))))
			
		}else{
			
			summaryTable <- addStats(sumTable = summaryTable, stats = stats)
			statsVar <- getStatColName(stats)
		
		}

	}else	statsVar <- setdiff(colnames(summaryTable), c(rowVar, colVar, ".id", "variable", "variableGroup", "isTotal"))

	colsToRemove <- which(colnames(summaryTable) %in% c(".id", "variableInit"))
	if(length(colsToRemove) > 0)
		summaryTable <- summaryTable[, -colsToRemove]
	
	attributes(summaryTable)$statsVar <- statsVar
	
	attributes(summaryTable)$rowVar <- c(rowVar, 
		if(length(var) > 1)	c("variable", 
			if("variableGroup" %in% colnames(summaryTable))	"variableGroup"
		)
	)
	attributes(summaryTable)$rowVarLab <- c(rowVarLab, 
		if(length(var) > 1)	c("variable" = varLabGeneral, 
			if("variableGroup" %in% colnames(summaryTable))	c('variableGroup' = varLabSubgroup)
		)
	)
	
#	class(summaryTable) <- c(type, class(summaryTable))
	
	return(summaryTable)
	
}

#' Compute summary statistics by specified \code{rowVar} and \code{colVar}
#' @param rowVar Variable(s) of \code{data} used for
#' grouping in row in the final table.
#' @param rowInclude0 Logical, if TRUE (FALSE) by default,
#' include rows with \code{rowVar} elements not available in the data
#' (e.g. if a \code{rowVar} is a factor, include all levels even if no records in the data).
#' @param colVar Variable(s) of \code{data} used 
#' for grouping in column in the final table. The total 
#' for each subgroup across \code{rowVar} is computed.
#' @param colInclude0 Logical, if TRUE (FALSE) by default,
#' include columns with \code{colVar} elements not available in the data
#' (e.g. if a \code{colVar} is a factor, include all levels even if no records in the data).
#' @param varLab Named character vector with label for each variable 
#' specified in \code{var}.
#' By default, extracted from the \code{labelVars}.
#' @inheritParams computeSummaryStatistics
#' @inheritParams glpgUtilityFct::getLabelVar
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
#' \item{'m': }{number of records}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'N': }{number of subjects}
#' \item{'m': }{number of records}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
computeSummaryStatisticsByRowColVar <- function(
	data, 
	var = NULL, varLab = getLabelVar(var = var, data = data, labelVars = labelVars),
	type = "auto",
	rowVar = NULL, rowInclude0 = FALSE,
	colVar = NULL, colInclude0 = TRUE,
	subjectVar = "USUBJID",
	labelVars = NULL){
	
	if(nrow(data) > 0){

		computeSummaryStatisticsCustom <- function(...)
			computeSummaryStatistics(..., subjectVar = subjectVar)
			
		# build variables used for grouping:
		# 1) consider 'ddply(, .drop = FALSE)' to also include zeros
		# 2) build interaction of row variable(s), column variable(s) to only consider 
		# available combinations of row/column variables (if multiple variables in each direction)
		# otherwise ddply(, .drop = FALSE) with also include combinations of elements non present 
		# e.g. AE term in an AE group not present
		# (variables considered independently)
		groupVar <- c(
			if(!is.null(rowVar)){
				if(!rowInclude0){
					data$rowVariables <- interaction(data[, rowVar], drop = TRUE)
					"rowVariables"
				}else	rowVar
			},
			if(!is.null(colVar)){
				if(!colInclude0){
					data$colVariables <- interaction(data[, colVar], drop = TRUE)
					"colVariables"
				}else colVar	
			}
		)
		
		# get general statistics (by group if specified)
		summaryTable <- ddply(data, groupVar, function(x){
			# compute statistics for each specified variable
			# (loop in case multiple are specified)
			if(is.null(var)){
				sumTable <- computeSummaryStatisticsCustom(
					data = x, 
					var = var, 
					type = type
				)
			}else{
				summaryTableVarList <- lapply(var, function(varI){
					sumTable <- computeSummaryStatisticsCustom(
						data = x, 
						var = varI, 
						type = type
					)
					# only store the variable if more than one specified variable
					if(!is.null(sumTable) && length(var) > 1){
						cbind.data.frame(variableInit = varI, sumTable, stringsAsFactors = FALSE)
					}else sumTable
				})
				summaryTable <- do.call(rbind.fill, summaryTableVarList)
				# if multiple variable(s), sort 'variable' in order specified in input
				if(length(var) > 1){
					summaryTable$variable <- factor(
						varLab[summaryTable$variableInit],
						levels = varLab[var]
					)
				}
				summaryTable
			}
		}, .drop = FALSE)

		if(is.null(groupVar))
			summaryTable[, ".id"] <- NULL
	
		# include original rowVar/colVar
		if(!is.null(rowVar) & !rowInclude0){
			summaryTable[, rowVar] <- data[match(summaryTable$rowVariables, data$rowVariables), rowVar]
			summaryTable$rowVariables <- NULL
		}
		if(!is.null(colVar) & !colInclude0){
			summaryTable[, colVar] <- data[match(summaryTable$colVariables, data$colVariables), colVar]
			summaryTable$colVariables <- NULL
		}
		
	}else summaryTable <- NULL
		
	return(summaryTable)
	
}

#' Get summary statistics of interest of an unique variable of interest.
#' @param data Data.frame with data.
#' @param var String, variable of \code{data} with variable to compute statistics on.
#' Missing values, if present, are filtered.
#' @param subjectVar String, variable of \code{data} with subject ID,
#' 'USUBJID' by default.
#' @param type String with type of table: 
#' \itemize{
#' \item{'summaryTable': }{summary table with custom statistics}
#' \item{'countTable': }{count table}
#' \item{'auto' (by default): }{'summaryTable' of \code{var} is numeric,
#' 'countTable' otherwise
#' }
#' }
#' @param filterEmptyVar Logical, if TRUE doesn't return any results
#' if the variable is empty, otherwise return 0 for the counts and NA for summary 
#' statistics (by default, only TRUE if the final table is 'summaryTable')
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
	filterEmptyVar = ((type == "auto" && is.numeric(data[, var])) | type == "summaryTable"),
	type = "auto"){

	## checks parameters

	type <- match.arg(type, choices = c("auto", "summaryTable", "countTable"))
	
	if(type == "auto")
		type <- ifelse(is.numeric(data[, var]), "summaryTable", "countTable")
	
	if(type == "summaryTable"){
		if(is.null(var)){
			stop("Variable of interest should be specified via the 'var' parameter for a summary table.")
		}else if(!is.numeric(data[, var])){
			stop("Variable of interest: 'var' should be numeric in case type is set to 'summaryTable'.")
		}
	}
	
	if(!is.null(var))
		data <- data[!is.na(data[, var]), ]
	
	getNSubjects <- function(x)	as.integer(n_distinct(x[, subjectVar]))
	getNRecords <- function(x) nrow(x)

	switch(type,
		'summaryTable' = {
			val <- data[, var]
			emptyVar <- is.null(val) || length(val) == 0
			res <- if(!(filterEmptyVar & emptyVar)){
				data.frame(
					N = getNSubjects(data),
					m = getNRecords(data),
					Mean = ifelse(emptyVar, NA, mean(val)),
					SD = ifelse(emptyVar, NA, sd(val)),
					SE = ifelse(emptyVar, NA, sd(val)/sqrt(length(val))),
					Median = ifelse(emptyVar, NA, median(val)),
					Min = ifelse(emptyVar, NA, min(val)),
					Max = ifelse(emptyVar, NA, max(val))
				)
			}
		},
		'countTable' = {
			# to avoid that ddply with empty data returns entire data.frame
			if(nrow(data) == 0){
				res <- data.frame(N = 0, m = 0)
			}else{
				res <- ddply(data, var, function(x){
					if(!(filterEmptyVar & nrow(x) == 0)){
						data.frame(
							N = getNSubjects(x),
							m = getNRecords(x)
						)
					}
				})
				if(is.null(var)){
					res[, ".id"] <- NULL
				}else{
					colnames(res)[match(var, colnames(res))] <- "variableGroup"
				}
			}
		}
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
#' @param totalFilterFct (optional) Function which returns a subset of the data of interest,
#' to filter the total data considered for the ordering.
#' @return Factor \code{var} variable of \code{data} with specified order.
#' @importFrom plyr daply
#' @author Laure Cougnaud
convertVarToFactorWithOrder <- function(
	data, var, otherVars = NULL, 
	method = c("auto", "alphabetical", "total"),
	totalFilterFct = NULL,
	totalVar = "N"){

	if(!is.function(method)){
		
		method <- match.arg(method)
	
		res <- switch(method,
			'auto' = if(is.factor(data[, var]))
				data[, var]	else	
				convertVarToFactorWithOrder(data = data, var = var, method = "alphabetical"),
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
					convertVarToFactorWithOrder(data = data, var = var, method = "auto")
				}else{
					
					# remove total for this variable
					dataForTotal <- data[which(data[, var] != "Total"), ]
					
					# filter records if any 'filterFct' is specified
					if(!is.null(totalFilterFct))
						dataForTotal <- totalFilterFct(dataForTotal)
					
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

