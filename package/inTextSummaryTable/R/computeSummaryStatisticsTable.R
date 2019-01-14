#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowVarInSepCol Variable(s) of \code{rowVar} which should be 
#' included in separated column in the table.
#' NULL by default. 
#' This is only available if \code{rowVar} if not NULL.
#' @param rowOrder String or function of named list with method used to order the rows,
#' see the \code{method} parameter of \code{\link{convertVarToFactorWithOrder}}.
#' 'total' is only available if the total is available for each \code{rowVar} (\code{rowSubtotalInclude} set to TRUE)
#' If a string, the same method is used for all \code{rowVar},
#' otherwise the list is named with the \code{rowVar} variable, to
#' specify a different ordering method for each variable.
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @param rowOrderTotalFilterFct Function used to filter the data used to order the rows
#' based on total counts (in case \code{rowOrder} is 'total').
#' @param rowSubtotalInclude Logical, if TRUE (FALSE by default) include also the total for each
#' group of the nested variable specified in \code{rowVar}
#' @param stats (optional) Named list of expression or call object of summary statistics of interest.
#' The following variables are recognized, if the table is a: 
#' \itemize{
#' \item{'summaryTable': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statMean': }{mean of \code{var}}
#' \item{'statSD': }{standard deviation of \code{var}}
#' \item{'statSE': }{standard error of \code{var}}
#' \item{'statMedian': }{median of \code{var}}
#' \item{'statMin': }{minimum of \code{var}}
#' \item{'statMax': }{maximum of \code{var}}
#' \item{'statPerc': }{percentage of subjects}
#' \item{'statm': }{number of records}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statPercN': }{percentage of subjects}
#' \item{'statm': }{number of records}
#' }
#' }
#' }
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.
#' The statistics can be specified for each \code{var} (if multiple),
#' by specifying a nested list of such parameters named by the variable.
#' @param varLabGeneral String with general label for variable specified in \code{var}.
#' In case of multiple variable in \code{var}, this will be included in the table header
#' (see 'rowVarLab' attribute of the output).
#' @param varLabSubgroup String with general label for sub-group, in case
#' \code{var} is specified for a count table.
#' This will be included in the table header (see 'rowVarLab' attribute of the output).
#' Empty by default.
#' @param varIgnore Vector with elements to ignore in the \code{var} variable
#' @param dataTotal Data.frame used to extract the Total count, indicated
#' in 'N' in column header.
#' It should contain the variables specified by \code{colVarTotal}.
#' @param dataTotalPerc Data.frame used to extract the Total count, 
#' used for the computation of the percentage: 'statPercN' parameter.
#' It should contain the variables specified by \code{colVarTotalPerc}.
#' @param rowTotalInclude Logical, if TRUE (FALSE by default) include the total
#' counts across rows in a separated row.
#' @param filterFct (optional) Function based on computed statistics of
#' \code{rowVar}/code{colVar} which returns a subset of the summary table of interest.
#' Note: the total count per category of the row variables is included
#' in a row with this variable set to 'Total'.
#' @param colVarTotal String with column(s) considered to compute the total by,
#' reported in the header of the table, by default same as \code{colVar}.
#' @param colVarTotalPerc String with column(s) considered to compute the total by,
#' used as denominator for the percentage computation, by default same as \code{colVarTotal}.
#' @param byVar Variable(s) of \code{data} for which separated table(s)
#' should be created.
#' @param byVarLab String with label for \code{byVar}, used to set the names
#' of the output list of table(s).
#' @inheritParams computeSummaryStatisticsTableTotal
#' @inheritParams computeSummaryStatisticsByRowColVar
#' @return data.frame of class 'countTable' or 'summaryTable',
#' depending on the 'type' parameter; or list of such data.frame if
#' \code{byVar} is specified.
#' The data.frame contains the :
#' \itemize{
#' \item{row and column variables}
#' \item{computed statistics: }{if \code{type} is:
#' \itemize{
#' \item{'summaryTable': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statMean': }{mean of \code{var}}
#' \item{'statSD': }{standard deviation of \code{var}}
#' \item{'statSE': }{standard error of \code{var}}
#' \item{'statMedian': }{median of \code{var}}
#' \item{'statMin': }{minimum of \code{var}}
#' \item{'statMax': }{maximum of \code{var}}
#' \item{'statPerc': }{percentage of subjects}
#' \item{'statm': }{number of records}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statPercN': }{percentage of subjects}
#' \item{'statm': }{number of records}
#' }}}
#' }
#' \item{statistics specified by \code{statsVar}.
#' (if not named and of length 1 in a column 'Statistic')}
#' \item{variables: }{
#' \itemize{
#' \item{'variable': }{variable name in case \code{var} is of length > 1}
#' \item{'variableGroup': }{in case \code{var} is of length > 1 and for variable(s) used for count:
#' elements of the variable}
#' }
#' }
#' \item{'isTotal': }{variable with logical flag, TRUE if the record contain the total by column}
#' }
#' The output contains additional the following attributes:
#' \itemize{
#' \item{'statsVar': }{column name(s) of summary table with statistics
#' included in the final table}
#' \item{'rowVar': }{column name(s) of summary table with row variable
#' included in the final table}
#' \item{'rowVarLab': }{labels corresponding to the 'rowVar' attribute}
#' \item{'rowSubtotalInclude': }{\code{rowSubtotalInclude} checked}
#' }
#' The computed summary statistics are stored in the 'statsVar' attribute.
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill dlply
#' @export
computeSummaryStatisticsTable <- function(
	data,  
	var = NULL, 
	varLab = getLabelVar(var, data = data, labelVars = labelVars),
	varLabGeneral = "Variable", varLabSubgroup = NULL,
	varIgnore = NULL,
	varIncludeTotal = FALSE,
	colVar = NULL, colVarDataLevels = NULL, 
	colVarTotal = colVar, 
	colVarTotalPerc = colVarTotal,
	colTotalInclude = FALSE, colTotalLab = "Total",
	rowVar = NULL, rowVarDataLevels = NULL, 
	rowVarLab = getLabelVar(rowVar,  data = data, labelVars = labelVars),
	rowVarInSepCol = NULL,
	rowOrder = "auto", rowOrderTotalFilterFct = NULL,
	rowTotalInclude = FALSE,
	rowSubtotalInclude = FALSE,
	rowSubtotalInSepRow = FALSE,
	type = "auto",
	subjectVar = "USUBJID",	
	dataTotal = NULL, dataTotalPerc = dataTotal,
	stats = NULL, statsExtra = NULL,
	filterFct = NULL,
	rowInclude0 = FALSE, colInclude0 = FALSE,
	labelVars = NULL,
	byVar = NULL, byVarLab = getLabelVar(byVar, data = data, labelVars = labelVars)
){
	
	inputParams <- as.list(environment())
	
	if(nrow(data) == 0)
		stop("The specified dataset is empty.")
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")

	if(!is.null(byVar)){
		if(!byVar %in% colnames(data)){
			warning("'byVar' is not available in the 'data' so is not used.")
			byVar <- FALSE
		}else{
			res <- dlply(data, byVar, function(dataBy){
				inputParamsBy <- inputParams
				inputParamsBy$data <- dataBy
				inputParamsBy$byVar <- NULL
				do.call(computeSummaryStatisticsTable, inputParamsBy)		
			})	
			if(!is.null(byVarLab))
				names(res) <- paste(byVarLab, names(res), sep = ": ")
			return(res)
		}
	}

	if(colTotalInclude & is.null(colVar)){
		warning("Column 'total' is not included because no column variable is specified.")
		colTotalInclude <- FALSE
	}
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		data <- data[!data[, var] %in% varIgnore, ]
	
	# convert row and column variable to factor in the data
	# (if character, variables pre-defined as factor in one summary tables will be lost during rbind.fill)
	if(!is.null(rowVar))
		data[, rowVar] <- lapply(data[, rowVar, drop = FALSE], function(x){
			factor(x, levels = if(is.factor(x))	levels(x)	else	sort(unique(x)))
		})
	if(!is.null(colVar))
		data[, colVar] <- lapply(data[, colVar, drop = FALSE], function(x){
			factor(x, levels = if(is.factor(x))	levels(x)	else	sort(unique(x)))
		})
		
	# get general statistics (by group if specified)
	summaryTable <- computeSummaryStatisticsByRowColVar(
		data = data, 
		var = var, varLab = varLab, varIncludeTotal = varIncludeTotal,
		statsExtra = statsExtra,
		type = type,
		rowVar = rowVar, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
		colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar, labelVars = labelVars
	)
	# original levels of colVar in summaryTable
	colVarLevels <- sapply(
		summaryTable[, colVar, drop = FALSE], function(x)
			if(is.factor(x))	levels(x)	else	sort(unique(x)),
		simplify = FALSE
	)
	
	if(colTotalInclude){
		
		summaryTableColTotal <- computeSummaryStatisticsByRowColVar(
			data = data, 
			var = var, varLab = varLab, varIncludeTotal = varIncludeTotal, 
			statsExtra = statsExtra,
			type = type,
			rowVar = rowVar, rowInclude0 = rowInclude0,	rowVarDataLevels = rowVarDataLevels,
			subjectVar = subjectVar, labelVars = labelVars
		)
		if(nrow(summaryTableColTotal) > 0)
			summaryTableColTotal[, colVar] <- colTotalLab
		
	}
	
	# get total by row (if specified)
	if(rowTotalInclude){
		if(!is.null(rowVar)){
			
			summaryTableRowTotal <- computeSummaryStatisticsByRowColVar(
				data = data, 
				var = var, varIncludeTotal = varIncludeTotal, statsExtra = statsExtra,
				type = type,
				colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
				subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
			)
			
			# include also the total per column (if required)
			if(colTotalInclude){
				summaryTableRowTotalColTotal <- computeSummaryStatisticsByRowColVar(
					data = data, 
					var = var, varIncludeTotal = varIncludeTotal, statsExtra = statsExtra,
					varLab = varLab, type = type,	
					subjectVar = subjectVar, labelVars = labelVars
				)
				if(nrow(summaryTableRowTotalColTotal) > 0)
					summaryTableRowTotalColTotal[, colVar] <- colTotalLab
				summaryTableRowTotal <- rbind.fill(summaryTableRowTotal, summaryTableRowTotalColTotal)
			}
			
			if(nrow(summaryTableRowTotal) > 0)
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
			
			warning("The row 'sub-total' is not included because only ",
				"one or no variable is specified in 'rowVar' (without row variable in separated column)."
			)
			rowSubtotalInclude <- FALSE
			
		}else{
		
			# compute sub-total for each specified rowVar (excepted the last one)
			summaryTableRowSubtotal <- data.frame()
			rVST <- rowVarSubTotal
			while(length(rVST) > 0){
				
				# remove rows which have NA for the nested sub-variable
				# otherwise have summary statistics are duplicated (sub-total and initial)
				rowVarSubTotalOther <- rowVarForSubTotal[
					setdiff(seq_along(rowVarForSubTotal), seq_along(match(rVST, rowVarForSubTotal)))
				]
				idxMissingSubVar <- which(
					rowSums(is.na(data[, rowVarSubTotalOther, drop = FALSE])) == length(rowVarSubTotalOther)
				)
				dataForSubTotal <- if(length(rowVarSubTotalOther) > 0 && length(idxMissingSubVar) > 0){
					data[-idxMissingSubVar, ]
				}else	data
				
				# compute statistics
				summaryTableRowSubtotalVar <- computeSummaryStatisticsByRowColVar(
					data = dataForSubTotal, 
					var = var, varIncludeTotal = varIncludeTotal,
					statsExtra = statsExtra,
					type = type,
					rowVar = rVST, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
					colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
					subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
				)
				
				# include also the total per column (if required)
				if(colTotalInclude){
					summaryTableRowSubtotalVarColTotal <- computeSummaryStatisticsByRowColVar(
						data = dataForSubTotal, 
						var = var, varIncludeTotal = varIncludeTotal,
						statsExtra = statsExtra,
						type = type,
						rowVar = rVST, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
						subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
					)
					if(nrow(summaryTableRowSubtotalVarColTotal) > 0)
						summaryTableRowSubtotalVarColTotal[, colVar] <- colTotalLab
					summaryTableRowSubtotalVar <- rbind.fill(summaryTableRowSubtotalVar, summaryTableRowSubtotalVarColTotal)
					summaryTableRowSubtotalVar[, colVar] <- lapply(colVar, function(x)
						factor(summaryTableRowSubtotalVar[, x], levels = unique(c(colVarLevels[[x]], colTotalLab)))
					)
				
				}
				
				# set other row variables to 'Total'
				if(nrow(summaryTableRowSubtotalVar) > 0)
					summaryTableRowSubtotalVar[, setdiff(rowVarForSubTotal, rVST)] <- "Total"
				
				# save results
				summaryTableRowSubtotal <- rbind.fill(summaryTableRowSubtotal, summaryTableRowSubtotalVar)
				# consider the next variable
				rVST <- rVST[-length(rVST)]
				
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
		
		summaryTable[, rowVar] <- lapply(rowVar, function(var){
			xVar <- summaryTable[, var]
			# only add Total if included in the table (case: missing nested var)
			levelsX <- unique(c(if(rowSubtotalInclude && (var %in% rowVarSubTotal | rowSubtotalInSepRow))	"Total", rowVarLevels[[var]]))
			factor(xVar, levels = levelsX)
		})
		
	}
	
	## get counts for the entire dataset
	
	# data considered to compute the total
	if(!is.null(dataTotal)){
		# to have specified order for colVar in case different order 'dataTotal'
		if(!is.null(colVarTotal)){
			dataTotal[, colVarTotal] <- lapply(colVarTotal, function(var)
				if(is.factor(summaryTable[, var])){
					factor(dataTotal[, var], levels = levels(summaryTable[, var]))
				}else dataTotal[, var]
			)
		}
	}else dataTotal <- data
	
	## column total
	if(colTotalInclude){
		
		# total summary table
		summaryTableTotalCol <- computeSummaryStatisticsByRowColVar(
			data = dataTotal, 
			type = "countTable", 
			subjectVar = subjectVar, labelVars = labelVars
		)
		if(nrow(summaryTableTotalCol) > 0)
			summaryTableTotalCol[, colVar] <- colTotalLab
		
		summaryTable <- rbind.fill(summaryTable, summaryTableColTotal)
		summaryTable[, colVar] <- lapply(colVar, function(x)
			factor(summaryTable[, x], levels = unique(c(colVarLevels[[x]], colTotalLab)))
		)
		
	}
	
	# get counts (for column header total)
	summaryTableTotal <- computeSummaryStatisticsTableTotal(
		data = dataTotal, 
		colVar = colVar, colVarTotal = colVarTotal,
		colTotalLab = colTotalLab,
		colInclude0 = colInclude0, colTotalInclude = colTotalInclude,
		colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
		subjectVar = subjectVar
	)
	
	# save total or not in the 'isTotal' column
	summaryTableTotal$isTotal <- TRUE
	summaryTable$isTotal <- FALSE
	# bind to the summary table
	summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	
	## compute percentages
	
	# get counts (for percentage computation)
	if(is.null(dataTotalPerc))	dataTotalPerc <- dataTotal
	if(!all(colVarTotalPerc %in% colnames(summaryTable)))
		stop("'colVarTotalPerc' are not in the computed summary statistics table.")
	summaryTableTotalPerc <- if(!setequal(colVarTotal, colVarTotalPerc)){
		computeSummaryStatisticsTableTotal(
			data = dataTotalPerc, 
			colVar = colVar, colVarTotal = colVarTotalPerc,
			colTotalLab = colTotalLab,
			colInclude0 = colInclude0, colTotalInclude = colTotalInclude,
			colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
			subjectVar = subjectVar
		)
	}else	summaryTableTotal
	summaryTable <- rbind.fill(
		cbind(summaryTable, isTotalPerc = FALSE), 
		cbind(summaryTableTotalPerc, isTotalPerc = TRUE)
	)
	
	# compute percentages
	summaryTable <- ddply(summaryTable, colVarTotalPerc, function(x){
		idxTotalPerc <- which(x$isTotalPerc)
		if(length(idxTotalPerc) > 0){
			res <- cbind(x, statPercN = x$statN/x[idxTotalPerc, "statN"]*100)
			res[-idxTotalPerc, ]
		}else cbind(x, statPercN = NA)
	})
	summaryTable$isTotalPerc <- NULL

	# filter records if any 'filterFct' is specified
	if(!is.null(filterFct))
		summaryTable <- filterFct(summaryTable)
	
	# order the row variables as specified
	if(!is.null(rowVar) && !is.null(rowOrder)){
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
	
	# extract statistic names
	statsVarInit <- setdiff(
		colnames(summaryTable), 
		c(rowVar, colVar, ".id", "variable", "variableGroup", "isTotal", "variableInit")
	)
	
	# compute specified metrics
	if(!is.null(stats)){
		
		if(length(stats) > 1 & is.null(names(stats)))
			stop("'statsFct' should be named.")
		
		# add specified custom statistics in summaryTable
		addStats <- function(sumTable, stats){
			
			# check if stats has the same name than default name
			statsName <- names(stats)
			if(!is.null(statsName)){
				statsNameConflict <- intersect(statsName, statsVarInit)
				if(length(statsNameConflict) > 0){
					for(stat in statsNameConflict){
						if(as.character(stats[[stat]]) == stat){
							stats <- stats[names(stats) != stat]
						}else{
							stop("The statistic name: '", stat, "'",
								" is a default name used, please choose a different name.")
						}
					}
				}
			}
			
			statsDf <- sapply(stats, function(expr)
				eval(expr = expr, envir = sumTable)
			, simplify = FALSE)
			if(is.null(statsName))	names(statsDf) <- "Statistic"
			
			# save in summaryTable
			sumTable <- cbind(sumTable, statsDf, stringsAsFactors = FALSE)
			
			return(sumTable)
			
		}
		
		getStatColName <- function(stats)
			if(is.null(names(stats)))	"Statistic"	else	names(stats)
		
		# if statistics specified for each variable:
		if(any(names(stats) %in% var)){
	
			statsVar <- unname(unique(unlist(lapply(stats, getStatColName))))
			summaryTable <- ddply(summaryTable, "variable", function(x){
				varI <- unique(x$variableInit)
				if(varI %in% names(stats)){
					addStats(sumTable = x, stats = stats[[varI]])
				}else x
			})
			
			
		}else{
			
			statsVar <- getStatColName(stats)
			summaryTable <- addStats(sumTable = summaryTable, stats = stats)
		
		}

	}else	statsVar <- statsVarInit

	colsToRemove <- which(colnames(summaryTable) %in% c(".id", "variableInit"))
	if(length(colsToRemove) > 0)
		summaryTable <- summaryTable[, -colsToRemove]
	
	# table attributes
	attrTable <- list(
			
		# attributes created from this function
		statsVar = statsVar,
		rowSubtotalInclude = rowSubtotalInclude,
		rowSubtotalInSepRow = rowSubtotalInSepRow,
		rowVar = c(rowVar, 
			if(length(var) > 1)	"variable", 
			# in case only one variable, but still count
			if("variableGroup" %in% colnames(summaryTable))	"variableGroup"
		),
		rowVarLab = c(rowVarLab, 
			if(length(var) > 1)	c("variable" = varLabGeneral, 
				if("variableGroup" %in% colnames(summaryTable))	c('variableGroup' = varLabSubgroup)
			)
		),

		# attributes extracted from the input parameters,
		# to set similar defaults for the exportSummaryStatisticsTable
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude,
		colVar = colVar		

	)
	
	attributes(summaryTable) <- c(attributes(summaryTable), list(summaryTable = attrTable))
	
	return(summaryTable)
	
}

#' Compute summary statistics total table.
#' @param colVarTotal column considered to compute the total.
#' @param colTotalLab String, label for the total column included 
#' if \code{colTotalInclude} is TRUE, 'Total' by default.
#' @param colVarLevels list with levels of each \code{colVar}
#' @param colTotalInclude Logical, if TRUE (FALSE by default) include the summary 
#' statistics across columns in a separated column.
#' @inheritParams computeSummaryStatisticsByRowColVar
#' @return data.frame with total table.
#' @author Laure Cougnaud
computeSummaryStatisticsTableTotal <- function(
	data, 
	colVar = NULL, colVarTotal = colVar,
	colTotalLab = "Total",
	colInclude0 = FALSE,
	colTotalInclude = FALSE,
	colVarDataLevels = NULL, colVarLevels = NULL,
	subjectVar = "USUBJID"){
	
	# counts by elements in colVar
	summaryTableTotal <- computeSummaryStatisticsByRowColVar(
		data = data, 
		type = "countTable", 
		colVar = colVarTotal, 
		colInclude0 = colInclude0, 
		colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar
	)
	
	# counts across all elements of colVar
	if(colTotalInclude){
		colVarTotalTI <- setdiff(colVarTotal, colVar)
		if(length(colVarTotalTI) == 0)	colVarTotalTI <- NULL
		summaryTableTotalCol <- computeSummaryStatisticsByRowColVar(
			data = data, 
			type = "countTable", 
			colVar = colVarTotalTI,
			subjectVar = subjectVar
		)
		if(nrow(summaryTableTotalCol) > 0)
			summaryTableTotalCol[, colVar] <- colTotalLab
		summaryTableTotal <- rbind.fill(summaryTableTotal, summaryTableTotalCol)
		if(!is.null(colVarLevels))
			summaryTableTotal[, colVar] <- lapply(colVar, function(x)
				factor(summaryTableTotal[, x], levels = unique(c(colVarLevels[[x]], colTotalLab)))
			)
	}
	
	return(summaryTableTotal)
	
}

#' Compute summary statistics by specified \code{rowVar} and \code{colVar}
#' @param rowVar Variable(s) of \code{data} used for
#' grouping in row in the final table.
#' @param rowInclude0 Logical, if TRUE (FALSE by default),
#' include rows with no records, based on all combinations 
#' of the \code{rowVar} (assuming nested variable(s)).
#' @param colVar Variable(s) of \code{data} used 
#' for grouping in column in the final table. The total 
#' for each subgroup across \code{rowVar} is computed.
#' If variable(s) are not nested, possible combinations
#' can be specified via \code{rowVarDataLevels}.
#' @param colInclude0 Logical, if TRUE (FALSE by default),
#' include columns with no records, based on all combinations 
#' of the \code{columnVar} (assuming nested variable(s)).
#' If variable(s) are not nested, possible combinations
#' can be specified via \code{colVarDataLevels}.
#' @param varLab Named character vector with label for each variable 
#' specified in \code{var}.
#' By default, extracted from the \code{labelVars}.
#' @param rowVarDataLevels Data.frame with levels to consider for the \code{rowVar} variable(s)
#' (in case of non nested row variable(s)).
#' @param colVarDataLevels Data.frame with levels to consider for the \code{colVar} variable(s)
#' (in case of non nested column variable(s)).
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
	varIncludeTotal = FALSE,
	type = "auto",
	rowVar = NULL, rowInclude0 = FALSE, rowVarDataLevels = NULL,
	colVar = NULL, colInclude0 = FALSE, colVarDataLevels = NULL,
	subjectVar = "USUBJID",
	labelVars = NULL,
	statsExtra = NULL){

	computeSummaryStatisticsCustom <- function(...)
		computeSummaryStatistics(..., 
			subjectVar = subjectVar, 
			varIncludeTotal = varIncludeTotal,
			statsExtra = statsExtra
		)
		
	# build variables used for grouping:
	# 1) consider 'ddply(, .drop = FALSE)' to also include zeros
	# 2) build interaction of row variable(s), column variable(s) to only consider 
	# available combinations of row/column variables (if multiple variables in each direction)
	# otherwise ddply(, .drop = FALSE) with also include combinations of elements non present 
	# e.g. AE term in an AE group not present
	# (variables considered independently)
	groupVar <- c(
		if(!is.null(rowVar)){
			if((!rowInclude0) | (!is.null(rowVarDataLevels))){
				resICR <- interactionCustom(data = data, var = rowVar, varDataLevels = rowVarDataLevels)
				data$rowVariables <- resICR$x
				rowDataLevels <- resICR$dataLevels
				"rowVariables"
			}else	rowVar
		},
		if(!is.null(colVar)){
			if((!colInclude0) | (!is.null(colVarDataLevels))){
				resICC <- interactionCustom(data = data, var = colVar, varDataLevels = colVarDataLevels)
				data$colVariables <- resICC$x
				colDataLevels <- resICC$dataLevels
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
			if(!is.null(summaryTable) && length(var) > 1){
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
	if(!is.null(rowVar) & ((!rowInclude0) | (!is.null(rowVarDataLevels)))){
		summaryTable[, rowVar] <- rowDataLevels[match(summaryTable$rowVariables, rowDataLevels$factorLevel), rowVar]
		summaryTable$rowVariables <- NULL
	}
	if(!is.null(colVar) & ((!colInclude0) | (!is.null(colVarDataLevels)))){
		summaryTable[, colVar] <- colDataLevels[match(summaryTable$colVariables, colDataLevels$factorLevel), colVar]
		summaryTable$colVariables <- NULL
	}
	
	if("variableGroup" %in% colnames(summaryTable) && varIncludeTotal)
		summaryTable[, "variableGroup"] <- factor(summaryTable[, "variableGroup"], 
			levels = c(setdiff(levels(summaryTable[, "variableGroup"]), "Total"), "Total")
		)
		
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
#' @param varIncludeTotal Logical, if TRUE (FALSE by default) the total across 
#' all categories of \code{var} is included.
#' Only considered if \code{type} is 'countTable'.
#' @param statsExtra (optional) Named list with functions for additional custom
#' statistics to be computed, only available for 'summaryTable',
#' e.g. list(statCVPercN = function(x) sd(x)/mean(x)*100).
#' Each function has as parameter: either 'x': the variable or 'data': the entire dataset,
#' and return the corresponding summary statistic.
#' @return Data.frame with summary statistics in columns,
#' depending if \code{type} is:
#' \itemize{
#' \item{'summary': }{
#' \itemize{
#' \item{'statN': }{number of subjects }
#' \item{'statm': }{number of records}
#' \item{'statMean': }{mean of \code{var}}
#' \item{'statSD': }{standard deviation of \code{var}}
#' \item{'statSE': }{standard error of \code{var}}
#' \item{'statMedian': }{median of \code{var}}
#' \item{'statMin': }{minimum of \code{var}}
#' \item{'statMax': }{maximum of \code{var}}
#' }
#' }
#' \item{'count': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statm': }{number of records}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom stats na.omit median sd
#' @importFrom methods formalArgs
#' @export
computeSummaryStatistics <- function(data, 
	var = NULL, varIncludeTotal = FALSE,
	statsExtra = NULL,
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

	# wrapper to add extra statistics
	statsExtraFct <- function(res, statsExtra, val = NULL, data){
		if(!is.null(statsExtra)){
			statsExtraCommon <- intersect(names(statsExtra), colnames(res))
			if(length(statsExtraCommon) > 0)
				stop("Please specify names for 'statsExtra' different than default statistics.")
			resExtra <- sapply(statsExtra, function(fct){
				switch(formalArgs(fct), 'x' = fct(val), 'data' = fct(data))
			}, simplify = FALSE)	
			res <- cbind(res, resExtra)
		}
		return(res)
	}
	
	switch(type,
			
		'summaryTable' = {
			
			val <- data[, var]
			emptyVar <- is.null(val) || length(val) == 0
			res <- if(!(filterEmptyVar & emptyVar)){
				res <- data.frame(
					statN = getNSubjects(data),
					statm = getNRecords(data),
					statMean = ifelse(emptyVar, NA, mean(val)),
					statSD = ifelse(emptyVar, NA, sd(val)),
					statSE = ifelse(emptyVar, NA, se(val)),
					statMedian = ifelse(emptyVar, NA, median(val)),
					statMin = ifelse(emptyVar, NA, min(val)),
					statMax = ifelse(emptyVar, NA, max(val))
				)
				res <- statsExtraFct(
					res = res, statsExtra = statsExtra, 
					val = val, data = data
				)
			}
		},
		
		'countTable' = {
			
			# to avoid that ddply with empty data returns entire data.frame
			if(nrow(data) == 0){
				res <- data.frame(statN = 0, statm = 0)
				statsExtraFct(
					res = res, statsExtra = statsExtra,
					data = data
				)
			}else{
				res <- ddply(data, var, function(x){
					if(!(filterEmptyVar & nrow(x) == 0)){
						res <- data.frame(
							statN = getNSubjects(x),
							statm = getNRecords(x)
						)
						statsExtraFct(
							res = res, statsExtra = statsExtra,
							data = x
						)
					}
				}, .drop = FALSE)
				if(varIncludeTotal & (!(filterEmptyVar & nrow(data) == 0))){
					resTotal <- data.frame(
						statN = getNSubjects(data),
						statm = getNRecords(data)
					)
					resTotal <- statsExtraFct(res = resTotal, statsExtra = statsExtra, data = data)
					resTotal[, var] <- "Total"
					res <- rbind.fill(res, resTotal)
					res[, var] <- factor(res[, var], 
						levels = c(
							if(is.factor(data[, var]))	levels(data[, var])	else	unique(data[, var]),
							"Total"
						)
					)
				}
					
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
#' If no \code{otherVars} is specified, consider all rows.}
#' }
#' }
#' \item{Function to be applied on each subset to get the order elements of the variable}
#' }
#' @param totalVar String with variable of \code{data} considered in case \code{type} is 'total',
#' 'statN' by default.
#' @param totalFilterFct (optional) Function which returns a subset of the data of interest,
#' to filter the total data considered for the ordering.
#' @return Factor \code{var} variable of \code{data} with specified order.
#' @importFrom plyr daply
#' @author Laure Cougnaud
convertVarToFactorWithOrder <- function(
	data, var, otherVars = NULL, 
	method = c("auto", "alphabetical", "total"),
	totalFilterFct = NULL,
	totalVar = "statN"){

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
					
					# remove total for this variable
					dataForTotal <- data[which(data[, var] != "Total"), ]
					
					# filter records if any 'filterFct' is specified
					if(!is.null(totalFilterFct))
						dataForTotal <- totalFilterFct(dataForTotal)
					
					if(!is.null(otherVars) && length(otherVars) > 0){
						# consider rows with subtotal for this variable (if any)
						idxRowTotal <- which(rowSums(dataForTotal[, otherVars, drop = FALSE] == "Total") == length(otherVars))
						if(length(idxRowTotal) > 0)
							dataForTotal <- dataForTotal[idxRowTotal, ]
					}
					
					totalPerVar <- daply(dataForTotal, var, function(x) sum(x[, totalVar], na.rm = TRUE))
					varLevels <- c(
						if("Total" %in% data[, var])	"Total", 
						setdiff(names(sort(totalPerVar, decreasing = TRUE)), "Total")
					)
					varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
					factor(data[, var], levels = varLevels)
					
			}
		)
		
	}else{
		
		varLevels <- method(data)
		res <- factor(data[, var], levels = varLevels)
		
	}
	
	return(res)
		
}

