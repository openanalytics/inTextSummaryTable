#' Compute summary statistics for a specific dataset and variables of interest
#' @param var Character vector, variable(s) of \code{data} to compute statistics on.
#' Missing values, if present, are filtered.
#' If NULL (by default), counts of the \code{rowVar} are returned.
#' @param varFlag Character vector, subset of \code{var} with variable(s) of type 'flag' (with 'Y' or 'N').
#' Only the counts for records flagged (with 'Y') are retained.
#' @param rowOrder Specify how the rows should be ordered in the table, either a:
#' \itemize{
#' \item{String among:}{
#' \itemize{
#' \item{'auto' (by default): }{if the variable is a factor, keep its order, otherwise order alphabetically}
#' \item{'alphabetical': }{order alphabetically}
#' \item{'total': }{order rows in decreasing order of the total number of subjects
#' across all columns for this specific category.}
#' }}
#' \item{Function with input the summary table and output the ordered elements of the \code{rowVar}}
#' }
#' To specify different ordering methods for different \code{rowVar}, specify a list
#' of such elements, named with the \code{rowVar} variable.
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @param rowOrderTotalFilterFct Function used to filter the data used to order the rows
#' based on total counts (in case \code{rowOrder} is 'total'),
#' To order rows based on one specific column category,
#' e.g. to order based on the counts in the treatment column:
#' function(x) subset(x, TRTP == "treatmentX")
#' @param rowOrderCatLast String with category to be printed last of each \code{rowVar}
#' (if any, set to NULL if none). By default, category labelled 'Other' is printed last.
#' @param rowVarTotalInclude Character vector with \code{rowVar}
#' for which to include the total for each group.
#' @param rowVarTotalByVar Character vector with extra \code{rowVar}
#' for which to compute the row total by.
#' Can be specified for each \code{rowVarTotalInclude} if named by the corresponding variable.
#' @param varGeneralLab String with general label for variable specified in \code{var}.
#' In case of multiple variable in \code{var}, this will be included in the table header
#' (see 'rowVarLab' attribute of the output).
#' @param varSubgroupLab String with general label for sub-group, in case
#' \code{var} is specified for a count table.
#' This will be included in the table header (see 'rowVarLab' attribute of the output).
#' Empty by default.
#' @param varIgnore Vector with elements to ignore in the \code{var} variable
#' @param dataTotal Data.frame used to extract the Total number of subject
#' per column in column header ('N = [X]').
#' It should contain the variables specified by \code{colVarTotal}.
#' If not specified, the total number of subjects is extracted from the \code{data}.
#' @param dataTotalPerc Data.frame used to extract the Total count per column 
#' for the computation of the percentage ('statPercN' statistic).
#' By default same as \code{dataTotal} .
#' It should contain the variables specified by \code{colVarTotalPerc}.
#' @param dataTotalRow Data.frame used to extract the total count across all
#' elements of the row
#' variable, in case \code{rowVarTotalInclude} is specified,
#' or list of such data.frame for each \code{rowVar} variable (named by variable).
#' @param dataTotalCol Data.frame from which the total across columns is 
#' extracted (in case \code{colTotalInclude} is TRUE)
#' or list of such data.frame for each \code{rowVar} variable.
#' This data is used for:
#' \itemize{
#' \item{the header of the total column in case \code{dataTotal} is
#' not specified}
#' \item{the denominator of the percentages in the total column
#' in case \code{dataTotalPerc} is not specified}
#' }
#' @param filterFct (optional) Function based on computed statistics of
#' \code{rowVar}/code{colVar} which returns a subset of the summary table 
#' (after statistics computation).
#' Note: The filtering function should also handle records with :
#' \itemize{
#' \item{\code{rowVar}/\code{codeVar} set to 'Total'/\code{colTotalLab} 
#' if \code{rowVarTotalInclude}/\code{colTotalInclude} is TRUE}
#' \item{total for the column header: \code{isTotal} set to TRUE,
#' and \code{colVar}/\code{rowVar} is NA}
#' }
#' Note: the total count per category of the row variables is included
#' in a row with this variable set to 'Total'.
#' @param colVarTotal String with column(s) considered to compute the total by,
#' reported in the header of the table, by default same as \code{colVar}.
#' @param colVarTotalPerc String with column(s) considered to compute the total by,
#' used as denominator for the percentage computation, by default same as \code{colVarTotal}.
#' @param rowVarTotalPerc Character vector with row variables by which the total
#' should be computed for the denominator for the percentage computation.
#' By default the total is only computed by column (NULL by default).
#' If the total should be based on the total number of records per variable,
#' \code{rowVarTotalPerc} should be set to 'variable'.
#' @param byVar Variable(s) of \code{data} for which separated table(s)
#' should be created.
#' @param byVarLab String with label for \code{byVar}, used to set the names
#' of the output list of table(s).
#' @param stats (Optionally) Either:
#' \itemize{
#' \item{string with: }{
#' \itemize{
#' \item{'default': }{default sets of statistics, 
#' see types: 'summary-default' and 'count-default' in \code{\link{getStats}}}
#' \item{'all': }{all computed statistics, see types: 'summary' and 'count' in \code{\link{getStats}}}
#' }}
#' \item{Named list of expressions or call objects of summary statistics of interest: }{
#' The names are reported in the header.
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
#' by naming each element of the list:
#' list(varName1 = list(...), varName2 = list()) and/or for each element in:
#' \code{statsVarBy}, by naming each sublist.
#' }}
#' @param statsGeneralLab String with general label for statistics, 'Statistic' by default.
#' Only included if no \code{statsVar} if longer than 1.
#' @param varIncludeTotal This argument is deprecated, please use: 'varTotalInclude' instead.
#' @param varTotalInSepRow Logical, should the total per variable be included in
#' a separated row (by default) or in the row containing the header of the variable?
#' @param rowVarTotalInSepRow Character vector with \code{rowVarTotalInclude}
#' (not in \code{rowVarInSepCol}) for which the total should be included in a separated row labelled 'Total'.
#' Otherwise (by default) the total is included in the header row of each category.
#' @inheritParams computeSummaryStatisticsTableTotal
#' @inheritParams computeSummaryStatisticsByRowColVar
#' @inheritParams getStatisticsSummaryStatisticsTable
#' @inheritParams convertVarToFactorWithOrder
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
#' \item{'statPercTotalN': }{total number of subjects based on \code{dataTotalPerc},
#' denominator of \code{statPerc}}
#' \item{'statm': }{number of records}
#' }
#' }
#' \item{'countTable': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statPercN': }{percentage of subjects}
#' \item{'statPercTotalN': }{total number of subjects based on \code{dataTotalPerc},
#' denominator of \code{statPerc}}
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
#' Additionally, the output contains the following attributes:
#' \itemize{
#' \item{'statsVar': }{column name(s) of summary table with computed statistics
#' included in the final table}
#' \item{'rowVar': }{column name(s) of summary table with row variable
#' included in the final table}
#' \item{'rowVarLab': }{labels corresponding to the 'rowVar' attribute}
#' \item{'rowVarTotalInclude': }{row variables whose total will be included:
#' \code{rowVarTotalInclude} and 'variableGroup' if the variable total should be included}
#' \item{'rowVarTotalInSepRow': }{row variables whose total will be included in a separated row:
#' \code{rowVarTotalInSepRow} and 'variableGroup' if \code{varTotalInSepRow}}
#' \item{'colVar': }{column name(s) of summary table with column variable
#' included in the final table}
#' \item{'colTotalLab': }{label for the total}
#' } 
#' @author Laure Cougnaud
#' @importFrom dplyr n_distinct
#' @importFrom plyr ddply rbind.fill dlply
#' @importFrom glpgUtilityFct getLabelVar
#' @export
computeSummaryStatisticsTable <- function(
	data,  
	var = NULL, varFlag = NULL, varInclude0 = FALSE,
	varLab = getLabelVar(var, data = data, labelVars = labelVars),
	varGeneralLab = "Variable", varSubgroupLab = NULL,
	varIgnore = NULL,
	varIncludeTotal = FALSE,
	varTotalInclude = FALSE,
	varTotalInSepRow = FALSE,
	colVar = NULL, colVarDataLevels = NULL, 
	colVarTotal = colVar, 
	colVarTotalPerc = colVarTotal,
	colTotalInclude = FALSE, colTotalLab = "Total",
	rowVar = NULL, rowVarDataLevels = NULL, 
	rowVarLab = getLabelVar(rowVar,  data = data, labelVars = labelVars),
	rowOrder = "auto", rowOrderTotalFilterFct = NULL, rowOrderCatLast = "Other",
	rowVarTotalInclude = NULL,
	rowVarTotalInSepRow = NULL,
	rowVarTotalByVar = NULL,
	rowVarTotalPerc = NULL,
	type = "auto",
	subjectVar = "USUBJID",	
	dataTotal = NULL, dataTotalPerc = dataTotal,
	dataTotalRow = NULL,
	dataTotalCol = NULL,
	stats = NULL, 
	statsVarBy = NULL,
	statsExtra = NULL,
	statsGeneralLab = "Statistic",
	filterFct = NULL,
	rowInclude0 = FALSE, colInclude0 = FALSE,
	labelVars = NULL,
	byVar = NULL, byVarLab = getLabelVar(byVar, data = data, labelVars = labelVars)
){
	
	# get default set of statistics
	if(is.character(stats) && length(stats) == 1)
		stats <- getStatsData(data = data, var = var, type = stats)
	
	# check if 'varIncludeTotal' is not default
	if(!(is.logical(varIncludeTotal) && length(varIncludeTotal) == 1 && !varIncludeTotal)){
		warning("Argument: 'varIncludeTotal' is deprecated, please use 'varTotalInclude' instead.")
		varTotalInclude <- varIncludeTotal
	}
	
	inputParams <- as.list(environment())
	
	if(nrow(data) == 0){
		message("No data to report.")
		return(invisible())
	}
	
	if(!is.null(dataTotal) && !all(colVar %in% colnames(dataTotal)))
		stop("The variable(s) specified in 'colVar': ",
			toString(paste0("'", colVar, "'")), 
			" are not available in 'dataTotal'.")

	if(!is.null(byVar)){
		if(!all(byVar %in% colnames(data))){
			warning("'byVar' is not available in the 'data' so is not used.")
			byVar <- FALSE
		}else{
			res <- dlply(data, byVar, function(dataBy){
				inputParamsBy <- inputParams
				inputParamsBy$data <- dataBy
				inputParamsBy$byVar <- NULL
				do.call(computeSummaryStatisticsTable, inputParamsBy)		
			})	
			if(!is.null(byVarLab)){
				uniqueNameDf <- unique(data[, byVar, drop = FALSE])
				newName <- do.call(paste, 
					c(mapply(paste, byVarLab[byVar], uniqueNameDf, sep = ": ", SIMPLIFY = FALSE),
					sep = "\n")
				)
				initName <- do.call(paste, c(uniqueNameDf, sep = "."))
				names(res) <- newName[match(names(res), initName)]
			}
			return(res)
		}
	}

	# Always compute the column total, because the rows could be asked to be ordered 
	# based on the total category or total can be extracted within a function specified in rowOrder
	# excepted when no column variable is specified
	colTotalIncludeInit <- colTotalInclude
	if(is.null(colVar)){
		if(colTotalIncludeInit)	
			warning("Column 'total' is not included because no column variable is specified.")
		colTotalInclude <- FALSE
	}else	colTotalInclude <- TRUE
	
#	if(!is.null(colVar))
#	checkIfTotal <- function(x)	!is.function(x) && any(x == "total")
#	if(!colTotalInclude && any(sapply(rowOrder, checkIfTotal)))
#		colTotalInclude <- TRUE
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		for(varI in var){
			data <- data[!data[, varI] %in% varIgnore, ]
		}
	
	# for flag variable:
	if(!is.null(varFlag)){
		if(!all(varFlag %in% var))
			stop("All flag variables in 'varFlag' should be specified in the 'var' parameter.")
		# convert them to a format to only retain flagged records
		data[, varFlag] <- colwise(convertVarFlag)(data[, varFlag, drop = FALSE])
		# filter the 'non' flagged counts:
		# 'variableInit' only included
		filterFctFlag <- function(x){x}		
		body(filterFctFlag) <- bquote({
			isVar <- if(.(length(var) > 1)){
				x$variableInit %in% varFlag
			}else{TRUE}
			idxKept <- which(x$isTotal | !(isVar & x$variableGroup == "N"))
			x[idxKept, ]
		})
		filterFct <- c(filterFct, list(filterFctFlag))
	}
	
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
		
	# get general statistics for each combination of rowVar/colVar
	summaryTable <- computeSummaryStatisticsByRowColVar(
		data = data, 
		var = var, varLab = varLab, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
		statsExtra = statsExtra,
		type = type,
		rowVar = rowVar, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
		colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar, labelVars = labelVars
	)
	
	if(nrow(summaryTable) == 0)
		stop("No data available (or all missing values).")
	
	# original levels of colVar in summaryTable
	colVarLevels <- sapply(
		summaryTable[, colVar, drop = FALSE], function(x)
			if(is.factor(x))	levels(x)	else	sort(unique(x)),
		simplify = FALSE
	)
	
	# get statistics across columns by row
	if(colTotalInclude){
		
		dataForColTotal <- if(!is.null(dataTotalCol)){
			# different datasets for the different row variables:
			if(is.list(dataTotalCol) && !is.data.frame(dataTotalCol)){ 
				if(!is.null(rowVar) && rowVar[length(rowVar)] %in% names(dataTotalCol)){
					dataTotalCol[[rowVar[length(rowVar)]]]
				}else	data
			# one unique df:
			}else	dataTotalCol
		}else	data
		
		summaryTableColTotal <- computeSummaryStatisticsByRowColVar(
			data = dataForColTotal, 
			var = var, varLab = varLab, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
			statsExtra = statsExtra,
			type = type,
			rowVar = rowVar, rowInclude0 = rowInclude0,	rowVarDataLevels = rowVarDataLevels,
			subjectVar = subjectVar, labelVars = labelVars
		)
		
		if(nrow(summaryTableColTotal) > 0)
			summaryTableColTotal[, colVar] <- colTotalLab
			
		summaryTable <- rbind.fill(summaryTable, summaryTableColTotal)
		summaryTable[, colVar] <- lapply(colVar, function(x)
			factor(summaryTable[, x], levels = unique(c(colVarLevels[[x]], colTotalLab)))
		)
		
	}
	
	if(!is.null(rowVarTotalInclude)){
		
		# order specified variables as in rowVar
		rowVarSubTotal <- intersect(rowVar, rowVarTotalInclude)
		
		# compute sub-total for each specified rowVar (excepted the last one)
		summaryTableRowSubtotal <- data.frame()
		for(rVST in rowVarSubTotal){
			
			dataForSubTotal <- if(!is.null(dataTotalRow)){
				if(is.data.frame(dataTotalRow)){
					dataTotalRow
				}else{
					if(rVST %in% names(dataTotalRow)){
						dataTotalRow[[rVST]]
					}else	stop(paste0("'dataTotalRow' missing for: ", rVST, "."))
				}
			}else	data
	
			# remove rows which have NA for the nested sub-variable
			# otherwise have summary statistics are duplicated (sub-total and initial)
			rowVarSubTotalOther <- rowVar[
				setdiff(seq_along(rowVar), seq_len(match(rVST, rowVar)))
			]
			filterRowNestedVar <- function(data){
				if(length(rowVarSubTotalOther) > 0){
					idxMissingSubVar <- which(
						rowSums(is.na(data[, rowVarSubTotalOther, drop = FALSE])) == length(rowVarSubTotalOther)
					)
					if(length(idxMissingSubVar) > 0)
						data <- data[-idxMissingSubVar, ]
				}
				data
			}
			dataForSubTotal <- filterRowNestedVar(data = dataForSubTotal)
			
			# compute statistics by higher level rowVar
			rowVarOther <- rowVar[seq_len(match(rVST, rowVar)-1)]
			if(length(rowVarOther) == 0) rowVarOther <- NULL
			
			# extra variable used to compute the total by
			rowVarOtherExtra <- if(!is.null(rowVarTotalByVar)){
				if(!is.null(names(rowVarTotalByVar))){
					if(!is.null(rowVarTotalByVar[rVST]))
						rowVarTotalByVar[rVST]
				}else	rowVarTotalByVar
			}
			if(!is.null(rowVarOtherExtra)){
				if(!rowVarOtherExtra %in% rowVar)
					stop("'rowVarTotalByVar' not in 'rowVar'.")
				rowVarOther <- c(rowVarOther, rowVarOtherExtra)
			}
			
			# compute statistics
			summaryTableRowSubtotalVar <- computeSummaryStatisticsByRowColVar(
				data = dataForSubTotal, 
				var = var, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
				statsExtra = statsExtra,
				type = type,
				rowVar = rowVarOther, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
				colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
				subjectVar = subjectVar, varLab = varLab, labelVars = labelVars
			)
			
			# include also the total across columns (if required)
			if(colTotalInclude){
				
				rowVarOtherCol <- if(is.null(rowVarOther))	'total'	else	rowVarOther
				dataForSubTotalForColTotal <- if(!is.null(dataTotalCol)){
					# different datasets for the different row variables:
					if(is.list(dataTotalCol) && !is.data.frame(dataTotalCol)){ 
						if(!is.null(rowVar) && rowVarOtherCol %in% names(dataTotalCol)){
							filterRowNestedVar(dataTotalCol[[rowVarOtherCol]])
						}else	dataForSubTotal
					# one unique df:
					}else	dataTotalCol
				}else	dataForSubTotal
				
				summaryTableRowSubtotalVarColTotal <- computeSummaryStatisticsByRowColVar(
					data = dataForSubTotalForColTotal, 
					var = var, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
					statsExtra = statsExtra,
					type = type,
					rowVar = rowVarOther, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
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
				summaryTableRowSubtotalVar[, setdiff(rowVar, rowVarOther)] <- "Total"
			
			# save results
			summaryTableRowSubtotal <- rbind.fill(summaryTableRowSubtotal, summaryTableRowSubtotalVar)
			
		}
	
		# bind the df with row total and subtotal to the summary table
		# ensure that the order of the levels of the variable are retained		
		rowVarLevels <- sapply(
			summaryTable[, rowVar, drop = FALSE], function(x)
				if(is.factor(x))	levels(x)	else	sort(unique(x)),
			simplify = FALSE
		)
		
		summaryTable <- rbind.fill(summaryTable, summaryTableRowSubtotal)
		
		summaryTable[, rowVar] <- lapply(rowVar, function(var){
			xVar <- summaryTable[, var]
			# only add Total if included in the table (case: missing nested var)
			levelsX <- unique(c("Total", rowVarLevels[[var]]))
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
					factor(dataTotal[, var], levels = colVarLevels[[var]])
				}else dataTotal[, var]
			)
		}
		dataTotalColTotalHeader <- dataTotal
	}else{
		dataTotal <- data
		# in case no 'dataTotal' is included, consider 'dataTotalCol' for the header across columns
		dataTotalColTotalHeader <- if(colTotalInclude)	dataForColTotal
	}
	
	# get total for column headers:
	summaryTableTotal <- computeSummaryStatisticsTableTotal(
		data = dataTotal, 
		colVar = colVar, colVarTotal = colVarTotal,
		colTotalLab = colTotalLab,
		colInclude0 = colInclude0, 
		colTotalInclude = colTotalInclude, dataTotalCol = dataTotalColTotalHeader,
		colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
		subjectVar = subjectVar,
		# not used:
		var = var, varLab = varLab, labelVars = labelVars
	)
	
	# save total or not in the 'isTotal' column
	summaryTableTotal$isTotal <- TRUE
	if(nrow(summaryTable) > 0)
		summaryTable$isTotal <- FALSE
	# bind to the summary table
	summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	
	## compute percentages
	
	# get counts (for percentage computation)
	if(is.null(dataTotalPerc)){
		dataTotalPerc <- dataTotal
		dataTotalPercTotalHeader <- dataTotalColTotalHeader
	}else{
		dataTotalPercTotalHeader <- dataTotalPerc
	}
	if(!all(colVarTotalPerc %in% colnames(summaryTable)))
		stop("'colVarTotalPerc' are not in the computed summary statistics table.")
	computeTotalPerc <- !setequal(colVarTotal, colVarTotalPerc) | !is.null(rowVarTotalPerc)
	summaryTableTotalPerc <- if(computeTotalPerc){
		computeSummaryStatisticsTableTotal(
			data = dataTotalPerc, dataTotalCol = dataTotalPercTotalHeader,
			colVar = colVar, colVarTotal = colVarTotalPerc,
			colTotalLab = colTotalLab,
			colInclude0 = colInclude0, colTotalInclude = colTotalInclude,
			colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
			rowVarTotal = rowVarTotalPerc, 
			var = var, varLab = varLab, labelVars = labelVars,
			subjectVar = subjectVar	
		)
	}else	summaryTableTotal
	summaryTable <- rbind.fill(
		cbind(summaryTable, isTotalPerc = FALSE), 
		cbind(summaryTableTotalPerc, isTotalPerc = TRUE)
	)
	
	# compute percentages
	summaryTable <- ddply(summaryTable, c(rowVarTotalPerc, colVarTotalPerc), function(x){
		idxTotalPerc <- which(x$isTotalPerc)
		if(length(idxTotalPerc) > 0){
			if(length(idxTotalPerc) != 1)
				stop("Multiple total records for the percentage computation.")
			statPercTotalN <- x[idxTotalPerc, "statN"]
			res <- cbind(x, statPercTotalN = statPercTotalN, statPercN = x$statN/statPercTotalN*100)
			res[-idxTotalPerc, ]
		}else cbind(x, statPercTotalN = NA, statPercN = NA)
	})
	summaryTable$isTotalPerc <- NULL

	# filter records if any 'filterFct' is specified
	if(!is.null(filterFct)){
		if(!is.list(filterFct))	filterFct <- list(filterFct)
		for(fct in filterFct)
			summaryTable <- fct(summaryTable)
	}
	
	# order the row variables as specified
	if(!is.null(rowVar) && !is.null(rowOrder)){
		summaryTable[, rowVar] <- lapply(rowVar, function(var){
			# (double bracket works for list and character vector)
			methodVar <- if(var %in% names(rowOrder))	rowOrder[[var]]	else rowOrder
			convertVarToFactorWithOrder(
				data = summaryTable, var = var, 
				method = methodVar,
				catLast = rowOrderCatLast,
				totalFilterFct = rowOrderTotalFilterFct,
				otherVars = setdiff(rowVar, var),
				colVar = colVar, colTotalLab = colTotalLab
			)
		})
	}
	
	# extract statistic names
	statsVarInit <- setdiff(
		colnames(summaryTable), 
		c(rowVar, colVar, ".id", "variable", "variableGroup", "isTotal", "variableInit")
	)
	
	# compute stats
	resStats <- getStatisticsSummaryStatisticsTable(
		summaryTable = summaryTable, 
		statsVarInit = statsVarInit, 
		var = var, 
		stats = stats,
		statsVarBy = statsVarBy
	)
	summaryTable <- resStats$summaryTable
	statsVar <- resStats$statsVar

	colsToRemove <- which(colnames(summaryTable) %in% c(".id", "variableInit"))
	if(length(colsToRemove) > 0)
		summaryTable <- summaryTable[, -colsToRemove]
	
	# remove the rows with total if should be ordered by total but the total not included
	if(!colTotalIncludeInit & colTotalInclude){
		idxColTotal <- which(rowSums(summaryTable[, colVar, drop = FALSE] == colTotalLab) > 0)
		if(length(idxColTotal) > 0)	summaryTable <- summaryTable[-idxColTotal, ]
	}
	
	# if only flag variables, remove 'variableGroup'
	# (otherwise empty line when not specifying 'stats')
	if("variableGroup" %in% colnames(summaryTable)){
		uniqueGroup <- all(summaryTable[which(!summaryTable$isTotal), "variableGroup"] == "", na.rm = TRUE)
		if(uniqueGroup)
			summaryTable[, which(colnames(summaryTable) == "variableGroup")] <- NULL
	}
	
	# sort columns
	colSorted <- c(rowVar, colVar, "variable", "variableGroup", "isTotal", statsVarInit, statsVar)
	colSorted <- intersect(colSorted, colnames(summaryTable)) 
	colSorted <- c(colSorted, setdiff(colnames(summaryTable), colSorted))
	if(length(colSorted) > 0)
		summaryTable <- summaryTable[, colSorted]
	
	# table attributes
	attrTable <- list(
			
		# attributes created from this function
		statsVar = statsVar,
		rowVar = c(rowVar, 
			if(length(var) > 1)	"variable", 
			# in case only one variable, but still count
			if("variableGroup" %in% colnames(summaryTable))	"variableGroup"
		),
		rowVarLab = c(
			rowVarLab, 
			if(length(var) > 1)	
				c("variable" = varGeneralLab),
			if("variableGroup" %in% colnames(summaryTable))	c('variableGroup' = varSubgroupLab),
			if(length(statsVar) > 1)	c("Statistic" = statsGeneralLab)
		),

		# attributes extracted from the input parameters,
		# to set similar defaults for the exportSummaryStatisticsTable
		rowVarTotalInclude = {
			c(
				rowVarTotalInclude,
				# add 'variableGroup' if the total should be computed by variable group
				if(!is.null(var) && (
					(is.logical(varTotalInclude) && varTotalInclude) ||
					any(var %in% varTotalInclude)
				))	"variableGroup"
			)
		},
		rowVarTotalInSepRow = c(rowVarTotalInSepRow, if(varTotalInSepRow)	"variableGroup"),
		colVar = colVar,
		colTotalLab = colTotalLab

	)
	
	attributes(summaryTable) <- c(attributes(summaryTable), list(summaryTable = attrTable))
	
	return(summaryTable)
	
}

#' Compute summary statistics total table.
#' @param colVarTotal Character vector with column(s) considered to compute the total.
#' @param rowVarTotal Character vector with row(s) considered to compute the total.
#' @param var Character vector with variable, used if 'variable' is specified
#' within \code{rowVarTotal}.
#' @param colVarLevels list with levels of each \code{colVar}
#' @param colTotalInclude Logical, if TRUE (FALSE by default) include the summary 
#' statistics across columns in a separated column.
#' @param dataTotalCol Data.frame from which the total across columns is 
#' extracted (in case \code{colTotalInclude} is TRUE).
#' @inheritParams computeSummaryStatisticsByRowColVar
#' @return data.frame with total table.
#' @author Laure Cougnaud
#' @importFrom reshape2 melt
#' @keywords internal
computeSummaryStatisticsTableTotal <- function(
	data, 
	colVar = NULL, colVarTotal = colVar,
	rowVarTotal = NULL, 
	var = NULL, varLab = getLabelVar(var, data = data, labelVars = labelVars),
	colTotalLab = "Total",
	colInclude0 = FALSE,
	colTotalInclude = FALSE, dataTotalCol = NULL,
	colVarDataLevels = NULL, colVarLevels = NULL,
	subjectVar = "USUBJID",
	labelVars = NULL){

	# in case total should be computed by 'var'
	# convert wide -> long format: one column with all variables
	formatDataTotalWithVar <- function(data){
		data <- melt(
			data = data, 
			measure.vars = var, 
			variable.name = "variableInit",
			value.name = "value",
			factorsAsStrings = FALSE # reshape2 >= 1.4
		)	
		data <- data[!is.na(data$value), ]# remove records not in the variable
		data$variable <- factor(
			varLab[as.character(data$variableInit)],
			levels = varLab[var]
		)
		return(data)
	}

	# total is computed based on number of elements available in each 'var'
	# (and not # elements within each group of 'var')
	if("variable" %in% rowVarTotal){
		if(is.null(var)){
			warning("Total is not computed by variable because no 'var' is specified.")
			rowVarTotal <- setdiff(rowVarTotal, "variable")
		}else{
			data <- formatDataTotalWithVar(data)
		}
	}
	
	# counts by elements in colVar
	summaryTableTotal <- computeSummaryStatisticsByRowColVar(
		data = data, 
		type = "countTable", 
		colVar = colVarTotal, 
		rowVar = rowVarTotal,
		colInclude0 = colInclude0, 
		colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar
	)
	
	# counts across all elements of colVar
	if(colTotalInclude){
		colVarTotalTI <- setdiff(colVarTotal, colVar)
		if(length(colVarTotalTI) == 0)	colVarTotalTI <- NULL
		if(is.null(dataTotalCol))	dataTotalCol <- data
		
		if("variable" %in% rowVarTotal)
			dataTotalCol <- formatDataTotalWithVar(dataTotalCol)
		
		summaryTableTotalCol <- computeSummaryStatisticsByRowColVar(
			data = dataTotalCol, 
			type = "countTable", 
			colVar = colVarTotalTI,
			rowVar = rowVarTotal,
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
#' @param varInclude0 Logical, if TRUE (FALSE by default)
#' include rows with no counts for the count \code{var} or \code{varFlag} variable(s).
#' @param colInclude0 Logical, if TRUE (FALSE by default),
#' include columns with no records, based on all combinations 
#' of the \code{columnVar} (assuming nested variable(s)).
#' If variable(s) are not nested, possible combinations
#' can be specified via \code{colVarDataLevels}.
#' @param varLab Named character vector with label for each variable 
#' specified in \code{var}.
#' By default, extracted from the \code{labelVars}.
#' @param rowVarDataLevels Data.frame with unique combinations of \code{rowVar}
#' to be included in columns.
#' Each column should correspond to \code{colVar} and as factor
#' if the elements should be ordered in the final table.
#' @param colVarDataLevels Data.frame with unique combinations of \code{colVar}
#' to be included in columns.
#' Each column should correspond to \code{colVar} and as factor
#' if the elements should be ordered in the final table.
#' @param varTotalInclude Should the total across all categories of \code{var} 
#' be included for the count table?
#' Either:
#' \itemize{
#' \item{logical of length 1, if TRUE (FALSE by default) include the total for all \code{var}}
#' \item{a character vector containing \code{var} for which the total should be included}
#' }
#' @inheritParams convertVarToFactorWithOrder
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
#' @keywords internal
computeSummaryStatisticsByRowColVar <- function(
	data, 
	var = NULL, varLab = getLabelVar(var = var, data = data, labelVars = labelVars), varInclude0 = FALSE,
	varTotalInclude = FALSE,
	type = "auto",
	rowVar = NULL, rowInclude0 = FALSE, rowVarDataLevels = NULL,
	colVar = NULL, colInclude0 = FALSE, colVarDataLevels = NULL,
	subjectVar = "USUBJID",
	labelVars = NULL,
	statsExtra = NULL){

	if(is.logical(varTotalInclude) && length(varTotalInclude) > 1)
		stop("If 'varTotalInclude' if a logical, it should be of length 1.")

	computeSummaryStatisticsCustom <- function(...)
		computeSummaryStatistics(..., 
			subjectVar = subjectVar, 
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
				type = type,
				varTotalInclude = varTotalInclude
			)
		}else{
			summaryTableVarList <- lapply(var, function(varI){
				varITotalInclude <- 
					(is.logical(varTotalInclude) && varTotalInclude) || 
					varI %in% varTotalInclude
				sumTable <- computeSummaryStatisticsCustom(
					data = x, 
					var = varI, 
					type = type,
					varTotalInclude = varITotalInclude,
					filterEmptyVar = !varInclude0
				)
				# only store the variable if more than one specified variable
				if(!is.null(sumTable) && nrow(sumTable) > 0 && length(var) > 1){
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
	
	# include the total as the last level
	varTotalIsIncluded <- 
		(is.logical(varTotalInclude) && varTotalInclude) ||
		any(var %in% varTotalInclude)
	if("variableGroup" %in% colnames(summaryTable) && varTotalIsIncluded)
		summaryTable[, "variableGroup"] <- factor(summaryTable[, "variableGroup"], 
			levels = c("Total", setdiff(levels(summaryTable[, "variableGroup"]), "Total"))
		)
		
	return(summaryTable)
	
}

#' Compute summary statistics of interest of an unique variable of interest.
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
#' @param varTotalInclude Logical (FALSE by default)
#' Should the total across all categories of \code{var} 
#' be included for the count table?
#' @param statsExtra (optional) Named list with functions for additional custom
#' statistics to be computed, only available for 'summaryTable',
#' e.g. list(statCVPerc = function(x) sd(x)/mean(x)*100) (or \code{\link{cv}}).
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
	var = NULL, varTotalInclude = FALSE,
	statsExtra = NULL,
	subjectVar = "USUBJID",
	filterEmptyVar = ((type == "auto" && is.numeric(data[, var])) | type == "summaryTable"),
	type = "auto"){

	## checks parameters

	type <- match.arg(type, choices = c("auto", "summaryTable", "countTable"))
	
	if(type == "auto")
		type <- ifelse(!is.null(var) && is.numeric(data[, var]), "summaryTable", "countTable")
	
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
				switch(
					formalArgs(fct)[1], 
					'x' = fct(val), 
					'data' = fct(data),
					stop("No parameter 'x' or 'data' for the 'statsExtra' function.")
				)
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
				
				if(!is.null(var)){
					res <- data.frame()
				}else{
					res <- data.frame(statN = 0, statm = 0)
				}
				statsExtraFct(
					res = res, statsExtra = statsExtra,
					data = data
				)
				
			}else{
				
				if(!is.null(var)){
					
					varLevels <- if(is.factor(data[, var]))	levels(data[, var])	else	unique(data[, var])
					resList <- lapply(varLevels, function(level){
						x <- data[which(data[, var] == level), ]	
						# compute stats in data or if filterEmptyVar is FALSE
						if(!(nrow(x) == 0 & filterEmptyVar)){
							res <- setNames(
								data.frame(level, getNSubjects(x), getNRecords(x)),
								c(var, "statN", "statm")
							)
							res <- statsExtraFct(
								res = res, statsExtra = statsExtra,
								data = x
							)
						}
					})
					resList <- resList[!sapply(resList, is.null)]
					if(length(resList) > 0){
						res <- do.call(rbind, resList)
						rownames(res) <- NULL
					}else{
						res <- data.frame()
					}
					
				}else{
					res <- data.frame(statN = getNSubjects(data), statm = getNRecords(data))
					res <- statsExtraFct(res = res, statsExtra = statsExtra, data = data)
				}
				
				if(varTotalInclude & (!(filterEmptyVar & nrow(data) == 0))){
					
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
#' @param catLast String with last category (if any).
#' By default, category labelled 'Other' is last.
#' Set to NULL if no specific category should be included as last element.
#' @param colVar Character vector with variable(s) used for the columns.
#' If multiple variables are specified, the variables should be sorted in hierarchical order,
#' and are included in multi-columns layout.
#' @param colTotalLab String, label for the total column included 
#' if \code{colTotalInclude} is TRUE, 'Total' by default.
#' @return Factor \code{var} variable of \code{data} with specified order.
#' @importFrom plyr daply
#' @author Laure Cougnaud
#' @keywords internal
convertVarToFactorWithOrder <- function(
	data, var, otherVars = NULL, 
	colVar = NULL, colTotalLab = "Total",
	method = c("auto", "alphabetical", "total"),
	totalFilterFct = NULL,
	totalVar = "statN",
	catLast = "Other"){

	if(!is.function(method)){
		
		method <- match.arg(method)
	
		res <- switch(method,
			'auto' = if(is.factor(data[, var]))
				data[, var]	else	
				convertVarToFactorWithOrder(
					data = data, var = var, catLast = catLast,
					method = "alphabetical"
				),
			'alphabetical' = {
				varLevels <- c(
					if("Total" %in% data[, var])	"Total", 
					sort(setdiff(unique(data[, var]), c(catLast, "Total")), decreasing = TRUE),
					if(catLast %in% data[, var])	catLast
				)
				factor(data[, var])
			},
			'total' = {
					
					# remove total for this variable
					dataForTotal <- data[which(data[, var] != "Total"), ]
					
					# filter records if any 'filterFct' is specified
					if(!is.null(totalFilterFct)){
						dataForTotal <- totalFilterFct(dataForTotal)
					}else{
						if(!is.null(colVar)){
							idxTotal <- which(rowSums(dataForTotal[, colVar, drop = FALSE] == colTotalLab) == length(colVar))
							if(length(idxTotal) == 0){
								warning("Total across columns not available to order the rows in the summary table.")
							}else{
								dataForTotal <- dataForTotal[idxTotal, ]
							}
						}
					}
					
					if(!is.null(otherVars) && length(otherVars) > 0){
						# consider rows with subtotal for this variable (if any)
						idxRowTotal <- which(rowSums(dataForTotal[, otherVars, drop = FALSE] == "Total") == length(otherVars))
						if(length(idxRowTotal) > 0)
							dataForTotal <- dataForTotal[idxRowTotal, ]
					}
					
					totalPerVar <- daply(dataForTotal, var, function(x) sum(x[, totalVar], na.rm = TRUE))
					varLevels <- c(
						if("Total" %in% data[, var])	"Total", 
						setdiff(names(sort(totalPerVar, decreasing = TRUE)), c("Total", catLast)),
						if(catLast %in% data[, var])	catLast
					)
					varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
					factor(data[, var], levels = varLevels)
					
			}
		)
		
	}else{
		
		varLevels <- as.character(method(data))
		varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
		if("Total" %in% varLevels)	varLevels <- c("Total", setdiff(varLevels, "Total"))
		res <- factor(data[, var], levels = varLevels)
		
	}
	
	return(res)
		
}

#' Compute custom statistics specified by the user.
#' @param summaryTable Summary table.
#' @param statsVarInit Character vector with initial statistics names.
#' @param var Character vector with variable, used if 'variable' is specified
#' within \code{rowVarTotal}.
#' @param stats (Optionally) named list of expression or call object of summary statistics of interest.
#' The names are reported in the header.
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
#' by naming each element of the list:
#' list(varName1 = list(...), varName2 = list()) and/or for each element in:
#' \code{statsVarBy}, by naming each sublist.
#' @param statsVarBy String with variable in \code{rowVar}/code{colVar}
#' which the statistics should be computed by.
#' In this case, \code{stats} (nested list or not) should be additionally nested
#' to specify the statistics for each element in \code{statsVarBy}.
#' @return List with two elements:
#' \itemize{
#' \item{'summaryTable': }{summary table updated with statistics specified in \code{stats}}
#' \item{'statsVar': }{Character vector with statistics names}
#' }
#' @author Laure Cougnaud
#' @keywords internal
getStatisticsSummaryStatisticsTable <- function(
	summaryTable, statsVarInit, 
	var = NULL, stats = NULL, statsVarBy = NULL){
	
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
			
			statsDf <- if(is.list(stats)){
				sapply(stats, function(expr)
					eval(expr = expr, envir = sumTable)
				, simplify = FALSE)
			}else	list(eval(expr = stats, envir = sumTable))
			if(is.null(statsName))	names(statsDf) <- "Statistic"
			
			# save in summaryTable
			sumTable <- cbind(sumTable, statsDf, stringsAsFactors = FALSE)
			
			return(sumTable)
			
		}
		
		getStatColName <- function(stats)
			if(is.null(names(stats)))	"Statistic"	else	names(stats)

		# variable to compute the statistics by:
		if(any(names(stats) %in% var)){
			statsVarByUsed <- c(statsVarBy, "variableInit")
			# in case more stats are specified than specified var
			varsUsed <- setdiff(unique(as.character(summaryTable$variableInit)), NA)
			if(!all(varsUsed %in% names(stats))){
				stop("If 'stats' is specified for each variable,",
					"it should be specified for all variables specified in 'var'.")
			}else stats <- stats[varsUsed]
			statsVar <- unname(unique(unlist(lapply(stats, function(x)
				if(!is.null(statsVarBy))	lapply(x, getStatColName)	else	getStatColName(x)))
			))
		}else{
			statsVarByUsed <- statsVarBy
			statsVar <- if(!is.null(statsVarBy)){
				unname(unique(unlist(lapply(stats, getStatColName))))
			}else	getStatColName(stats)
		}
#		statsVar <- unname(unique(unlist(lapply(unlist(stats, recursive = FALSE), getStatColName))))
		
		summaryTable <- ddply(summaryTable, statsVarByUsed, function(x){
			statsX <- if("variableInit" %in% statsVarByUsed){
				stats[[unique(x$variableInit)]]
			}else stats
			if(!is.null(statsVarBy))
				statsX <- statsX[[as.character(unique(x[, statsVarBy]))]]
			if(is.null(statsX)){
				if(!"isTotal" %in% colnames(x) && all(x$isTotal))
					warning("'stats' missing for:", toString(unique(x[, statsVarByUsed])))
				x
			}else	addStats(sumTable = x, stats = statsX)
		})
		
	}else	statsVar <- statsVarInit
	
	res <- list(
		summaryTable = summaryTable,
		statsVar = statsVar	
	)

}
