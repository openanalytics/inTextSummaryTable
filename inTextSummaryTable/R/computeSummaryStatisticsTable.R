#' Compute summary statistics for a specific dataset and variables of interest
#' @param rowOrder Specify how the rows should be ordered in the final table, either a:
#' \itemize{
#' \item{String among:
#' \itemize{
#' \item{'auto' (by default): if the variable is a factor, keep its order, otherwise order alphabetically}
#' \item{'alphabetical': order alphabetically}
#' \item{'total': order rows in decreasing order of the total number of subjects
#' across all columns for this specific category.}
#' }}
#' \item{Function with input the summary table and output the ordered elements of the \code{rowVar}}
#' }
#' To specify different ordering methods for different \code{rowVar}, specify a list
#' of such elements, named with the \code{rowVar} variable.
#' For the table output of \code{\link{computeSummaryStatisticsTable}} (long format),
#' this order is also reflected in the \strong{\code{levels}} of the row factor variable.
#' @param rowOrderTotalFilterFct Function used to filter the data used to order the rows
#' based on total counts (in case \code{rowOrder} is 'total'),
#' To order rows based on one specific column category,
#' e.g. to order based on the counts in the treatment column:
#' function(x) subset(x, TRTP == "treatmentX")
#' @param rowOrderCatLast String with category to be printed in the last 
#' row of each \code{rowVar} (if any, set to NULL if none). 
#' @param rowVarTotalByVar Character vector with a row variable
#' used to categorize the row total.\cr
#' Note that this is only used if row total(s) is/are requested via \code{rowVarTotalInclude},
#' and this variable should also be included in \code{rowVar}.
#' This can be specified also for a specific row variable if the vector is named.\cr
#' For example: \code{c(ADECOD = "AESEV")} to compute total by severity 
#' for row adverse event term in a typical adverse event count table 
#' (by System Organ Class and Adverse Event Term).
#' @param varGeneralLab String with general label for variable specified in \code{var}.
#' In case of multiple variable in \code{var}, this will be included in the table header
#' (see 'rowVarLab' attribute of the output).
#' @param varSubgroupLab String with general label for sub-group of
#' categorical variable(s) for count table, 'Variable group' by default.
#' This will be included in the final table header (see 'rowVarLab' attribute of the output).
#' @param varIgnore Vector with elements to ignore in the \code{var} variable(s).
#' The \code{data} records with such elements in \code{var} are \strong{filtered} from the data 
#' at the start of the workflow.
#' @param dataTotal Data.frame used to extract the Total number of subject
#' per column in column header ('N = [X]').
#' It should contain the variables specified by \code{colVarTotal}.
#' If not specified, the total number of subjects is extracted from the \code{data}.
#' @param dataTotalPerc Data.frame used to extract the total counts per column 
#' for the computation of the percentage.\cr
#' By default, \code{dataTotal} is used.\cr
#' It should contain the variables specified by \code{colVarTotalPerc}.
#' @param dataTotalRow Data.frame used to extract the total count across all
#' elements of the row
#' variable, list of such data.frame for each \code{rowVar} variable.\cr
#' If the dataset is specified by row variable, the list should be named with:
#' variable X if the total across elements of variable X should be included.
#' By default, \code{data} is used.
#' @param dataTotalCol Data.frame from which the total across columns is 
#' extracted (in case \code{colTotalInclude} is TRUE)
#' or list of such data.frame for each \code{rowVar} variable.\cr
#' If the dataset is specified by row variable, the list should be named with:
#' with:
#' \itemize{
#' \item{last row variable: for the dataset used in the total column for 
#' the most nested row variable}
#' \item{higher row variable (X+1): for the dataset used for the total column
#' and row total of X}
#' \item{'total': for the dataset used for the total column and general row total}
#' }
#' If only a subset of the variables is specified in this list, 
#' \code{data} is used for the remaining variable(s) (or 'total') if needed.\cr
#' This dataset (the one for 'total' if a list) is also used for:
#' \itemize{
#' \item{the header of the total column in case \code{dataTotal} is
#' not specified}
#' \item{the denominator of the percentages in the total column
#' in case \code{dataTotalPerc} is not specified}
#' }
#' By default, \code{data} is used.
#' @param filterFct (optional) Function taking as input
#' the summary table with computed statistics and returning a subset 
#' of the summary table.\cr
#' Note: The filtering function should also handle records with :
#' \itemize{
#' \item{total for the column header: \code{isTotal} set to TRUE,
#' and \code{colVar}/\code{rowVar} is NA.\cr
#' For example: \code{filterFct = function(data) subset(data, isTotal & myColVar == "group 1")}
#' }
#' \item{\code{rowVar}/\code{colVar} set to 'Total'/\code{colTotalLab} 
#' if \code{rowVarTotalInclude}/\code{colTotalInclude} is specified}
#' }
#' @param colVarTotal String with column(s) considered to compute the total by,
#' reported in the header of the table, by default same as \code{colVar}.
#' Use: 'variable' to compute total by \code{var} (if multiple).
#' @param colVarTotalPerc String with column(s) considered to compute the total by,
#' used as denominator for the percentage computation, by default same as \code{colVarTotal}.
#' Use: 'variable' to compute total by \code{var} (if multiple).
#' @param rowVarTotalPerc Character vector with row variables by which the total
#' should be computed for the denominator for the percentage computation.
#' By default the total is only computed only by column (NULL by default).
#' If the total should be based on the total number of records per variable,
#' \code{rowVarTotalPerc} should be set to 'variable'.
#' @param byVar Variable(s) of \code{data} for which separated table(s)
#' should be created.
#' @param byVarLab String with label for \code{byVar}, used to set the names
#' of the output list of table(s).
#' @param statsPerc String with 'base statistical variable' used to compute the 
#' percentage, either: 
#' \itemize{
#' \item{'statN' (by default): the number of subjects}
#' \item{'statm': the number of records}
#' }
#' @param stats (optional) Statistic(s) of interest to compute, either:
#' \itemize{
#' \item{string with the name of a default set of statistics
#' available in the package, 
#' see section 'Formatted statistics' in
#' \code{\link[=inTextSummaryTable-stats]{in-text table statistics}}.\cr
#' See the corresponding \code{type} parameter of the
#' \code{\link{getStatsData}} for more information
#' on how the statistic is internally extracted.}
#' \item{(expert mode) named list of language object (see \code{\link{is.language}}) 
#' of base summary statistics of interest, see section:
#' 'Base statistics' in
#' \code{\link[=inTextSummaryTable-stats]{in-text table statistics}}.\cr
#' The names are reported in the header.\cr
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.\cr
#' The statistics can be specified separately:
#' \itemize{
#' \item{for each \code{var} (if multiple), 
#' by naming each element of the list:
#' \code{list(varName1 = list(...), varName2 = list())}
#' }
#' \item{and/or for each element in:
#' \code{statsVarBy}, by naming each sublist.}
#' }}
#' }
#' @param statsGeneralLab String with general label for statistics, 'Statistic' by default.
#' Only included if no \code{statsVar} if longer than 1.
#' @param statsVarBy String with variable in \code{rowVar}/\code{colVar}
#' which the statistics should be computed by.\cr
#' In this case, \code{stats} (nested list or not) should be additionally nested
#' to specify the statistics for each element in \code{statsVarBy}.
#' @param varIncludeTotal This argument is deprecated, please use: 'varTotalInclude' instead.
#' @param varTotalInSepRow Logical, should the total per variable be included in
#' a separated row (by default) or in the row containing the header of the variable?
#' @inheritParams inTextSummaryTable-common-args
#' @inheritParams computeSummaryStatisticsByRowColVar
#' @return An object \code{\link{summaryTable}}
#' or list of such objects if
#' \code{byVar} is specified.
#' @author Laure Cougnaud
#' @importFrom plyr ddply rbind.fill dlply
#' @importFrom clinUtils getLabelVar
#' @export
computeSummaryStatisticsTable <- function(
	data,  
	var = NULL, varFlag = NULL, varInclude0 = FALSE,
	varLab = NULL,
	varLabInclude = length(var) > 1,
	varGeneralLab = "Variable", varSubgroupLab = "Variable group",
	varIgnore = NULL,
	varIncludeTotal = FALSE,
	varTotalInclude = FALSE,
	varTotalInSepRow = FALSE,
	colVar = NULL, colVarDataLevels = NULL, 
	colVarTotal = colVar, 
	colVarTotalPerc = colVarTotal,
	colTotalInclude = FALSE, colTotalLab = "Total",
	colInclude0 = FALSE,
	rowVar = NULL, rowVarDataLevels = NULL, 
	rowVarLab = NULL,
	rowOrder = "auto", rowOrderTotalFilterFct = NULL, rowOrderCatLast = NULL,
	rowVarTotalInclude = NULL,
	rowVarTotalInSepRow = NULL,
	rowVarTotalByVar = NULL,
	rowVarTotalPerc = NULL,
	rowInclude0 = FALSE, 
	type = "auto",
	subjectVar = "USUBJID",	
	dataTotal = NULL, dataTotalPerc = dataTotal,
	dataTotalRow = NULL,
	dataTotalCol = NULL,
	stats = NULL, 
	statsVarBy = NULL,
	statsExtra = NULL,
	statsGeneralLab = "Statistic",
	statsPerc = c("statN", "statm"),
	filterFct = NULL,
	labelVars = NULL,
	byVar = NULL, byVarLab = NULL,
	checkVarDiffBySubj = "error"
){
	
	inputParams <- as.list(environment())
	
	statsPerc <- match.arg(statsPerc)
	
	if(!inherits(data, "data.frame"))
		stop("A data.frame should be specified in 'data'.")
	
	if(class(data)[1] != "data.frame")
		data <- as.data.frame(data)
	
	if(nrow(data) == 0){
		message("No data to report.")
		return(invisible())
	}

	## compute multiple statistics table based on 'byVar'
	byVar <- checkVar(var = byVar, varLabel = "byVar", data = data)
	if(!is.null(byVar)){
		
		byVarLab <- getLabelVar(byVar, data = data, labelVars = labelVars, label = byVarLab)
		
		res <- dlply(data, byVar, function(dataBy){
			inputParamsBy <- inputParams
			inputParamsBy$data <- dataBy
			inputParamsBy$byVar <- NULL
			do.call(computeSummaryStatisticsTable, inputParamsBy)		
		})	
		uniqueNameDf <- unique(data[, byVar, drop = FALSE])
		newName <- do.call(paste, 
			c(mapply(paste, byVarLab[byVar], uniqueNameDf, sep = ": ", SIMPLIFY = FALSE),
			sep = "\n")
		)
		initName <- do.call(paste, c(uniqueNameDf, sep = "."))
		names(res) <- newName[match(names(res), initName)]
			
		return(res)
			
	}
	
	# get default set of statistics
	if(is.character(stats) && length(stats) == 1){
		stats <- getStatsData(data = data, var = var, type = stats)
		# if only one variable is specified, 'stats' should not be named:
		if(!is.null(var) && length(var) == 1)	stats <- stats[[var]]
	}
	
	# check if 'varIncludeTotal' is not default
	if(!(is.logical(varIncludeTotal) && length(varIncludeTotal) == 1 && !varIncludeTotal)){
		warning("Argument: 'varIncludeTotal' is deprecated, please use 'varTotalInclude' instead.")
		varTotalInclude <- varIncludeTotal
	}
	
	# check var
	var <- checkVar(var = var, varLabel = "var", data = data, varUncheck = "all")
	varFlag <- checkVar(var = varFlag, varLabel = "varFlag", varRef = var, refLabel = "var")
	varInclude0 <- checkVar(
		var = varInclude0, varLabel = "varInclude0", 
		varRef = var, refLabel = "var",
		varUncheck = c(TRUE, FALSE)
	)
	varLabInclude <- checkVarLabInclude(var = var, varLabInclude = varLabInclude)
	varTotalInclude <- checkVar(var = varTotalInclude, varLabel = "varTotalInclude", varRef = var, refLabel = "var", varUncheck = c(TRUE, FALSE))
	
	# check row var parameters:
	rowVar <- checkVar(var = rowVar, varLabel = "rowVar", data = data)
	rowVarTotalInclude <- checkVar(var = rowVarTotalInclude, varLabel = "rowVarTotalInclude", varRef = rowVar, refLabel = "rowVar")
	rowVarTotalInSepRow <- checkVar(var = rowVarTotalInSepRow, varLabel = "rowVarTotalInSepRow", varRef = rowVarTotalInclude, refLabel = "rowVarTotalInclude")
	if(!varLabInclude & "variable" %in% rowVarTotalPerc){
		warning("Percentages cannot be computed by 'variable' because variable not included in the table (varLabInclude is FALSE).")
		rowVarTotalPerc <- setdiff(rowVarTotalPerc, "variable")
	}
	rowVarTotalPerc <- checkVar(var = rowVarTotalPerc, varLabel = "rowVarTotalPerc", varRef = rowVar, refLabel = "rowVar", varUncheck = "variable")
	
	# in case the variable should be in multiple columns, 'colVar' might include: 'variable'
	colVarInit <- colVar <- checkVar(var = colVar, varLabel = "colVar", data = data, varUncheck = "variable")
	if("variable" %in% colVar){
		if(!(!is.null(var) && varLabInclude))
			warning(paste(
				"'var' not included in columns because",
				"'varLabInclude' is FALSE.",
				"You might want to set: 'statsLayout' to 'col'."
			))
	}
	colVar <- setdiff(colVar, "variable")
	if(length(colVar) == 0)	colVar <- NULL
	colVarTotal <- checkVar(var = colVarTotal, varLabel = "colVarTotal", varRef = colVar, refLabel = "colVar", varUncheck = "variable")
	colVarTotalPerc <- checkVar(var = colVarTotalPerc, varLabel = "colVarTotalPerc", varRef = colVar, refLabel = "colVar", varUncheck = "variable")

	# check variable(s) label
	varLab <- getLabelVar(var, data = data, labelVars = labelVars, label = varLab)
	rowVarLab <- getLabelVar(rowVar,  data = data, labelVars = labelVars, label = rowVarLab)
	if(is.null(varGeneralLab) || !is.character(varGeneralLab) ||length(varGeneralLab) > 1){
		warning("'varGeneralLab' should be a character of length 1, it is set to 'Variable' by default.")
		varGeneralLab <- "Variable"
	}
	if(is.null(varSubgroupLab) || !is.character(varSubgroupLab) ||length(varSubgroupLab) > 1){
		warning("'varSubgroupLab' should be a character of length 1, it is set to 'Variable group' by default.")
		varSubgroupLab <- "Variable group"
	}
	
	# check stats params:
	statsVarBy <- checkVar(
		var = statsVarBy , varLabel = "statsVarBy",
		varRef = c(colVar, rowVar), refLabel = "row or column variables",
		msgType = "error"
	)
	
	# Compute the column total in case:
	# - the rows should be ordered based on the total category
	# - total should be extracted within a function specified in rowOrder
	# - excepted when no column variable is specified
	colTotalIncludeInit <- colTotalInclude
	if(is.null(colVar)){
		if(colTotalIncludeInit)	
			warning("Column 'total' is not included because no column variable is specified.")
		colTotalInclude <- FALSE
	}else{
		check <- sapply(rowOrder, function(x) !(is.character(x) && x != "total"))
		if(any(check))	colTotalInclude <- TRUE
	}
	
#	if(!is.null(colVar))
#	checkIfTotal <- function(x)	!is.function(x) && any(x == "total")
#	if(!colTotalInclude && any(sapply(rowOrder, checkIfTotal)))
#		colTotalInclude <- TRUE
	
	# ignore certain elements
	if(!is.null(var) && !is.null(varIgnore))
		for(varI in var){
			if(varI != "all")
				data <- data[!data[, varI] %in% varIgnore, ]
		}
	
	# for flag variable:
	if(!is.null(varFlag)){
		
		# convert them to a format to only retain flagged records
		data[, varFlag] <- colwise(convertVarFlag)(data[, varFlag, drop = FALSE])
		
		postProcessVarFlagTable <- function(summaryTable)
			postProcessVarFlag(summaryTable = summaryTable, varFlag = varFlag)
		filterFct <- c(filterFct, list(postProcessVarFlagTable))
		
	}
	
	# convert row/column variables to factor
	data <- convertVarRowVarColVarToFactor(
		data = data, rowVar = rowVar, colVar = colVar, var = var
	)
		
	# get general statistics for each combination of rowVar/colVar
	summaryTable <- computeSummaryStatisticsByRowColVar(
		data = data, 
		var = var, varLab = varLab, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
		varLabInclude = varLabInclude,
		statsExtra = statsExtra,
		type = type,
		rowVar = rowVar, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
		colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar, labelVars = labelVars,
		checkVarDiffBySubj = checkVarDiffBySubj
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

		# convert row/column variables to factor
		dataForColTotal <- convertVarRowVarColVarToFactor(
			data = dataForColTotal, 
			rowVar = rowVar, 
			colVar = NULL,
			var = var
		)
		
		summaryTableColTotal <- computeSummaryStatisticsByRowColVar(
			data = dataForColTotal, 
			var = var, varLab = varLab, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
			varLabInclude = varLabInclude,
			statsExtra = statsExtra,
			type = type,
			rowVar = rowVar, rowInclude0 = rowInclude0,	rowVarDataLevels = rowVarDataLevels,
			subjectVar = subjectVar, labelVars = labelVars,
			msgLabel = "total column",
			checkVarDiffBySubj = checkVarDiffBySubj
		)
		
		if(nrow(summaryTableColTotal) > 0)
			summaryTableColTotal[, colVar] <- colTotalLab
			
		summaryTable <- rbind.fill(summaryTable, summaryTableColTotal)
		summaryTable[, colVar] <- lapply(colVar, function(x)
			factor(summaryTable[, x], levels = unique(c(colVarLevels[[x]], colTotalLab)))
		)
		
		# extract the data used for the total header + percentage:
		dataForColTotalPercHeader <- if(!is.null(dataTotalCol)){
			# different datasets for the different row variables:
			if(is.list(dataTotalCol) && !is.data.frame(dataTotalCol)){ 
				if(!is.null(rowVar) && 'total' %in% names(dataTotalCol)){
					dataTotalCol[["total"]]
				}else	data
				# one unique df:
			}else	dataTotalCol
		}else	data
	
		# convert row/column variables to factor
		dataForColTotalPercHeader <- convertVarRowVarColVarToFactor(
			data = dataForColTotalPercHeader, 
			rowVar = rowVar, 
			colVar = NULL,
			var = var
		)
		
	}
	
	if(!is.null(rowVarTotalInclude)){
		
		# order specified variables as in rowVar
		rowVarTotalInclude <- intersect(rowVar, rowVarTotalInclude)
		
		# compute sub-total for each specified rowVar (excepted the last one)
		summaryTableRowSubtotal <- data.frame()
		for(rVST in rowVarTotalInclude){
			
			dataForSubTotal <- if(!is.null(dataTotalRow)){
				if(is.data.frame(dataTotalRow)){
					dataTotalRow
				}else{
					if(rVST %in% names(dataTotalRow)){
						dataTotalRow[[rVST]]
					}else	stop(paste0("Dataset for row total missing for: ", shQuote(rVST), "."))
				}
			}else	data
	
			# remove rows which have NA for the nested sub-variable
			# otherwise have summary statistics are duplicated (sub-total and initial)
			rowVarSubTotalOther <- rowVar[
				setdiff(seq_along(rowVar), seq_len(match(rVST, rowVar)))
			]
			filterRowNestedVar <- function(data){
				if(length(rowVarSubTotalOther) > 0){
					
					checkVar(var = rowVarSubTotalOther, "rowVar", data = data, 
						refLabel = "dataset for row total", msgType = "error")
					
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
			rowVarTotalByVarI <- if(!is.null(rowVarTotalByVar)){
				if(!is.null(names(rowVarTotalByVar))){
					if(rVST %in% names(rowVarTotalByVar))
						rowVarTotalByVar[rVST]
				}else	rowVarTotalByVar
			}
			rowVarTotalByVarI <- checkVar(
				var = rowVarTotalByVarI, 
				varLabel = "rowVarTotalByVar",
				varRef = rowVar,
				refLabel = "rowVar"
			)
			rowVarOther <- c(rowVarOther, rowVarTotalByVarI)
			
			checkVar(
				var = c(rowVarOther, colVar, var), "variables", data = dataForSubTotal, 
				refLabel = "dataset for row total", msgType = "error",
				varUncheck = "all"
			)
			
			# convert row/column variables to factor
			dataForSubTotal <- convertVarRowVarColVarToFactor(
				data = dataForSubTotal, 
				rowVar = rowVarOther, 
				colVar = colVar,
				var = var
			)
			
			# compute statistics
			summaryTableRowSubtotalVar <- computeSummaryStatisticsByRowColVar(
				data = dataForSubTotal, 
				var = var, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
				varLabInclude = varLabInclude,
				statsExtra = statsExtra,
				type = type,
				rowVar = rowVarOther, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
				colVar = colVar, colInclude0 = colInclude0, colVarDataLevels = colVarDataLevels,
				subjectVar = subjectVar, varLab = varLab, labelVars = labelVars,
				msgLabel = paste("row total for", rVST),
				checkVarDiffBySubj = checkVarDiffBySubj
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
		
				# convert row/column variables to factor
				dataForSubTotalForColTotal <- convertVarRowVarColVarToFactor(
					data = dataForSubTotalForColTotal, 
					rowVar = rowVarOther, 
					colVar = NULL,
					var = var
				)
				
				checkVar(var = c(rowVarOther, var), "variables", data = dataForSubTotalForColTotal, 
					refLabel = "dataset for row total across columns", msgType = "error")
				
				summaryTableRowSubtotalVarColTotal <- computeSummaryStatisticsByRowColVar(
					data = dataForSubTotalForColTotal, 
					var = var, varTotalInclude = varTotalInclude, varInclude0 = varInclude0,
					varLabInclude = varLabInclude,
					statsExtra = statsExtra,
					type = type,
					rowVar = rowVarOther, rowInclude0 = rowInclude0, rowVarDataLevels = rowVarDataLevels,
					subjectVar = subjectVar, varLab = varLab, labelVars = labelVars,
					msgLabel = paste("total column for the row total for", rVST),
					checkVarDiffBySubj = checkVarDiffBySubj
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

	isDataTotalSpec <- !is.null(dataTotal)
	
	# by default, if not specified, data is used
	if(!isDataTotalSpec){
		dataTotal <- data
		# in case no 'dataTotal' is included, consider 'dataTotalCol' for the header across columns
		dataTotalColTotalHeader <- if(colTotalInclude)	dataForColTotalPercHeader
	}
	# check if colVarTotal is in dataTotal:
	colVarTotal <- checkVar(var = colVarTotal, varLabel = "colVarTotal", 
		data = dataTotal, refLabel = "total dataset", varUncheck = "variable")

	# format columns as factor in correct order
	if(isDataTotalSpec){
		# to have specified order for colVar in case different order 'dataTotal'
		colVarTotalUsed <- setdiff(colVarTotal, "variable")
		if(!is.null(colVarTotalUsed)){
			dataTotal[, colVarTotalUsed] <- lapply(colVarTotalUsed, function(var)
				if(is.factor(summaryTable[, var])){
					factor(dataTotal[, var], levels = colVarLevels[[var]])
				}else dataTotal[, var]
			)
		}
		dataTotalColTotalHeader <- dataTotal
	}
	
	# get total for column headers:

	# convert row/column variables to factor
	dataTotal <- convertVarRowVarColVarToFactor(
		data = dataTotal, 
		rowVar = NULL, 
		colVar = colVar,
		var = var
	)
	summaryTableTotal <- computeSummaryStatisticsTableTotal(
		data = dataTotal, 
		colVar = colVar, colVarTotal = colVarTotal,
		colTotalLab = colTotalLab,
		colInclude0 = colInclude0, 
		colTotalInclude = colTotalInclude, dataTotalCol = dataTotalColTotalHeader,
		colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
		subjectVar = subjectVar,
		# not used:
		var = var, varLab = varLab, labelVars = labelVars,
		msgLabel = "header total",
		checkVarDiffBySubj = checkVarDiffBySubj
	)
	
	# save total or not in the 'isTotal' column
	summaryTableTotal$isTotal <- TRUE
	if(nrow(summaryTable) > 0)
		summaryTable$isTotal <- FALSE
	# bind to the summary table
	summaryTable <- rbind.fill(summaryTable, summaryTableTotal)
	
	## compute percentages
	
	# get counts (for percentage computation)
	if(!all(colVarTotalPerc %in% colnames(summaryTable)))
		stop("'colVarTotalPerc' are not in the computed summary statistics table.")
	computeTotalPerc <- 
		!setequal(colVarTotal, colVarTotalPerc) | 
		!is.null(rowVarTotalPerc) | 
		!is.null(dataTotalPerc)

	if(is.null(dataTotalPerc)){
		dataTotalPerc <- dataTotal
		dataTotalPercTotalHeader <- dataTotalColTotalHeader
	}else{
		# convert row/column variables to factor
		dataTotalPerc <- convertVarRowVarColVarToFactor(
			data = dataTotalPerc, 
			rowVar = NULL, 
			colVar = colVar,
			var = var
		)
		dataTotalPercTotalHeader <- dataTotalPerc
	}
	
	# check if colVarTotal is in dataTotal:
	colVarTotalPerc <- checkVar(
		var = colVarTotalPerc, 
		varLabel = "colVarTotalPerc", 
		data = dataTotalPerc, 
		refLabel = "total dataset for percentage", 
		varUncheck = "variable"
	)

	summaryTableTotalPerc <- if(computeTotalPerc){

		computeSummaryStatisticsTableTotal(
			data = dataTotalPerc, dataTotalCol = dataTotalPercTotalHeader,
			colVar = colVar, colVarTotal = colVarTotalPerc,
			colTotalLab = colTotalLab,
			colInclude0 = colInclude0, colTotalInclude = colTotalInclude,
			colVarDataLevels = colVarDataLevels, colVarLevels = colVarLevels,
			rowVarTotal = rowVarTotalPerc, 
			var = var, varLab = varLab, labelVars = labelVars,
			subjectVar = subjectVar,
			msgLabel = "total for percentage",
			checkVarDiffBySubj = checkVarDiffBySubj
		)
		
	}else	summaryTableTotal
	summaryTable <- rbind.fill(
		cbind(summaryTable, isTotalPerc = FALSE), 
		cbind(summaryTableTotalPerc, isTotalPerc = TRUE)
	)
	
	# compute percentages
	statPercCols <- c(
		total = sub("stat(.+)", "statPercTotal\\1", statsPerc),
		perc = sub("stat(.+)", "statPerc\\1", statsPerc)
	)
	summaryTable <- ddply(summaryTable, c(rowVarTotalPerc, colVarTotalPerc), function(x){
		idxTotalPerc <- which(x$isTotalPerc)
		if(length(idxTotalPerc) > 0){
			if(length(idxTotalPerc) != 1)
				stop("Multiple total records for the percentage computation.")
			statPercTotal <- x[idxTotalPerc, statsPerc]
			dataPercTotal <- list(statPercTotal, x[, statsPerc]/statPercTotal*100)
			names(dataPercTotal) <- c(statPercCols["total"], statPercCols["perc"])
			res <- cbind(x, dataPercTotal)
			res[-idxTotalPerc, ]
		}else{
			dataPercTotal <- setNames(
				list(NA_integer_, NA_real_), 
				statPercCols[c("total", "perc")]
			)
			cbind(x, dataPercTotal)
		}
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
	
	# remove the rows with total if should be ordered by total 
	# but the total not included
	if(!colTotalIncludeInit & colTotalInclude){
		
		idxColTotal <- which(rowSums(summaryTable[, colVar, drop = FALSE] == colTotalLab) > 0)
		if(length(idxColTotal) > 0)
			summaryTable <- summaryTable[-idxColTotal, ]
		removeTotalLevel <- function(x){
			if(is.factor(x) && colTotalLab %in% levels(x)){
				factor(x, levels = setdiff(levels(x), colTotalLab))
			}else	x
		}
		summaryTable[, colVar] <- colwise(removeTotalLevel)(summaryTable[, colVar, drop = FALSE])
	
	}
	
	# if only flag variables, remove 'variableGroup'
	# (otherwise empty line when not specifying 'stats')
	if("variableGroup" %in% colnames(summaryTable) && 
		length(var) > 0 &&
		length(setdiff(var, varFlag)) == 0){
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
			if(varLabInclude & !"variable" %in% colVarInit)	"variable", 
			# in case only one variable, but still count
			if("variableGroup" %in% colnames(summaryTable))	"variableGroup"
		),
		rowVarLab = c(
			rowVarLab, 
			if(varLabInclude & !"variable" %in% colVarInit)	
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
		colVar = colVarInit,
		colTotalLab = colTotalLab

	)
	
	attributes(summaryTable) <- c(attributes(summaryTable), list(summaryTable = attrTable))
	
	class(summaryTable) <- c("summaryTable", class(summaryTable))
	
	return(summaryTable)
	
}

#' Compute summary statistics total table.
#' @param colVarTotal Character vector with column(s) considered to compute the total.
#' This could also contain 'variable'.
#' @param rowVarTotal Character vector with row(s) considered to compute the total.
#' This could also contain 'variable'.
#' @param colVarLevels list with levels of each \code{colVar}
#' @param dataTotalCol Data.frame from which the total across columns is 
#' extracted (in case \code{colTotalInclude} is TRUE).
#' @param msgLabel (optional) String with label for the data ('total' by default), 
#' included in the message/warning for checks.
#' @inheritParams inTextSummaryTable-common-args
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
	labelVars = NULL,
	msgLabel = "total",
	checkVarDiffBySubj = "error"){

	# in case total should be computed by 'var'
	# convert wide -> long format: one column with all variables
	formatDataTotalWithVar <- function(data){
		
		if("all" %in% var)	data[, "all"] <- "all"
		
		# to avoid warnings in 'melt': 
		# - 'attributes are not identical across measure variables; they will be dropped'
		# - cannot avoid coercion of factors when measure attributes not identical
		if(!is.null(var))
			data[, var] <- lapply(data[, var, drop = FALSE], as.character)
		
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
	if("variable" %in% c(rowVarTotal, colVarTotal)){
		data <- formatDataTotalWithVar(data)
	}
	
	# counts by elements in colVar
	summaryTableTotal <- computeSummaryStatisticsByRowColVar(
		data = data, 
		type = "countTable", 
		colVar = colVarTotal, 
		rowVar = rowVarTotal,
		colInclude0 = colInclude0, 
		colVarDataLevels = colVarDataLevels,
		subjectVar = subjectVar,
		msgLabel = msgLabel,
		checkVarDiffBySubj = checkVarDiffBySubj
	)
	
	# counts across all elements of colVar
	if(colTotalInclude){
		colVarTotalTI <- setdiff(colVarTotal, colVar)
		if(length(colVarTotalTI) == 0)	colVarTotalTI <- NULL
		if(is.null(dataTotalCol))	dataTotalCol <- data
		
		if("variable" %in% c(rowVarTotal, colVarTotal))
			dataTotalCol <- formatDataTotalWithVar(dataTotalCol)
		
		summaryTableTotalCol <- computeSummaryStatisticsByRowColVar(
			data = dataTotalCol, 
			type = "countTable", 
			colVar = colVarTotalTI,
			rowVar = rowVarTotal,
			subjectVar = subjectVar,
			msgLabel = paste0("total column", 
				if(!is.null(msgLabel))	paste(" for the ", msgLabel)
			),
			checkVarDiffBySubj = checkVarDiffBySubj
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
#' @param rowInclude0 Logical, if TRUE (FALSE by default),
#' include rows with no records, based on all combinations 
#' of the \code{rowVar} (assuming nested variable(s)).
#' @param varInclude0 Logical, should rows with no counts 
#' for the count \code{var} or \code{varFlag} variable(s)
#' be included in the table?
#' Either:
#' \itemize{
#' \item{logical of length 1, if TRUE (FALSE by default) 
#' rows with no count are included for all \code{var}
#' }
#' \item{a character vector containing categorical \code{var} 
#' for which zero counts rows should be included}
#' }
#' @param colInclude0 Logical, if TRUE (FALSE by default),
#' include columns with no records, based on all combinations 
#' of the \code{columnVar} (assuming nested variable(s)).
#' If variable(s) are not nested, possible combinations
#' can be specified via \code{colVarDataLevels}.
#' @param varLab Named character vector with label for each variable 
#' specified in \code{var}.
#' By default, extracted from the \code{labelVars}.
#' if not available, \code{var} is used.
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
#' Only used for categorical variables (and \code{var} not 'all').
#' Either:
#' \itemize{
#' \item{logical of length 1, if TRUE (FALSE by default) include the total for all categorical \code{var}}
#' \item{a character vector containing categorical \code{var} for which the total should be included}
#' }
#' @param checkVarDiffBySubj String, 'error' (default), 'warning',
#' or 'none'.  
#' Should an error, a warning, or nothing be produced
#' if a continuous variable (\code{var}) contains
#' different values for the same subject (by row/column)?
#' @inheritParams inTextSummaryTable-common-args
#' @return data.frame of class 'countTable' or 'summaryTable',
#' depending on the 'type' parameter; with statistics in columns,
#' either if \code{type} is:
#' \itemize{
#' \item{'summaryTable': 
#' \itemize{
#' \item{'N': number of subjects}
#' \item{'Mean': mean of \code{var}}
#' \item{'SD': standard deviation of \code{var}}
#' \item{'SE': standard error of \code{var}}
#' \item{'Median': median of \code{var}}
#' \item{'Min': minimum of \code{var}}
#' \item{'Max': maximum of \code{var}}
#' \item{'m': number of records}
#' }
#' }
#' \item{'countTable': 
#' \itemize{
#' \item{'N': number of subjects}
#' \item{'m': number of records}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom clinUtils getLabelVar
#' @keywords internal
computeSummaryStatisticsByRowColVar <- function(
	data, 
	var = NULL, varLab = getLabelVar(var = var, data = data, labelVars = labelVars), varInclude0 = FALSE,
	varLabInclude = length(var) > 1,
	varTotalInclude = FALSE,
	type = "auto",
	rowVar = NULL, rowInclude0 = FALSE, rowVarDataLevels = NULL,
	colVar = NULL, colInclude0 = FALSE, colVarDataLevels = NULL,
	subjectVar = "USUBJID",
	labelVars = NULL,
	statsExtra = NULL,
	msgLabel = NULL,
	checkVarDiffBySubj = "error"){

	if(is.logical(varTotalInclude) && length(varTotalInclude) > 1)
		stop("If 'varTotalInclude' if a logical, it should be of length 1.")
	
	varLabInclude <- checkVarLabInclude(var = var, varLabInclude = varLabInclude)

	computeSummaryStatisticsCustom <- function(...)
		computeSummaryStatistics(..., 
			subjectVar = subjectVar, 
			statsExtra = statsExtra,
			msgLabel = msgLabel,
			checkVarDiffBySubj = checkVarDiffBySubj
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
				varTotalInclude = varTotalInclude,
				msgVars = groupVar
			)
		}else{
			summaryTableVarList <- lapply(var, function(varI){
				varITotalInclude <- 
					(is.logical(varTotalInclude) && varTotalInclude) || 
					varI %in% varTotalInclude
				varIInclude0 <- 
					(is.logical(varInclude0) && varInclude0) || 
					varI %in% varInclude0
				sumTable <- computeSummaryStatisticsCustom(
					data = x, 
					var = varI, 
					type = type,
					varTotalInclude = varITotalInclude,
					filterEmptyVar = !varIInclude0,
					msgVars = groupVar
				)
				# only store the variable if more than one specified variable
				if(!is.null(sumTable) && nrow(sumTable) > 0 && varLabInclude){
					cbind.data.frame(variableInit = varI, sumTable, stringsAsFactors = FALSE)
				}else sumTable
				
			})
	
			summaryTable <- do.call(rbind.fill, summaryTableVarList)
			# if multiple variable(s), sort 'variable' in order specified in input
			if(!is.null(summaryTable) && varLabInclude){
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


#' Convert a variable to a factor with levels in specified order.
#' @param var String with variable of \code{data} to sort.
#' @param otherVars Character vector with other variable(s) of \code{data}
#' considered in the specific dimension.
#' @param method Ordering method to use, either:
#' \itemize{
#' \item{String among:
#' \itemize{
#' \item{'auto': if \code{var} is a factor, keep its order, otherwise alphabetically}
#' \item{'alphabetical': \code{var} is order in alphabetical order}
#' \item{'total': \code{var} is ordered based on the \code{totalVar} variable, in decreasing order.
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
#' Set to NULL if no specific category should be included as last element.
#' @inheritParams inTextSummaryTable-common-args
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
	catLast = NULL){

	if(!is.function(method)){
		
		method <- match.arg(method)
	
		res <- switch(method,
			'auto' = if(is.factor(data[, var])){
				varLevels <- levels(data[, var])
				varLevels <- c(
					setdiff(varLevels, catLast),
					intersect(catLast, varLevels)
				)
				res <- factor(data[, var], levels = varLevels)
			}else{
				convertVarToFactorWithOrder(
					data = data, var = var, catLast = catLast,
					method = "alphabetical"
				)
			},
			'alphabetical' = {
				varLevels <- c(
					intersect("Total", data[, var]),
					sort(setdiff(unique(as.character(data[, var])), c(catLast, "Total")), decreasing = FALSE),
					intersect(catLast, data[, var])
				)
				factor(data[, var], levels = varLevels)
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
							if(length(idxTotal) != 0){
								dataForTotal <- dataForTotal[idxTotal, ]
							}else	warning("Total across columns not available to order the rows in the summary table.")
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
						intersect("Total",  data[, var]), 
						setdiff(names(sort(totalPerVar, decreasing = TRUE)), c("Total", catLast)),
						intersect(catLast, data[, var])
					)
					varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
					factor(data[, var], levels = varLevels)
					
			}
		)
		
	}else{
		
		varLevels <- as.character(method(data))
		varLevels <- c(varLevels, setdiff(as.character(unique(data[, var])), varLevels))
		if("Total" %in% varLevels)	varLevels <- c("Total", setdiff(varLevels, "Total"))
		varLevels <- c(
			setdiff(varLevels, catLast),
			intersect(catLast, varLevels)
		)
		res <- factor(data[, var], levels = varLevels)
		
	}
	
	return(res)
		
}

#' Compute custom statistics specified by the user.
#' @param summaryTable Summary table.
#' @param statsVarInit Character vector with initial statistics names.
#' @param stats (Optionally) named list of expression or call object of summary statistics of interest.
#' The names are reported in the header.
#' The following variables are recognized, if the table is a: 
#' \itemize{
#' \item{'summaryTable': 
#' \itemize{
#' \item{'statN': number of subjects}
#' \item{'statMean': mean of \code{var}}
#' \item{'statSD': standard deviation of \code{var}}
#' \item{'statSE': standard error of \code{var}}
#' \item{'statMedian': median of \code{var}}
#' \item{'statMin': minimum of \code{var}}
#' \item{'statMax': maximum of \code{var}}
#' \item{'statPerc': percentage of subjects}
#' \item{'statm': number of records}
#' }
#' }
#' \item{'countTable': 
#' \itemize{
#' \item{'statN': number of subjects}
#' \item{'statPercN': percentage of subjects}
#' \item{'statm': number of records}
#' }
#' }
#' }
#' If \code{stats} if of length 1, the name of the summary statistic is not included
#' in the table.
#' The statistics can be specified for each \code{var} (if multiple), 
#' by naming each element of the list:
#' list(varName1 = list(...), varName2 = list()) and/or for each element in:
#' \code{statsVarBy}, by naming each sublist.
#' @param statsVarBy Character vector with colnames of
#' \code{data} by which the statistics should be computed.
#' In this case, \code{stats} (nested list or not) should be additionally nested
#' to specify the statistics for each element in \code{statsVarBy}.
#' @inheritParams inTextSummaryTable-common-args
#' @return List with two elements:
#' \itemize{
#' \item{'summaryTable': summary table updated with statistics specified in \code{stats}}
#' \item{'statsVar': Character vector with statistics names}
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
							if(length(stats) == 0){
								return(sumTable)
							}
						}else{
							stop("The statistic name: '", stat, "'",
								" is a default name used, please choose a different name.")
						}
					}
				}
			}
			
			runStats <- function(expr){
				if(is.language(expr)){
					eval(expr = expr, envir = sumTable)
				}else{
					stop(paste("Statistics specified in 'stats' should be an expression or name object,",
						"or a list named with variable, statistic name or statsVarBy elements."))
				}
			}
			statsDf <- if(is.list(stats)){
				sapply(stats, runStats, simplify = FALSE)
			}else	list(runStats(expr = stats))
			if(is.null(statsName))	names(statsDf) <- "Statistic"
			
			# save in summaryTable
			sumTable <- cbind(sumTable, statsDf, stringsAsFactors = FALSE)
			
			return(sumTable)
			
		}
		
		getStatColName <- function(stats)
			if(is.null(names(stats)))	"Statistic"	else	names(stats)

		# order stats across all nested list
		# to avoid wrong ordering in case some stats are ordered differently by variable	
		getOrderedStats <- function(statsByVar){
			statsByVarUnique <- unique(unlist(statsByVar))
			statsByVarPos <- lapply(statsByVar, match, x = statsByVarUnique)
			statsByVarPosMax <- do.call(pmax, c(statsByVarPos, list(na.rm = TRUE)))
			# order in case multiple elements have same position
			statsByVarOrdered <- unname(statsByVarUnique[order(statsByVarPosMax)]) 
			return(statsByVarOrdered)
		}
		
		# variable to compute the statistics by:
		if(any(names(stats) %in% var)){
			
			statsVarByUsed <- c(statsVarBy, "variableInit")
			
			if(!"variableInit" %in% colnames(summaryTable))
				stop("'stats' should not be named by variable if only one variable is specified.")
			
			# in case more stats are specified than specified var
			varsUsed <- setdiff(unique(as.character(summaryTable$variableInit)), NA)
			
			if(!all(varsUsed %in% names(stats))){
				stop(paste("If 'stats' is specified for each variable,",
					"it should be specified for all variables specified in 'var'."))
			}else stats <- stats[varsUsed]
			statsByVar <- lapply(stats, function(x)
				if(!is.null(statsVarBy)){
					getOrderedStats(lapply(x, getStatColName))
				}else	getStatColName(x)
			)
			statsVar <- getOrderedStats(statsByVar)
		}else{
			statsVarByUsed <- statsVarBy
			statsVar <- if(!is.null(statsVarBy)){
				statsByVar <- lapply(stats, getStatColName)
				getOrderedStats(statsByVar)
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

		if(".id" %in% colnames(summaryTable))
			summaryTable <- summaryTable[, -which(colnames(summaryTable) == ".id")]
		
	}else	statsVar <- statsVarInit
	
	res <- list(
		summaryTable = summaryTable,
		statsVar = statsVar	
	)

}
