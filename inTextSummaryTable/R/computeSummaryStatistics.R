#' Compute summary statistics of interest of an unique variable of interest.
#' 
#' Additionally, this function run extra checks on the data:
#' \itemize{
#' \item{an error message is triggered if any subject (identified by \code{subjectVar}) 
#' have different values in a continuous \code{var}
#' }
#' \item{an indicative message is triggered if multiple but identical records are available
#' for \code{subjectVar} and a continuous \code{var}}
#' } 
#' @param filterEmptyVar Logical, if TRUE doesn't return any results
#' if the variable is empty, otherwise return 0 for the counts and NA for summary 
#' statistics.
#' Criterias to consider a variable empty are:
#' \itemize{
#' \item{for a continuous variable: }{all missing (NA)}
#' \item{for a categorical variable: }{all missing or **category is included in the 
#' factor levels but not available in \code{data}**}
#' }
#' By default, an empty variable are filtered.
#' @param varTotalInclude Logical (FALSE by default)
#' Should the total across all categories of \code{var} 
#' be included for the count table?
#' Only used if \code{var} is a categorical variable.
#' @param msgLabel (optional) String with label for the data (NULL by default), 
#' included in the message/warning for checks.
#' @param msgVars (optional) Character vector with columns of \code{data}
#' containing extra variables (besides \code{var} and \code{subjectVar})
#' that should be included in the message/warning for checks.
#' @param checkVarDiffBySubj String, 'error' (default), 'warning',
#' or 'none'.  
#' Should an error, a warning, or nothing be produced
#' if a continuous variable (\code{var}) contains
#' different values for the same subject?
#' @inheritParams inTextSummaryTable-common-args
#' @return Data.frame with summary statistics in columns,
#' depending if \code{type} is:
#' \itemize{
#' \item{'summary': }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statm': }{number of records}
#' \item{'statMean': }{mean of \code{var}}
#' \item{'statSD': }{standard deviation of \code{var}}
#' \item{'statSE': }{standard error the mean of \code{var}}
#' \item{'statMedian': }{median of \code{var}}
#' \item{'statMin': }{minimum of \code{var}}
#' \item{'statMax': }{maximum of \code{var}}
#' }
#' }
#' \item{'count': }{
#' \itemize{
#' \item{'variableGroup': }{factor with groups of \code{var} for which counts are reported}
#' \item{'statN': }{number of subjects}
#' \item{'statm': }{number of records}
#' }
#' }
#' }
#' @author Laure Cougnaud
#' @importFrom stats na.omit median sd
#' @importFrom methods formalArgs
#' @importFrom utils capture.output hasName
#' @export
computeSummaryStatistics <- function(data, 
	var = NULL, varTotalInclude = FALSE,
	statsExtra = NULL,
	subjectVar = "USUBJID",
	filterEmptyVar = TRUE,
	type = "auto",
	checkVarDiffBySubj = c("error", "warning", "none"),
	msgLabel = NULL, msgVars = NULL){
	
	checkVarDiffBySubj <- match.arg(checkVarDiffBySubj)
	
	if(class(data)[1] != "data.frame")
		data <- as.data.frame(data)

	## checks parameters
	
	type <- match.arg(type, choices = c("auto", "summaryTable", "countTable"))
	
	if(!is.null(var) && var != "all" && !var %in% colnames(data))
		stop(paste("Variable to summarize:", shQuote(var), "is not available in data."))
	
	if(!subjectVar %in% colnames(data))
		stop(paste("Subject variable:", shQuote(subjectVar), 
			"is not available for the computation of the number of subjects."))
	
	if(type == "auto")
		type <- ifelse(
			!is.null(var) &&  var != "all" && is.numeric(data[, var]), 
			"summaryTable", 
			"countTable"
		)
	
	if(type == "summaryTable"){
		if(is.null(var)){
			stop("Variable of interest should be specified via the 'var' parameter for a summary table.")
		}else if(var == "all"){
			stop("The 'type' should be set to 'countTable' in case 'var' is 'all'.")
		}else	if(!is.numeric(data[, var])){
			stop("Variable of interest: 'var' should be numeric in case type is set to 'summaryTable'.")
		}
	}
	
	isVarSpec <- (!is.null(var) && var != "all")
	
	if(isVarSpec)
		data <- data[!is.na(data[, var]), , drop = FALSE]
	
	getNSubjects <- function(x){
		if(isVarSpec && any(is.na(x[, subjectVar])))
			warning(paste(
				"Missing records (NA) in:", shQuote(subjectVar),
				"are not considered for subject counts,",
				"please note that this can results in discrepancies",
				"between summary statistics of", shQuote(var),
				"and subject counts."
			))
		as.integer(length(unique(na.omit(x[, subjectVar]))))
	}
	getNRecords <- function(x) nrow(x)
	
	# wrapper to add extra statistics
	statsExtraFct <- function(res, statsExtra, val = NULL, data){
		# Note: error with: 'cbind' if res is empty
		# so only add stats if some results are available
		if(!is.null(statsExtra) & nrow(res) > 0){
			
			if(!is.list(statsExtra))
				stop("'statsExtra' should be a list.")
			
			if(is.null(names(statsExtra)) || !all(names(statsExtra) != ""))
				stop("'statsExtra' should be named.")
			
			statsExtraCommon <- intersect(names(statsExtra), colnames(res))
			if(length(statsExtraCommon) > 0)
				stop("Name(s) in 'statsExtra': ", toString(statsExtraCommon), 
					" are already in default statistics, please choose a different name for your custom statistics.")
			resExtra <- sapply(statsExtra, function(fct){
				switch(
					formalArgs(fct)[1], 
					'x' = fct(val), 
					'data' = fct(data),
					stop("'statsExtra' should contain a parameter named 'x' or 'data'.")
				)
			}, simplify = FALSE)	
			
			res <- cbind(res, resExtra, stringsAsFactors = TRUE)
		}
		return(res)
	}
	
	switch(type,
			
		'summaryTable' = {
				
			val <- data[, var]
			emptyVar <- is.null(val) || length(val) == 0
			if(!(filterEmptyVar & emptyVar)){
				
				# check of multiple records per subject
				if(!emptyVar){
					
					# filter records with same values per subject:
					dataSubjVar <- data[, c(subjectVar, var)]
					isDuplSubjVar <- duplicated(dataSubjVar)
					if(any(isDuplSubjVar) > 0){
						dataDupl <- merge(data, data[isDuplSubjVar, c(subjectVar, var), drop = FALSE])
						dataDupl <- dataDupl[, unique(c(subjectVar, msgVars, var)), drop = FALSE]
						message(sum(isDuplSubjVar), " record(s) with duplicated values",
							" for ", var, 
							" are filtered before the computation of the statistics ", 
							if(!is.null(msgLabel))	paste("for the", msgLabel), ":\n",
							paste(capture.output(print(dataDupl)), collapse = "\n")
						)
						data <- data[!isDuplSubjVar, ]
						val <- data[, var]
					}
					
					isDupl <- duplicated(data[, subjectVar])
					if(any(isDupl) & checkVarDiffBySubj != "none"){
						dataDupl <- merge(data, unique(data[isDupl, subjectVar, drop = FALSE]))
						dataDupl <- dataDupl[, unique(c(subjectVar, msgVars, var)), drop = FALSE]
						msgDupl <- paste0(
							switch(checkVarDiffBySubj,
								`error` = "Extraction of statistics failed because multiple", 
								`warning` = "Multiple"
							),
							" different records of ", var, 
							if(!is.null(msgLabel))	paste(" for the", msgLabel), 
							" are available for ",
							"the same ", subjectVar, ":\n",
							paste(capture.output(print(dataDupl)), collapse = "\n")
						)
						switch(
							checkVarDiffBySubj,
							`error` = stop(msgDupl),
							`warning` = warning(msgDupl)
						)
					}
				}
				
				res <- data.frame(
					statN = getNSubjects(data),
					statm = getNRecords(data),
					statMean = ifelse(emptyVar, NA_real_, mean(val)),
					statSD = ifelse(emptyVar, NA_real_, sd(val)),
					statSE = ifelse(emptyVar, NA_real_, se(val)),
					statMedian = ifelse(emptyVar, NA_real_, median(val)),
					statMin = ifelse(emptyVar, NA_real_, min(val)),
					statMax = ifelse(emptyVar, NA_real_, max(val))
				)
				res <- statsExtraFct(
					res = res, statsExtra = statsExtra, 
					val = val, data = data
				)
				
			}else	res <- NULL
			
		},
		
		'countTable' = {
				
			if(isVarSpec){
				
				varLevels <- if(is.factor(data[, var]))	levels(data[, var])	else	unique(data[, var])
				resList <- lapply(varLevels, function(level){
					x <- data[which(data[, var] == level), ]	
					# compute stats in data or if filterEmptyVar is FALSE
					if(!(nrow(x) == 0 & filterEmptyVar)){
						res <- setNames(
							data.frame(level, getNSubjects(x), getNRecords(x), stringsAsFactors = FALSE),
							c(var, "statN", "statm")
						)
						res <- statsExtraFct(
							res = res, 
							statsExtra = statsExtra,
							data = x
						)
					}
				})
				resList <- resList[!sapply(resList, is.null)]
				if(length(resList) > 0){
					res <- do.call(rbind, c(resList, stringsAsFactors = FALSE))
					res[, var] <- factor(res[, var], levels = varLevels)
					rownames(res) <- NULL
				}else{
					res <- data.frame()
				}
				
			}else{
				res <- data.frame(statN = getNSubjects(data), statm = getNRecords(data))
				res <- statsExtraFct(res = res, statsExtra = statsExtra, data = data)
			}
				
			includeTotal <- varTotalInclude & 
				(is.null(var) || var != "all")
			if(includeTotal){
				
				listTotal <- c(
					setNames(list("Total"), var),
					list(
						statN = getNSubjects(data),
						statm = getNRecords(data)
					)
				)
				resTotal <- do.call(data.frame, listTotal)
				resTotal <- statsExtraFct(res = resTotal, statsExtra = statsExtra, data = data)
				res <- rbind.fill(res, resTotal)
				elVar <- if(is.factor(data[, var]))	levels(data[, var])	else	unique(data[, var])
				res[, var] <- factor(res[, var], levels = c(elVar, "Total"))
				
			}
			
			if(isVarSpec && var %in% colnames(res))
				colnames(res)[match(var, colnames(res))] <- "variableGroup"
			
		}
	
	)
	
	return(res)
	
}
