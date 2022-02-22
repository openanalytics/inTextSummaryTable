#' Create a data.frame combining a set of records from same or multiple variables.
#' 
#' This typically converts the data from a wide to a long format.
#' For each variable, a subset of interest based on a condition can be specified.
#' @param paramsList nested list of parameters, 
#' specifying how the records of interest should be selected.\cr
#' There are two ways to select a subset of interest:
#' \itemize{
#' \item{by specifying one unique variable of interest with: }{
#' \itemize{
#' \item{\code{var}: }{string with column of \code{data} of interest}
#' \item{\code{value}: }{value of \code{var} of interest (only used if \code{var} is specified).\cr
#' If not specified only the values different than NA and '' are considered.}
#' \item{\code{fctTest}: }{string with name or directly comparison function
#' to apply on \code{var} to select subset of interest versus \code{value}.\cr
#' The function should take \code{var} as first parameter and \code{value} to compare to
#' as second parameter and returns a logical vector with TRUE or FALSE (of length \code{var})
#' if the condition is fullfilled.\cr
#' If not specified, the records with \code{var} equal to \code{value} are retained 
#' (\code{fctTest} is set to '==').} 
#' \item{label specification: }{
#' \itemize{
#' \item{\code{label}: }{string with label for the condition,
#' includde in the new 'variable' column.\cr
#' If not specified and:
#' \itemize{
#' \item{\code{var} is specified: }{label is extracted from 
#' \code{labelVars} if available or set to \code{var} otherwise.}
#' \item{\code{var} is not specified: }{label should be specified.}
#' }}
#' \item{\code{labelExtra}: }{string with extra label, will be concatenated with \code{label}}
#' }}
#' }}
#' 
#' \item{by specifying a combination of variable of interest with: }{
#' \itemize{
#' \item{\code{exprs}: }{string with expression of columns of \code{data} to select subset of interest}
#' \item{\code{label}: }{string with complete label for the group}
#' }}
#' 
#' }
#' @param newVar String with name of new variable to construct.
#' @param fctTest Default function to use to compare \code{var} and \code{value} specified
#' in each sublist of \code{paramsList}.\cr
#' This is only used if \code{fctTest} is not specified in each sublist.
#' @param includeAll Logical, if TRUE (FALSE by default) include also the entire data as an additional subgroup.
#' @param labelAll String of group label for the entire data in case \code{includeAll} is TRUE.
#' @inheritParams inTextSummaryTable-common-args
#' @return Data.frame with records from \code{data} extracted based on
#' the different conditions specified in \code{paramsList}.\cr
#' This data.frame contains an additional variable 
#' (labelled based on \code{newVar}) mentioning the
#' specific condition for which the record was extracted
#' (based \code{label}, \code{labelExtra}, \code{labelVars}).\cr
#' This variable is a factor whose levels are ordered based on the order
#' of the condition specified in \code{paramsList}.
#' @author Laure Cougnaud
#' @importFrom clinUtils getLabelVar
#' @export
combineVariables <- function(
	data, paramsList, newVar, 
	labelVars = NULL, fctTest = "==",
	includeAll = FALSE, labelAll = "Any"){

	if(class(data)[1] != "data.frame")
		data <- as.data.frame(data)
	
	if(!all(sapply(paramsList, is.list)))
		stop("'paramsList' should be a nested list.")

	# extract label used for new variable for each subset of interest
	paramsLabel <- sapply(seq_along(paramsList), function(i){
		x <- paramsList[[i]]
		label <- if(!is.null(x$var)){
			if(!is.null(x[["label"]]))	x[["label"]]	else	getLabelVar(x$var, data, labelVars)
		}else{
			if(!is.null(x[["label"]]))	x[["label"]]	else
				stop("Label should be specified for parameter: '", i, "'.")
		}
		if(!is.null(x[["labelExtra"]]))	label <- paste(label, x[["labelExtra"]])
		label
	})
	if(any(duplicated(paramsLabel)))
		stop("Duplicated labels in the specified parameter list.")
	
	# extract each subset of interest
	dataList <- lapply(seq_along(paramsList), function(i){
		x <- paramsList[[i]]
		dataRetained <- if(!is.null(x$exprs)){
			rowsRetained <- eval(expr = parse(text = x$exprs), envir = data)
			data[rowsRetained, ]
		}else{
			if(!is.null(x$var)){
				if(!is.null(x$value)){
					fct <- if(!is.null(x$fctTest))	x$fctTest	else	fctTest
					data[match.fun(fct)(data[, x$var], x$value), ]
				}else{
					xVar <- data[, x$var]
					data[!is.na(xVar) & xVar != "", ]
				}
			}else stop("Expression ('exprs') or a variable",
				" of interest ('var') should be specified",
				" for the parameter: ", i, ".")
		}
		if(nrow(dataRetained) > 0)
			dataRetained[, newVar] <- paramsLabel[i]
		dataRetained
	})

	# combine all data
	dataRetained <- do.call(rbind, dataList)

	if(includeAll){
		dataRetained <- rbind(
			cbind(data, setNames(list(labelAll), newVar), stringsAsFactors = TRUE), 
			dataRetained, stringsAsFactors = TRUE
		)
		paramsLabel <- c(labelAll, paramsLabel)
	}

	dataRetained[, newVar] <- factor(
		if(nrow(dataRetained) == 0)	character()	else	dataRetained[, newVar]
	, levels = paramsLabel)

	return(dataRetained)

}