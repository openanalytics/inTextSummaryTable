#' Create a data.frame combining set of records from same or multiple variables.
#' 
#' For each variable, subset of interest can be specified.
#' @param data Data.frame with data.
#' @param paramsList nested list of parameters.
#' There are two ways to select a subset of interest:
#' \itemize{
#' \item{by specifying one unique variable of interest with: }{
#' \itemize{
#' \item{'var': }{string with column of \code{data} of interest}
#' \item{'value': }{value of \code{var} of interest (only used if \code{var} is specified).
#' \code{If not specified only the values different than NA and '' are considered.}}
#' \item{'fctTest': }{string with function name or directly function
#' to apply on \code{var} to select subset of interest versus 'value',
#' The function should take  \code{var} parameter and \code{value} to compare to
#' as second parameter and returns a logical vector with TRUE or FALSE (of length \code{var})
#' if the condition is fullfilled.
#' If not specified, the records with \code{var} equal to \code{value} are retained (\code{fctTest} set to '==')
#' } 
#' \item{label specification: }{
#' \itemize{
#' \item{'label': }{string with label for the condition,
#' include in the new variable column.
#' If the label is not specified and:
#' \itemize{
#' \item{\code{var} is specified: }{label is extracted from 
#' \code{labelVars} if available or set to \code{var} otherwise.}
#' \item{\code{var} is not specified: }{label should be specified.}
#' }
#' \item{'labelExtra': }{string with extra label, will be concatenated with label}
#' }
#' }
#' }}
#' \item{by specifying a combination of variable of interest with: }{
#' \itemize{
#' \item{'exprs': }{string with expression of columns of \code{data} to select subset of interest}
#' \item{'label': }{string with complete label for the group}
#' }}}
#' @param newVar String with name of new variable to construct.
#' @param fctTest Global default function to use to compare \code{var} and \code{value} specified
#' in each sublist of \code{paramsList}.
#' This is only used if \code{fctTest} is not specified in each sublist.
#' @param includeAll Logical, if TRUE (FALSE by default) include also the entire data as an additional subgroup.
#' @param labelAll String of group label for the entire data in case \code{includeAll} is TRUE.
#' @inheritParams glpgUtilityFct::getLabelVar
#' @return Data.frame with data and additional variable \code{newVar}.
#' @author Laure Cougnaud
#' @export
combineVariables <- function(data, paramsList, newVar, 
	labelVars = NULL, fctTest = "==",
	includeAll = FALSE, labelAll = "Any"){
	
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