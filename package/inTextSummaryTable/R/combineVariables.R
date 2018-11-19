#' Create a data.frame combining records from multiple variable.
#' 
#' For each variable, subset of interest can be specified.
#' @param data Data.frame with data.
#' @param paramsList nested list of parameters.
#' There are two ways to select a subset of interest:
#' \itemize{
#' \item{by specifying one unique variable of interest with: }{
#' \itemize{
#' \item{
#' \item{'var': }{string with column of \code{data} of interest}
#' \item{'value': }{value of \code{var} of interest (only used if \code{var} is specified)}
#' \item{'fctTest': }{string with function to apply on \code{var} to select subset of interest versus 'value'
#' If not specified, the records with \code{var} equal to \code{value} are retained (\code{fctTest} set to '==')} 
#' \item{'labelExtra': }{string with extra label concatenate with \code{var} label extracted from \code{labelVars}}
#' }}}
#' \item{by specifying a combination of variable of interest with: }{
#' \itemize{
#' \item{'exprs': }{string with expression of columns of \code{data} to select subset of interest}
#' \item{'label': }{string with label for this group}
#' }}
#' }
#' @param newVar String with name of new variable to construct.
#' @param fctTest String with default function to use to compare \code{var} and \code{value} specified
#' in each sublist of \code{paramsList}.
#' @param includeAll Logical, if TRUE (FALSE by default) include also the entire data as an additional subgroup.
#' @param labelAll String of group label for the entire data in case \code{includeAll} is TRUE.
#' @inheritParams glpgUtilityFct::getLabelVar
#' @return Data.frame with data and additional variable \code{newVar}.
#' @author Laure Cougnaud
#' @export
combineVariables <- function(data, paramsList, newVar, 
	labelVars = NULL, fctTest = "==",
	includeAll = FALSE, labelAll = "Any"){
	
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
			}else stop("Expression ('exprs') of a variable",
				"of interest ('var') should be specified",
				"for the parameter:", i, ".")
		}
		if(nrow(dataRetained) > 0){
			dataRetained[, newVar] <- paramsLabel[i]
			dataRetained
		}
	})

	# combine all data
	dataRetained <- do.call(rbind, dataList)

	if(includeAll){
		dataRetained <- rbind(cbind(data, setNames(list(labelAll), newVar)), dataRetained)
		paramsLabel <- c(labelAll, paramsLabel)
	}

	dataRetained[, newVar] <- factor(dataRetained[, newVar], levels = paramsLabel)

	return(dataRetained)

}