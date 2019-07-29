#' Custom round function, rounding to the closest digits
#' (instead of rounding to the even number in case of 0.5)
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{roundCustom}} function.
#' @param format string with format for the number: 
#' 'text' (with trailing zeros) (by default) or 'number'.
#' @inherit glpgUtilityFct::roundCustom return
#' @importFrom glpgUtilityFct roundCustom
#' @export
roundCustomText <- function(..., format = c("text", "number")) {
	
	format <- match.arg(format)
	
	res <- roundCustom(..., format = format)
	
	return(res)
	
}

#' Get specific attribute from a summaryTable or a list of summaryTables
#' @param summaryTable Data.frame with summaryTable or list of such data.frames.
#' @param name String with attribute name.
#' @param default Object with default value for attribute, 
#' NULL by default.
#' @return Object with attribute, or \code{default} is doesn't
#' exists in \code{summaryTable}
#' @author Laure Cougnaud
getAttribute <- function(summaryTable, name, default = NULL){
	
	attribute <- if(is.data.frame(summaryTable)){
		attributes(summaryTable)$summaryTable[[name]]
	}else{
		attributeList <- lapply(summaryTable, function(x) attributes(x)$summaryTable[[name]])
		# only keep names within the list ('use.names' in unlist concatenate with higher-level element)
		res <- unlist(attributeList)
		names(res) <- unlist(lapply(attributeList, names))
		# use 'duplicated' because unique lose names
		res[!duplicated(res)]
	}
	
	if(is.null(attribute))
		attribute <- default

	return(attribute)
	
}

#' Compute the interaction between variable(s),
#' without propagating the missing values (if present in one of the variable)
#' unlike the behaviour with \code{\link{interaction}}.
#' This also ensure that the levels of the final interaction variables
#' are ordered similarly as the levels of the input \code{var} (if present).
#' @param data Data.frame with data.
#' @param var Character vector with variable(s) to consider.
#' @param varDataLevels (optional) Data.frame with data to consider
#' to define the levels of the variable.
#' If not specified, only the combinations of variable(s) available in the data are retained.
#' @return list with:
#' \itemize{
#' \item{'x': }{factor with interaction between the input \code{var}}
#' \item{'dataLevels': }{dtaa.frame with mapping between the \code{var} variable(s) and the new factor levels}
#' }
#' @author Laure Cougnaud
interactionCustom <- function(data, var, 
	varDataLevels = NULL){

	# use paste rather than 'interaction', otherwise the missing values are propagated
	varInteraction <- do.call(paste, data[, var, drop = FALSE])

	# extract the levels of the output factor 
	dataLevels <- (if(!is.null(varDataLevels))	varDataLevels	else	data)[, var, drop = FALSE]
	# ensure that the order from the input factors are preserved:
	dataLevels <- unique(dataLevels[do.call(order, dataLevels), , drop = FALSE])
	dataLevels$factorLevel <- do.call(paste, dataLevels)

	# build the final output factor
	if(!all(varInteraction %in% dataLevels$factorLevel))
		warning("Some variable records are not present in the data used for variable levels.")
	varInteractionFact <- factor(varInteraction, levels = dataLevels$factorLevel)
	
	res <- list(x = varInteractionFact, dataLevels = dataLevels)
	
	return(res)
	
}

#' Convert flag variable to a format such as only the flagged records are counted
#' in the summary table.
#' @param x Character or factor variable with flag variable,
#' should contain elements: 'Y' and 'N'.
#' @return Formatted factor variable:
#' \itemize{
#' \item{empty string converted to NA}
#' \item{'Y' converted to empty string}
#' \item{'N' retained as originally}
#' }
#' @author Laure Cougnaud
#' @export
convertVarFlag <- function(x){
	
	if(any(!x %in% c("", "Y", "N")))
		stop("Flag variables should only contain: '', 'Y' or 'N'.")
	x <- as.character(x)
	xRF <- factor(ifelse(x == "", NA_character, ifelse(x == "Y", "", "N")), levels = c("", "N"))

	return(xRF)

}