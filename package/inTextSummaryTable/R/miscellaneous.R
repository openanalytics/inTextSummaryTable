#' Custom round function, rounding to the closest digits
#' (instead of rounding to the even number in case of 0.5)
#' @param x Numeric vector to round.
#' @param digits Integer with number of digits to consider, 0 by default.
#' @param format For backward compatibility. This parameter will be deprecated in the next package release.
#' @return A character vector with the rounded number.
#' See the \code{glpgUtilityFct::roundCustom} for the rounding customization.
#' @author Laure Cougnaud and Michela Pasetto
#' @importFrom glpgUtilityFct roundCustom
#' @examples 
#' # number of digits higher than number of decimal
#' roundCustomText(x = c(0.345, 0.567, -0.98), digits = 2)
#' # number of digits lower than number of decimal
#' roundCustomText(x = c(0.345, 0.567, -0.98), digits = 0)
#' # by default, 'digits' is 0!
#' roundCustomText(x = c(0.345, 0.567, -0.98))
#' @export
roundCustomText <- function(x, digits = 0, format) {
	
	#format <- match.arg(format)	
	#res <- roundCustom(..., format = format)
	if(! missing(format)) warning("The 'format' argument is deprecated. \n The format output is always a character vector.")
	
	z <- roundCustom(x = x, digits = digits)
	res <- formatC(z, digits = digits, format = "f", flag = "0")
	
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
#' @keywords internal
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
#' @keywords internal
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
#' should contain elements: 'Y' and 'N', or '' (for missing value).
#' @return Formatted factor variable:
#' \itemize{
#' \item{empty string converted to NA}
#' \item{'Y' converted to empty string ('')}
#' \item{'N' retained as originally}
#' }
#' The variable has levels: \code{c('', 'N')}.
#' @author Laure Cougnaud
convertVarFlag <- function(x){
	
	if(any(!x %in% c("", "Y", "N")))
		stop("Flag variable should only contain: '', 'Y' or 'N'.")
	x <- as.character(x)
	xRF <- factor(ifelse(x == "", NA_character_, ifelse(x == "Y", "", "N")), levels = c("", "N"))

	return(xRF)

}

#' Convert \code{rowVar}, \code{colVar} and character \code{var} in \code{data} to factor
#' @param data Data.frame with data.
#' @param rowVar Character vector with variable(s) used for the rows.
#' @param colVar Character vector with variable(s) used for the columns.
#' @param var Character vector with variable(s) used for the summary statistics.
#' @return Updated \code{data}
#' @author Laure Cougnaud
convertVarRowVarColVarToFactor <- function(data, rowVar = NULL, colVar = NULL, var = NULL){
	
	# convert row and column variable to factor in the data
	# (if character, variables pre-defined as factor in one summary tables will be lost during rbind.fill)
	if(!is.null(rowVar)){
		rowVar <- intersect(rowVar, colnames(data))
		if(length(rowVar) > 0)
			data[, rowVar] <- lapply(data[, rowVar, drop = FALSE], function(x){
				levelsX <- if(is.factor(x))	levels(x)	else	sort(unique(x))
				factor(x, levels = levelsX)
			})
	}
	
	if(!is.null(colVar)){
		colVar <- intersect(colVar, colnames(data))
		if(length(colVar) > 0)
			data[, colVar] <- lapply(data[, colVar, drop = FALSE], function(x){
				levelsX <- if(is.factor(x))	levels(x)	else	sort(unique(x))
				factor(x, levels = levelsX)
			})
	}

	if(!is.null(var)){
		var <- intersect(var, colnames(data))
		if(length(var) > 0){
			isVarCharac <- names(which(sapply(data[, var, drop = FALSE], is.character)))
			if(length(isVarCharac) > 0)
				data[, isVarCharac] <- lapply(data[, isVarCharac, drop = FALSE], function(x){
					levelsX <- if(is.factor(x))	levels(x)	else	sort(unique(x))
					factor(x, levels = levelsX)
				})
		}
	}

	return(data)
	
}

#' Check the \code{varLabInclude} variable.
#' 
#' This function ensures that:
#' \itemize{
#' \item{variable name is included if more than one variable
#' are specified
#' }
#' \item{variable name is not included if no variable is specified}
#' }
#' @param var Character vector with summary statistics variable(s).
#' @param varLabInclude Logical, if TRUE
#' the name of the summary statistic variable(s) (\code{var})
#' are included in the table.
#' This is automatically set to TRUE if more than one variable(s) 
#' and is specified, and FALSE if only one variable is specified.
#' @return (Updated) \code{varLabInclude}
#' @author Laure Cougnaud
checkVarLabInclude <- function(var, varLabInclude = length(var) > 1){
	
	if(length(var) > 1 & !varLabInclude){
		warning(paste("Variable label is included in the table ('varLabInclude' is set to TRUE)",
			"because more than 1 variable is specified in the 'var' parameter."))
		varLabInclude <- TRUE
	}
	if(length(var) == 0 & varLabInclude){
		warning(paste("Variable label is not included ('varLabInclude' is set to FALSE)",
			"because no variable is specified in the 'var' parameter."))
		varLabInclude <- FALSE
	}
	
	return(varLabInclude)
	
}

#' Check if variable(s) are present in reference: either in columns in 
#' a dataset or in reference set.
#' 
#' Filter variables not present in the data or in reference set with a warning,
#' and only returned filtered vector, or NULL if empty.
#' @param var (Named) character vector with variables of interest.
#' @param varLabel String with label for \code{var}, e.g.
#' name of associated parameter.
#' @param varRef (Named) character vector with set of reference variables.
#' @param data Data.frame with data.
#' @param refLabel String with label for the reference
#' @param varUncheck (Named) character vector with extra variables 
#' in \code{var} which shouldn't be checked.
#' @return (Named) \code{var} filtered with element not in \code{data}
#' or in \code{refSet}.
#' If filtered \code{var} is empty, NULL is returned
#' @author Laure Cougnaud
checkVar <- function(
	var, varLabel, varUncheck = NULL,
	varRef, 
	refLabel = ifelse(!missing(varRef), "reference variable", "data"),
	data){
	
	if(!missing(varRef) & !missing(data))
		stop("Either 'data' or 'varRef' should be specified.")
	
	varToFilter <- NULL
	if(!missing(varRef)){
		varToFilter <- var[!var %in% varRef]
	}else	if(!missing(data)){
		varToFilter <- var[!var %in% colnames(data)]
	}else	stop("'data' or 'varRef' should be specified.")
	
	# remove extra variables that shouldn't be checked
	varToFilter <- varToFilter[!varToFilter %in% varUncheck]
	
	if(length(varToFilter) > 0){
		warning(paste0(
			"Variable(s): ",
			toString(shQuote(varToFilter)),
			" in ", varLabel, 
			" are ignored because they are not available in: ", 
			refLabel, "."
		))
	}
	
	varFiltered <- var[!var %in% varToFilter]
	if(length(varFiltered) == 0)
		varFiltered <- NULL
	
	return(varFiltered)
	
}

#' Custom function to write table to a text file
#' 
#' This function is mainly a wrapper on \code{\link[utils]{write.table}},
#' with the specific options:
#' \itemize{
#' \item{no rownames}
#' \item{no quoting}
#' \item{tab separator}
#' }
#' @param x Data.frame to export to the table.
#' @param file String with text file to export to.
#' @param ... Any parameters passed to the \code{\link[utils]{write.table}} function.
#' @return No returned value, the object \code{x} is exported to the specified \code{file}.
#' @importFrom utils write.table
#' @importFrom tools file_ext
#' @author Laure Cougnaud
writeTable <- function(x, file, ...){
	if(length(file) > 1)
		stop("'file' should be of length 1.")
	if(file_ext(file) != "txt")
		stop("'file' should be of 'txt' extension.")
	write.table(x, file, quote = FALSE, sep = "\t", row.names = FALSE, ...)
}