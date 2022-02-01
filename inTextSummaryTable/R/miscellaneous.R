#' Get dimension of the page available for content 
#' for standard Word report or PowerPoint presentation.
#' 
#' Report is in A4 and presentation dimensions extracted from
#' PowerPoint.
#' The returned dimensions are the page dimensions without the margins.
#' @param type Character vector with dimension of interest, among: 
#' 'width', 'height', multiple are possible.
#' By default: \code{c("width", "height")}
#' @param landscape Logical, if TRUE the table is presented in landscape
#' format. \cr
#' By default: TRUE for \code{style}: 'report',
#' FALSE for \code{style}: 'presentation'.
#' @param margin Margin in the document in inches, 1 by default.
#' @param style String with table style, either 'report' (by default, a4 format) 
#' or 'presentation'
#' @param pageDim (optional) Numeric vector of length 2 with 
#' page width and height in inches in portrait format,
#' in case page dimensions differ from the default
#' implemented report/presentation.
#' These dimensions should include the margins.
#' @examples 
#' ## get part of the page available for content
#' # report A4 portrait format:
#' getDimPage(type = "width")
#' getDimPage(type = "height")
#' # report A4 landscape format:
#' getDimPage(type = "width", landscape = TRUE)
#' getDimPage(type = "height", landscape = TRUE)
#' # Note that the layout is by default set to 'landscape'
#' getDimPage(type = "width", style = "presentation")
#' getDimPage(type = "height", style = "presentation")
#' # custom dimensions: A3 format
#' getDimPage(type = "width", pageDim = c(11.7, 16.5))
#' # increase margin
#' getDimPage(type = "width", margin = 1.5)
#' # get both dimensions at once
#' getDimPage(type = c("width", "height"))
#' # get dimensions of the full page (including margins)
#' getDimPage(type = c("width", "height"), style = "report", margin = 0)
#' getDimPage(type = c("width", "height"), style = "presentation", margin = 0)
#' @return numeric vector with dimension of interest,
#' in the same order as specified via the \code{type}
#' parameter.
#' @author Laure Cougnaud
#' @export
getDimPage <- function(
    type = c("width", "height"), 
    landscape = (style == "presentation"), 
    margin = 1,
    pageDim = NULL,
    style = "report")
{
  
  # landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
  type <- match.arg(type, several.ok = TRUE)
  
  style <- match.arg(style, choices = c("report", "presentation"))
  
  pageDimPortrait <- 	if(is.null(pageDim)){
        switch(style,
            'report' = c(21, 29.7)/2.54,
            'presentation' = getOption("inTextSummaryTable.pageDim.presentation")
        )
      } else {
        if(!is.numeric(pageDim))
          stop("'pageDim' should be a numeric vector.")
        if(length(pageDim) != 2)
          stop("'pageDim' should be of length 2.")
        pageDim
      }
  
  typeDim <- numeric()
  for(i in seq_along(type)) {
    typeDim[i] <- switch(type[i],
        'width' = ifelse(landscape, pageDimPortrait[2], pageDimPortrait[1]),
        'height' = ifelse(landscape, pageDimPortrait[1], pageDimPortrait[2])
    )
  }
  dimPage <- typeDim - 2 * margin
  
  return(dimPage)
}


#' Page dimension for powerpoints
#' @export 
pageDimPresentation <- c(7.5, 10)

#' Get specific attribute from a summaryTable or a list of summaryTables
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
#' @return Formatted factor variable with levels: 'Y' & 'N'.
#' Empty strings have been converted to NA.
#' @author Laure Cougnaud
convertVarFlag <- function(x){
	
	if(any(!x %in% c("", "Y", "N")))
		stop("Flag variable should only contain: '', 'Y' or 'N'.")
	x <- as.character(x)
	x[which(x == "")] <- NA_character_
	xRF <- factor(x, levels = c("Y", "N"))

	return(xRF)

}

#' Post-process the summary statistics table with variable flag.
#' 
#' This function is for internal use within
#' the \code{\link{computeSummaryStatisticsTable}}
#' function.
#' 
#' This includes:
#' \itemize{
#' \item{converting the records from a flag variable
#' for the 'variableGroup' variable 
#' from 'Y' to \code{NA_character_}
#' }
#' \item{filter records from a flag variable 
#' with variableGroup set as 'N'}
#' }
#' @param summaryTable Summary table as created internally in
#' \code{\link{computeSummaryStatisticsTable}}.
#' @inheritParams inTextSummaryTable-common-args
#' @return Summary table with
#' @author Laure Cougnaud
postProcessVarFlag <- function(summaryTable, varFlag){
	
	# which records refer to a flag variable?
	summaryTable$isVarFlag <- if("variableInit" %in% colnames(summaryTable)){
		summaryTable$variableInit %in% varFlag
	}else{TRUE}
	
	# filter the 'non' flagged counts:
	idxKept <- with(summaryTable, which(isTotal | !(isVarFlag & variableGroup == "N")))
	summaryTable <- summaryTable[idxKept, ]
	
	# convert '' to NA to indicate that there was no internal subgroup
	idxFlag <- with(summaryTable, which(isVarFlag & variableGroup == "Y"))
	summaryTable[idxFlag, "variableGroup"] <- NA_character_
	
	summaryTable$isVarFlag <- NULL
	
	return(summaryTable)
	
}

#' Convert \code{rowVar}, \code{colVar} and character \code{var} in \code{data} to factor
#' @return Updated \code{data}
#' @inheritParams inTextSummaryTable-common-args
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
#' @param var String with variable to check.
#' @inheritParams inTextSummaryTable-common-args
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
#' @param var String with variable to check.
#' @param varLabel String with label for \code{var}, e.g.
#' name of associated parameter.
#' @param varRef (Named) character vector with set of reference variables.
#' @param data Data.frame with data.
#' @param refLabel String with label for the reference
#' @param varUncheck (Named) character vector with extra variables 
#' in \code{var} which shouldn't be checked.
#' @param msgType String with type of message returned, either a 'warning' (default)
#' or an error.
#' @return Depending on \code{msgType}:
#' \itemize{
#' \item{\code{warning}: }{warning is printed in the console, and a 
#' \code{var} filtered with element not in \code{data}
#' or in \code{refSet} is returned.
#' If filtered \code{var} is empty, NULL is returned.}
#' \item{\code{error}: }{an error is triggered.}
#' }
#' @author Laure Cougnaud
checkVar <- function(
	var, varLabel, varUncheck = NULL,
	varRef, 
	refLabel = ifelse(!missing(varRef), "reference variable", "data"),
	data,
	msgType = c("warning", "error")
){
	
	msgType <- match.arg(msgType)
	
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
		msg <- paste0(
			"Variable(s): ",
			toString(shQuote(varToFilter)),
			" in ", varLabel, 
			if(msgType == "warning")	" are ignored because they",
			" are not available in: ", 
			refLabel, "."
		)
		switch(msgType, warning = warning(msg), error = stop(msg))
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