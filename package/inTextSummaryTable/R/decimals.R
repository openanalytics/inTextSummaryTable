#' Get number of decimals for a specific vector.
#' 
#' The number of decimals is extracted either:
#' \itemize{
#' \item{from specific implemented rule :}{
#' see \code{\link{getNDecimalsRule}} for further details}
#' \item{from the data itself: }{
#' see \code{\link{getNDecimalsData}} for further details
#' }
#' \item{both criterias: }{in this case the minimum of 
#' the number of decimals for both criterias is used}
#' }
#' @param useRule Logical (TRUE by default), should the \code{rule} be applied?
#' @param useData Logical (TRUE by default), should the number of decimals
#' be extracted based on the input data \code{x}?
#' @inherit getNDecimalsRule params return
#' @author Laure Cougnaud
#' @examples 
#' x <- c(0.99, 5.679, 50.45, 1450)
#' # extract number of decimals based on data:
#' getNDecimals(x, useRule = FALSE, useData = TRUE)
#' # extract number of decimals based on pre-defined rule:
#' getNDecimals(x, useRule = TRUE, useData = FALSE)
#' # extract number of decimals based on both rules
#' # minimum of both is used (by default)
#' getNDecimals(x, useRule = TRUE, useData = TRUE)
#' @family decimals
#' @export
getNDecimals <- function(x, useRule = TRUE, rule = "1", useData = TRUE){
	
	if(!useRule & !useData)
		stop(paste("The number of decimals should be extracted based on the rule and/or the data:",
			"'useRule' and/or 'useData' should be set to TRUE."))
	
	if(useRule)	nDecRule <- getNDecimalsRule(x, rule = rule)
	if(useData)	nDecData <- getNDecimalsData(x)
	
	nDec <- if(useRule & useData){
		mapply(min, nDecRule, nDecData)
	}else	if(!useData){
		nDecRule
	}else if(!useRule)	nDecData
	
	return(nDec)
	
}

#' Get maximum number of decimals in a variable,
#' based on pre-defined rule and/or data.
#' 
#' The function \code{\link{getNDecimals}}) extracts the number
#' of decimals in a specific variable.
#' @param ... Any parameters for the \code{\link{getNDecimals}} function.
#' @inheritParams getNDecimals
#' @return Integer with maximum number of decimals in a character vector.
#' @author Laure Cougnaud
#' @examples 
#' x <- c(0.99, 5.679, 50.45, 1450)
#' # extract max number of decimals based on data:
#' getMaxNDecimals(x, useRule = FALSE, useData = TRUE)
#' # extract max number of decimals based on pre-defined rule:
#' getMaxNDecimals(x, useRule = TRUE, useData = FALSE)
#' # extract max number of decimals based on both rules
#' # minimum of both is used (by default)
#' getMaxNDecimals(x, useRule = TRUE, useData = TRUE)
#' @family decimals
#' @export
getMaxNDecimals <- function(x, ...)
	max(getNDecimals(x, ...), na.rm = TRUE)

#' Get number of decimals based pre-defined rule(s).
#' 
#' Note: NA is returned if the element is missing (NA).
#' @param rule Character vector with rule to use to derive 
#' the number of parameters.
#' Currently only: '1' is implemented.
#' \itemize{
#' \item{'1': }{standard rule for the number of 
#' decimals for individual values for a 
#' continuous variable:
#' \itemize{
#' \item{value < 1 ('very small values'): }{3}
#' \item{value < 10: }{2}
#' \item{value in [10, 1000[: }{1}
#' \item{value >= 1000: }{0}
#' }
#' }
#' }
#' @examples
#' x <- c(0.99, 5.679, 50.45, 1450)
#' getNDecimalsRule(x = x)
#' @inherit getNDecimalsData params return
#' @author Laure Cougnaud
#' @export
getNDecimalsRule <- function(x, rule = c("1")){
	
	if(!is.numeric(x))
		stop("'x' should be numeric.")
	
	nDecRule <- switch(rule,
		'1' = ifelse(x < 1, 3, ifelse(x < 10, 2, ifelse(x < 1000, 1, 0))),
		stop(paste("Rule:", shQuote(rule), "not yet implemented."))
	)
	return(nDecRule)
}

#' Get number of decimals based on the data in a numeric vector.
#' Note: NA is returned if the element is missing (NA).
#' @param x Numeric vector.
#' @return Numeric vector of same length than \code{x}
#' with the number of decimals.
#' @author Laure Cougnaud
#' @examples
#' x <- c(0.99, 5.679, 50.45, 1450)
#' getNDecimalsData(x)
#' @family decimals
#' @export
getNDecimalsData <- function(x){
	
	if(!is.numeric(x))
		stop("'x' should be numeric.")
	
	xNumber <- ifelse(
		is.na(x), 
		NA_character_,
		sub("-*[[:digit:]]+\\.*([[:digit:]]+)*", 
			"\\1", 
			as.character(sapply(x, format, scientific = FALSE)
			)
		)
	)
	nDec <- nchar(xNumber)
	return(nDec)
}

#' Get maximum number of decimals in a variable based on the data
#' (\code{\link{getNDecimalsData}})
#' @inheritParams getNDecimalsData
#' @return Integer with maximum number of decimals in a character vector.
#' @author Laure Cougnaud
#' x <- c(0.99, 5.679, 50.45, 1450)
#' # extract max number of decimals based on data:
#' getMaxNDecimalsData(x)
#' @family decimals
#' @export
getMaxNDecimalsData <- function(x)
	return(max(getNDecimalsData(x), na.rm = TRUE))

#' Format a percentage.
#' 
#' The following rules are used:
#' \itemize{
#' \item{percentage = 0\%: }{'0'}
#' \item{ 0\% < percentage < 0.1\%: }{'<0.1'}
#' \item{99.9\% < percentage < 100\%: }{'>99.9'}
#' \item{percentage = 100\%: }{'100'}
#' \item{missing value (NA) (class without valid data): }{'-'}
#' \item{other: }{'x.x' (1 decimal)}
#' }
#' @param x Numeric vector with percentage(s)
#' @param nDec Integer of length 1, number
#' of decimals used to round the percentage,
#' 1 by default.
#' @return String with formatted percentage
#' @author Laure Cougnaud
#' @examples
#' xPerc <- c(NA, 0, 100, 99.95, 0.012, 34.768)
#' formatPercentage(x = xPerc)
#' @family decimals
#' @importFrom clinUtils roundHalfUpTextFormat
#' @export
formatPercentage <- function(x, nDec = 1){
	
	xRF <- ifelse(is.na(x),
		"-",
		ifelse(x == 0,
			"0", 
			ifelse(x == 100,
				"100",
				ifelse(x < 0.1,
					"<0.1",
					ifelse(x > 99.9,
						">99.9",
                        roundHalfUpTextFormat(x, digits = nDec)
					)
				)
			)
		)
	)
	return(xRF)
	
}
