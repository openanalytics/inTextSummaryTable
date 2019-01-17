#'Create custom statistic sets to be passed to the \code{stats} parameter
#' of the \code{\link{computeSummaryStatisticsTable}} function.
#' @param type String with type of statistics:
#' \itemize{
#' \item{'summary': }{all statistics for 'summaryTable' (\code{type} parameter)
#' \item{'count': }{all statistics for 'countTable' (\code{type} parameter)
#' \item{'n (%)':}{number of subjects with 0 digit (percentage with 1 digits)}
#' }
#' @return (Optionally named) list of expression or call object of summary statistics of interest
#' @author Laure Cougnaud
#' @export
getStats <- function(type = c("summary", "count", "n (%)")){
	
	type <- match.arg(type)
	
	stats <- switch(
		type,
		summary = list(
			n = expression(statN),
			Mean = expression(statMean),
			SD = expression(statSD),
			Median = expression(statMedian),
			Min = expression(statMin),
			Max = expression(statMax),
			`%` = expression(statPerc),
			m = expression(statm)
		),
		count = list(
			n = expression(statN),
			`%` = expression(statPerc),
			m = expression(statm)
		),
		'n (%)' = list(
			expression(paste0(roundCustom(statN, 0), " (", roundCustom(statPercN, 1), ")"))
		)
	)
	
	return(stats)
	
}

#' Compute standard error of the mean
#' @param x Numeric vector.
#' @param na.rm Logical, should NA value(s) be removed (FALSE by default)?
#' @return Numeric vector with standard error of the mean
#' @author Laure Cougnaud
#' @importFrom stats na.omit sd
#' @export
se <- function(x, na.rm = FALSE){
	if(na.rm)	 x <- na.omit(x)
	res <- sd(x)/sqrt(length(x))
	return(res)
}

#' Compute coefficient of variation.
#' @inheritParams se
#' @return Numeric vector of length 1 with coefficient of variation.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @export
cv <- function(x, na.rm = FALSE){
	
	res <- sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)*100
	return(res)
	
}

#' Compute geometric mean.
#' @inheritParams se
#' @return Numeric vector of length 1 with geometric mean.
#' @author Laure Cougnaud
#' @export
geomMean <- function(x, na.rm = FALSE){
	res <- exp(mean(log(x), na.rm = na.rm))
	return(res)
}

#' Compute geometric standard deviation
#' @inheritParams se
#' @return Numeric vector of length 1 with geometric mean.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @export
geomSD <- function(x, na.rm = FALSE){
	res <- exp(sd(log(x), na.rm = na.rm))
	return(res)
}

#' Compute geometric coefficient of variation.
#' @inheritParams se
#' @return Numeric vector of length 1 with geometric coefficient of variation.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @export
geomCV <- function(x, na.rm = FALSE){
	res <- sqrt(exp(sd(log(x), na.rm = na.rm)^2)-1)*100
	return(res)
}
