#'Create custom statistic sets to be passed to the \code{stats} parameter
#' of the \code{\link{computeSummaryStatisticsTable}} function.
#' @param type String with type of statistics:
#' \itemize{
#' \item{'summary': }{all statistics for 'summaryTable' (\code{type} parameter)}
#' \item{'count': }{all statistics for 'countTable' (\code{type} parameter)}
#' \item{'n (\%)': }{number of subjects with 0 digit (percentage with 1 digits)}
#' \item{'median (range)': }{median (minimum, maximum)}
#' \item{'median\n(range)': }{median and (minimum, maximum) below (linebreak)}
#' }
#' @param includeName Logical, should the statistics name be included (TRUE by default)?
#' This is only available if an unique statistic is specified.
#' @param x (optional, recommended) Vector for which the statistics should be computed on.
#' If specified, this is used to derive the number of decimals to include.
#' If not specified, the values are rounded with \code{\link{formatC}}.
#' @param nDecCont Integer with base number of decimals 
#' for continuous variable, or function returning this number based on \code{x} 
#' (\code{\link{getNDecimals}} by default).
#' @param nDecN,nDecm Integer with number of decimals for number of subjects/records (0 by default).
#' @param formatPercentage Function used to format the percentage of the number of subjects
#' (see \code{\link{formatPercentage}} for default behaviour).
#' @return (Optionally named) list of expression or call object of summary statistics of interest
#' @author Laure Cougnaud
#' @export
getStats <- function(
	type = c(
		"summary", "count", "n (%)", 
		"median (range)", "median\n(range)",
		
	),
	includeName = TRUE,
	x = NULL, 
	nDecCont = getNDecimals,
	nDecN = 0, nDecm = nDecN,
	formatPercentage = formatPercentage
){
	
	type <- match.arg(type)
	
	# number of decimals for continuous variable
	nDecContBase <- if(is.function(nDecCont) & !is.null(x)){
		nDecCont(x)
	}else	if(is.numeric(nDecCont))	nDecCont	
	
	statsBase <- c(
		# statistics for categorical variable
		list(
			# counts
			n = bquote(roundCustomText(statN, .(nDecN))),
			m = bquote(roundCustomText(statm, .(nDecm))),
			# percentage
			`%` = bquote(formatPercentage(statPercN))
		),
		# statistics for continuous variable
		if(!is.null(nDecContBase)){
			list(
				Mean = bquote(roundCustomText(statMean, .(nDecContBase + 1))),
				Median = bquote(roundCustomText(statMedian, .(nDecContBase + 1))),
				SD = bquote(roundCustomText(statSD, .(nDecContBase + 1))),
				SE = bquote(roundCustomText(statSE, .(nDecContBase + 2))),
				Min = bquote(roundCustomText(statMin, .(nDecContBase))),
				Max = bquote(roundCustomText(statMax, .(nDecContBase)))		
			)
		}else{
			list(
				Mean = expression(formatC(statMean)),
				Median = expression(formatC(statMedian)),
				SD = expression(formatC(statSD)),
				SE = expression(formatC(statSE)),
				Min = expression(formatC(statMin)),
				Max = expression(formatC(statMax))
			)
		}
	)
	
	stats <- switch(
		type,
		summary = statsBase[c("n", "Mean", "SD", "SE", "Median", "Min", "Max", "%", "m")],
		count = statsBase[c("n", "%", "m")],
		'n (%)' = list('n (%)' = paste0(statsBase$n, " (", statsBase$`%`, ")")),
		'median (range)' = list('Median (range)' =
			expression(paste0(statsBase$Median, " (", statsBase$Min, ",",  statsBase$Max, ")"))
		),
		'median\n(range)' = list('Median\n(range)' =
			expression(paste0(statsBase$Median, "\n(", statsBase$Min, ",",  statsBase$Max, ")"))
		),
		'mean (se)' = list('Mean (SE)' = expression(paste0(statsBase$Mean, "\n(", statsBase$SE, ")")))
	)
	
	if(!includeName){
		if(length(stats) > 1){
			warning("The name of the statistics is included, ",
				"because multiple statistics are present.")
		}else	stats <- unname(stats)
	}
	
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

#' Compute percentage coefficient of variation.
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
