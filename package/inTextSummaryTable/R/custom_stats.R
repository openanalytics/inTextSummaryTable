#' Extract default set of statistics.
#' 
#' This set of statistics is passed directly to the \code{stats} parameter
#' of the \code{\link{computeSummaryStatisticsTable}} function.
#' @param type Character vector with type of statistics (multiple are possible):
#' \itemize{
#' \item{'summary': }{all statistics for 'summaryTable' (\code{type} parameter)}
#' \item{'count': }{all statistics for 'countTable' (\code{type} parameter)}
#' \item{'n (\%)': }{number of subjects (percentage)}
#' \item{'median (range)': }{median (minimum, maximum)}
#' \item{'median\\n(range)': }{median and (minimum, maximum) below (linebreak)}
#' \item{'mean (se)': }{mean and standard deviation}
#' \item{'mean (range)': }{mean and (minimum, maximum)}
#' }
#' @param includeName Logical, should the statistics name be included (TRUE by default)?
#' This is applied for the statistic names used in each for the set defined in \code{type};
#' and for the label of the list if \code{type} is of length 2.
#' If there are multiple \code{type} or statistics within a set, the names are retained (to avoid confusion).
#' @param x (optional, recommended for continuous variable) Numeric vector 
#' for which the statistics should be computed on 
#' (has an effect only for continuous variable).
#' If specified, this is used to derive the number of decimals to include.
#' If not specified, the values are rounded with \code{\link{formatC}}.
#' @param nDecCont Integer with base number of decimals 
#' for continuous variable, or function returning this number based on \code{x} 
#' (\code{\link{getNDecimals}} by default).
#' @param nDecN,nDecm Integer with number of decimals for number of subjects/records (0 by default).
#' @param formatPercentage Function used to format the percentage of the number of subjects
#' (see \code{\link{formatPercentage}} for default behaviour).
#' @return (Optionally named) list of expression or call object containing
#' function to extract summary statistics.
#' If multiple \code{type} are specified, they are combined to a list.
#' @examples
#' ## default set of statistics are available for:
#' 
#' # for count table:
#' getStats("count")
#' getStats("n (%)")
#' # for continuous variable:
#' getStats("summary")
#' getStats("mean (se)")
#' getStats("median (range)")
#' getStats("median\n(range)")
#' 
#' ## to not include statistic name in the table
#' getStats("median\n(range)", includeName = FALSE)
#' getStats(c("summary", "median\n(range)"), includeName = FALSE)
#' 
#' ## to extract the number of decimals based on a continuous variable (see ?getMaxNDecimals) 
#' library(glpgUtilityFct)
#' data(ADaMDataPelican)
#' getStats(type = c('median (range)', 'mean (se)'), x = ADaMDataPelican$ADSL$WEIGHTBL)
#' # compare with when 'x' is not specified:
#' getStats(type = c('median (range)', 'mean (se)'))
#' @author Laure Cougnaud
#' @export
getStats <- function(
	type = "summary",
	includeName = TRUE,
	x = NULL, 
	nDecCont = getMaxNDecimals,
	nDecN = 0, nDecm = nDecN,
	formatPercentage = formatPercentage
){
	
	type <- match.arg(
		type,
		choices = c(
			"summary", "count", "n (%)", 
			"median (range)", "median\n(range)",
			"mean (se)", "mean (range)"
		),
		several.ok = TRUE
	)
	
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
				Mean = quote(formatC(statMean)),
				Median = quote(formatC(statMedian)),
				SD = quote(formatC(statSD)),
				SE = quote(formatC(statSE)),
				Min = quote(formatC(statMin)),
				Max = quote(formatC(statMax))
			)
		}
	)
	
	stats <- c(
		if("summary" %in% type)	
			statsBase[c("n", "Mean", "SD", "SE", "Median", "Min", "Max", "%", "m")],
		if("count" %in% type)	statsBase[c("n", "%", "m")],
		if('n (%)' %in% type)
			list('n (%)' = 
				bquote(
					ifelse(
						is.na(statPercN), "-",
						ifelse(statN == 0, "0", 
							paste0(.(statsBase$n), " (", .(statsBase$`%`), ")")
						)
					)
				)
			),
		if('median (range)' %in% type)	
			list('Median (range)' = 
				bquote(paste0(
					.(statsBase$Median), 
					" (", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			),
		if('median\n(range)' %in% type)
			list('Median\n(range)' = 
				bquote(paste0(
					.(statsBase$Median), 
					"\n(", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			),
		if('mean (se)' %in% type)
			list('Mean (SE)' = 
				bquote(paste0(.(statsBase$Mean), " (", .(statsBase$SE),  ")"))
			),
		if('mean (range)' %in% type)
			list('Mean (range)' = 
				bquote(paste0(
					.(statsBase$Mean), 
					" (", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			)
	)
	
	if(any(duplicated(names(type))))
		warning("Returned statistics have duplicated names (you probably included multiple 'type' statistics).",
			"Be sure to provide only one statistic with the same name to the in-text table functions.")
	
	if(!includeName){
		if(length(stats) == 1){
			stats <- unname(stats)
		}else	warning("The labels for the different types:", toString(sQuote(type)), "are retained, to avoid confusion.")
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
