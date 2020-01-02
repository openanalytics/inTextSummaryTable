
#' Get default set of statistics for variables of interest and specific dataset
#' 
#' This set of statistics is passed directly to the \code{stats} parameter
#' of the \code{\link{computeSummaryStatisticsTable}} function.
#' @param data Data.frame with data.
#' @param var Character vector with variables of interest
#' @param type String with type of statistics to extract:
#' \itemize{
#' \item{'default': }{default sets of statistics, 
#' see types: 'summary-default' and 'count-default' in \code{\link{getStats}}}
#' \item{'all': }{all computed statistics, see types: 'summary' and 'count' in \code{\link{getStats}}}
#' }
#' @param extra List with extra statistics to include, or function to apply on each
#' \code{var} (e.g. depending on the class of \code{var}) to get such list.
#' @param ... parameters passed to the \code{\link{getStats}} function
#' @return List with statistics to compute, named by \code{var}
#' @examples 
#' library(glpgUtilityFct)
#' data(ADaMDataPelican)
#' getStatsData(data = ADaMDataPelican$ADSL, var = "WEIGHTBL")
#' @author Laure Cougnaud
#' @export
getStatsData <- function(
	data, var = NULL, type = c("default", "all"), 
	extra = NULL, ...){
	
	type <- match.arg(type)
	
	getType <- function(var = NULL){
		switch(type,
			'all' = ifelse(!is.null(var) && is.numeric(data[, var]), "summary", "count"),
			'default' = ifelse(!is.null(var) && is.numeric(data[, var]), "summary-default", "count-default")
		)
	}
	
	getExtra <- function(var = NULL){
		if(!is.null(extra)){
			listExtra <- lapply(extra, function(extraEl){
				if(is.function(extraEl))	extraEl(data[, var])	else	extraEl
			})
			listExtra <- listExtra[!sapply(listExtra, is.null)]
		}
	}
	
	if(is.null(var)){
		stats <- getStats(type = getType(), ...)
		stats <- c(stats, getExtra())
	}else{
		stats <- sapply(var, function(varI){
			typeVar <- getType(var = varI)
			statsVar <- getStats(type = typeVar, x = data[, varI], ...)
			c(statsVar, getExtra(var = varI))
		}, simplify = FALSE)
		
	}
	
	return(stats)

}

#'  Get default set of statistics for one particular variable.
#' 
#' This set of statistics is passed directly to the \code{stats} parameter
#' of the \code{\link{computeSummaryStatisticsTable}} function.
#' @param type Character vector with type of statistics (multiple are possible):
#' \itemize{
#' \item{'summary': }{all statistics for 'summaryTable' (\code{type} parameter)}
#' \item{'count': }{all statistics for 'countTable' (\code{type} parameter)}
#' \item{'summary-default': }{all statistics for 'summaryTable' (\code{type} parameter), 
#' excepted percentage and number of records}
#' \item{'count-default': }{all statistics for 'countTable' (\code{type} parameter), 
#' excepted number of records}
#' \item{'n (\%)': }{number of subjects (percentage)}
#' \item{'median (range)': }{median (minimum, maximum)}
#' \item{'median\\n(range)': }{median and (minimum, maximum) below (linebreak)}
#' \item{'mean (se)': }{mean and standard error}
#' \item{'mean (range)': }{mean and (minimum, maximum)}
#' \item{base statistics: }{
#' \itemize{
#' \item{'n': }{number of subjects}
#' \item{'\%': }{percentage of subjects}
#' \item{'Mean' (only for continuous variable): }{mean}
#' \item{'Median' (only for continuous variable): }{median}
#' \item{'SE' (only for continuous variable): }{standard error}
#' \item{'SD' (only for continuous variable): }{standard deviation}
#' \item{'Min' (only for continuous variable): }{minimum}
#' \item{'Max' (only for continuous variable): }{maximum}
#' }}
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
#' getStats("n")
#' getStats("%")
#' # for continuous variable:
#' getStats("summary")
#' getStats("mean (se)")
#' getStats("median (range)")
#' getStats("median\n(range)")
#' getStats(c("Mean", "SE"))
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
#' 
#' ## custom function to format the percentages:
#' getStats(type = "count", formatPercentage = function(x) round(x, 2))
#' @author Laure Cougnaud
#' @export
getStats <- function(
	type = "summary",
	includeName = TRUE,
	x = NULL, 
	nDecCont = getMaxNDecimals,
	nDecN = 0, nDecm = nDecN,
	formatPercentage = get("formatPercentage", envir = parent.frame())
){
	
	# number of decimals for continuous variable
	nDecContBase <- if(is.function(nDecCont) & !is.null(x) & is.numeric(x)){
		nDecCont(x)
	}else	if(is.numeric(nDecCont))	nDecCont	
	
	statsBase <- c(
		# statistics for categorical variable
		list(
			# counts
			n = bquote(roundCustomText(statN, .(nDecN))),
			m = bquote(roundCustomText(statm, .(nDecm))),
			# percentage
			`%` = substitute(
				formatPercentage(statPercN), 
				list(formatPercentage = formatPercentage)
			)
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
	
	type <- match.arg(
		arg = type,
		choices = c(
			"summary-default", "count-default", 
			"summary", "count", 
			"n (%)", 
			"median (range)", "median\n(range)",
			"mean (se)", "mean (range)",
			names(statsBase)
		),
		several.ok = TRUE
	)
	
	statsList <- sapply(type, function(typeI){
		switch(typeI,
			`summary-default` = statsBase[c("n", "Mean", "SD", "SE", "Median", "Min", "Max")],
			`count-default` = statsBase[c("n", "%")],
			`summary` = statsBase[c("n", "Mean", "SD", "SE", "Median", "Min", "Max", "%", "m")],
			`count` = statsBase[c("n", "%", "m")],
			`n (%)` = list('n (%)' = 
				bquote(
					ifelse(
						is.na(statPercN), "-",
						ifelse(statN == 0, "0", 
							paste0(.(statsBase$n), " (", .(statsBase$`%`), ")")
						)
					)
				)
			),
			`median (range)` = list('Median (range)' = 
				bquote(paste0(
					.(statsBase$Median), 
					" (", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			),
			`median\n(range)` = list('Median\n(range)' = 
				bquote(paste0(
					.(statsBase$Median), 
					"\n(", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			),
			`mean (se)` = list('Mean (SE)' = 
				bquote(paste0(.(statsBase$Mean), " (", .(statsBase$SE),  ")"))
			),
			`mean (range)` = list('Mean (range)' = 
				bquote(paste0(
					.(statsBase$Mean), 
					" (", .(statsBase$Min), ",", .(statsBase$Max), ")"
				))
			),
			if(typeI %in% names(statsBase))	statsBase[typeI]
		)
	}, simplify = FALSE)
	stats <- unlist(unname(statsList), recursive = FALSE)
	
	if(any(duplicated(names(stats))))
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
