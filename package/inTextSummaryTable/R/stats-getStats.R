#' Get default set of statistics for variables of interest and specific dataset.
#' 
#' This set of statistics can be passed directly to the \code{stats} parameter
#' of the package functions.\cr
#' By default, statistics are extracted based on the variable(s) type
#' and formatted with the default rules implemented in the package.
#' @param type Character vector with type of statistics to extract, among:
#' \itemize{
#' \item{'default': }{default sets of statistics, 
#' see types: 'summary-default' and 'count-default' in \code{\link{getStats}}}
#' \item{'all': }{all computed statistics, see types: 'summary' and 'count' in \code{\link{getStats}}}
#' \item{any formatted statistics as implemented in \code{\link{getStats}}, 
#' see section 'Formatted statistics' in
#' \code{\link[=inTextSummaryTable-stats]{in-text table statistics}}.
#' }
#' }
#' To specify statistics for a continuous (numeric) or categorical
#' variable separately, this vector can be named with: 'cont' or
#' 'cat' respectively (elements not named are used for both continuous 
#' and categorical variables).
#' @param var (optional, recommended for continuous variable)
#' Character vector with variable(s) of data, to
#' compute statistics on.\cr
#' If NULL (by default), counts of the entire dataset are computed.\cr
#' It is passed to the \code{x} parameter of \code{\link{getStats}}.
#' @param extra List with extra statistics to include, or function to apply on each
#' \code{var} (e.g. depending on the class of \code{var}) to get such statistic.
#' @param args (optional) Named list with extra arguments for 
#' \code{\link{getStats}} for continuous (name: 'cont') or 
#' categorical variable (name: 'cat') specifically. 
#' @param ... Extra parameters passed to the \code{\link{getStats}} function
#' (independent of the variable type).
#' @inheritParams inTextSummaryTable-common-args
#' @return List with statistics to compute, named by \code{var}
#' @seealso \link{getStats}
#' @examples 
#' # default set of statistics (depending if the variable is continuous or categorical)
#' exampleData <- data.frame(
#'   USUBJID = 1 : 4, 
#'   WEIGHT = c(67, 78, 83, 61), 
#'   SEX = c("F", "M", "M", "F"), 
#'   stringsAsFactors = FALSE
#' )
#' getStatsData(data = exampleData, var = c("WEIGHT", "SEX"))
#' # all set of statistics (depending if the variable is continuous or categorical)
#' getStatsData(data = exampleData, var = c("WEIGHT", "SEX"), type = "all")
#' # custom set of statistics for all variables
#' getStatsData(data = exampleData, var = c("WEIGHT", "SEX"), type = c("n", "%"))
#' # custom set of statistics, depending on the type of the variable
#' getStatsData(data = exampleData, var = c("WEIGHT", "SEX"), 
#'   type = c(cont = "median (range)", cont = "mean (se)", cat = "n (%)"), 
#'   args = list(cat = list(includeName = FALSE))
#' )
#' @author Laure Cougnaud
#' @export
getStatsData <- function(
	data, 
	var = NULL, 
	type = "default", 
	extra = NULL, 
	args = NULL,
	...){
	
	argsGeneral <- list(...)
	getArgsGetStats <- function(var = NULL){
		
		isNumVar <- !is.null(var) && var != "all" && is.numeric(data[, var])
		
		# get type of the variable
		if(!is.null(names(type))){
			if(any(!names(type) %in% c("", "cont", "cat")))
				stop("Only: 'cont', 'cat' and '' (empty) are allowed for the names of the 'type' parameter.")
			if(isNumVar){
				type <- type[names(type) %in% c("", "cont")]
			}else	type <- type[names(type) %in% c("", "cat")]
		}
		if("all" %in% type){
			type[match("all", type)] <- ifelse(isNumVar, "summary", "count")
		}else	if("default" %in% type){
			type[match("default", type)] <- ifelse(isNumVar, "summary-default", "count-default")
		}
		type <- unname(type)
		
		# get extra variable
		argsVar <- if(isNumVar)	args[["cont"]]	else	args[["cat"]]
		argsGetStats <- c(
			list(type = type),
			argsVar,
			if(!is.null(var) &&	var != "all")	list(x = data[, var]),
			list(...)
		)
		stats <- do.call(getStats, argsGetStats)
		return(stats)
		
	}
	
	getExtra <- function(var = NULL){
		if(!is.null(var) && var == "all")	var <- NULL
		if(!is.null(extra)){
			listExtra <- lapply(extra, function(extraEl){
				if(is.function(extraEl))	extraEl(data[, var])	else	extraEl
			})
			listExtra <- listExtra[!sapply(listExtra, is.null)]
		}
	}
		
	if(is.null(var)){
		stats <- getArgsGetStats()
		stats <- c(stats, getExtra())
	}else{
		stats <- sapply(var, function(varI){
			statsVar <- getArgsGetStats(var = varI)
			c(statsVar, getExtra(var = varI))
		}, simplify = FALSE)
		
	}
	
	return(stats)

}

#'  Get default set of statistics for one particular variable.
#' 
#' This set of statistics can be passed directly to the \code{stats} parameter
#' of the of the package functions.
#' @param type Character vector with type of statistics (multiple are possible).
#' Available statistics are specified in the section 'Formatted statistics' and
#' formatting in 'Statistics formatting' in
#' \code{\link[=inTextSummaryTable-stats]{in-text table statistics}}.
#' @param includeName Logical, should the statistics name be included (TRUE by default)?
#' This is applied for the statistic names used in each for the set defined in \code{type};
#' and for the label of the list if \code{type} is of length 2.
#' If there are multiple \code{type} or statistics within a set, the names are retained (to avoid confusion).
#' @param x (optional, recommended for continuous variable) Numeric vector 
#' for which the statistics should be computed on.\cr
#' This is used to derive the number of decimals to include
#' for a continuous variable.\cr
#' If not specified, the values are rounded with \code{\link{formatC}}.
#' @param nDecCont Integer with base number of decimals 
#' for continuous variable, or function returning this number based on \code{x} 
#' (\code{\link{getNDecimals}} by default).
#' @param nDecN,nDecm Integer with number of decimals 
#' for number of subjects/records (0 by default).
#' @param formatPercentage Function used to format the percentages
#' (see \code{\link{formatPercentage}} for default behaviour).
#' @return Expression (or call object) containing
#' function to extract requested summary statistics.
#' If multiple \code{type} are specified, they are combined to a list.
#' Names of the list will be typically used to name the statistic
#' in the summary table.
#' @examples
#' ## default set of statistics are available for:
#' 
#' # for count table:
#' getStats("count")
#' getStats("n (%)")
#' getStats("n")
#' getStats("%")
#' getStats("m")
#' getStats("%m")
#' getStats("m (%)")
#' # for continuous variable:
#' getStats("summary")
#' getStats("mean (se)")
#' getStats("mean (sd)")
#' getStats("median (range)")
#' getStats("median\n(range)")
#' getStats(c("Mean", "SE"))
#' 
#' ## to not include statistic name in the table
#' getStats("median\n(range)", includeName = FALSE)
#' getStats(c("summary", "median\n(range)"), includeName = FALSE)
#' 
#' ## to extract the number of decimals based on a continuous variable (see ?getMaxNDecimals) 
#' exampleData <- data.frame(
#'   USUBJID = 1 : 4, 
#'   WEIGHT = c(67, 78, 83, 61), 
#'   SEX = c("F", "M", "M", "F"), 
#'   stringsAsFactors = FALSE
#' )
#' getStats(type = c('median (range)', 'mean (se)'), x = exampleData$WEIGHT)
#' # compare with when 'x' is not specified:
#' getStats(type = c('median (range)', 'mean (se)'))
#' 
#' ## custom function to format the percentages:
#' getStats(type = "count", formatPercentage = function(x) round(x, 2))
#' @seealso \link{getStatsData}
#' @author Laure Cougnaud
#' @importFrom clinUtils roundHalfUpTextFormat
#' @export
getStats <- function(
	type = "summary",
	includeName = TRUE,
	x = NULL, 
	nDecCont = getMaxNDecimals,
	nDecN = 0, nDecm = nDecN,
	formatPercentage = inTextSummaryTable:::formatPercentage
){
	
	# number of decimals for continuous variable
	nDecContBase <- if(is.function(nDecCont) & !is.null(x) & is.numeric(x)){
		nDecCont(x)
	}else	if(is.numeric(nDecCont))	nDecCont	
	
	statsBase <- c(
		# statistics for categorical variable
		list(
			# counts
			n = bquote(roundHalfUpTextFormat(statN, .(nDecN))),
			m = bquote(roundHalfUpTextFormat(statm, .(nDecm))),
			# percentage
			`%` = substitute(
				formatPercentage(statPercN), 
				list(formatPercentage = formatPercentage)
			),
			`%m` = substitute(
				formatPercentage(statPercm), 
				list(formatPercentage = formatPercentage)
			)
		),
		# statistics for continuous variable
		if(!is.null(nDecContBase)){
			list(
				Mean = bquote(roundHalfUpTextFormat(statMean, .(nDecContBase + 1))),
				Median = bquote(roundHalfUpTextFormat(statMedian, .(nDecContBase + 1))),
				SD = bquote(roundHalfUpTextFormat(statSD, .(nDecContBase + 1))),
				SE = bquote(roundHalfUpTextFormat(statSE, .(nDecContBase + 2))),
				Min = bquote(roundHalfUpTextFormat(statMin, .(nDecContBase))),
				Max = bquote(roundHalfUpTextFormat(statMax, .(nDecContBase)))		
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
			"n (%)", "n/N (%)",
			"m (%)",
			"median (range)", "median\n(range)",
			"mean (se)", "mean (sd)", "mean (range)",
			"(min, max)",
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
			`m (%)` = list('m (%)' = 
				bquote(
					ifelse(
						is.na(statPercm), "-",
						ifelse(statm == 0, "0",
							paste0(.(statsBase$m), " (", .(statsBase$`%m`), ")")
						)
					)
				)
			),
			`n/N (%)` = list('n/N (%)' = bquote(
					ifelse(
						is.na(statPercN), "-",
							ifelse(statN == 0, "0",
								paste0(.(statsBase$n), "/",  statPercTotalN, " (", .(statsBase$`%`), ")")
							)
					)
				)
			),
			`(min, max)` = list('(Min, Max)' =
				bquote(paste0(
					"(", .(statsBase$Min), ", ", .(statsBase$Max), ")"
				))
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
			`mean (sd)` = list('Mean (SD)' = 
				bquote(paste0(.(statsBase$Mean), " (", .(statsBase$SD),  ")"))
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
		}else	warning(paste("The labels for the different types:", 
			toString(sQuote(type)), "are retained, to avoid confusion."))
	}
	
	return(stats)
	
}


