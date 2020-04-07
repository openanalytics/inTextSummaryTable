#' Format summary statistics table for export
#' @param summaryTable Summary table, created with the \code{\link{computeSummaryStatisticsTable}} function.
#' @param colHeaderTotalInclude Logical, if TRUE include the total of number of patients
#' (\code{'statN'}) in the header.
#' @param statsValueLab String with label for the statistic value, 
#' 'StatisticValue' by default.
#' This is only used if the statistics provided in \code{stats} are not named
#' and if no \code{colVar} is specified.
#' @param emptyValue Value used to fill the table for missing values, '-' by default.
#' See the \code{fill} parameter of the \code{\link[reshape2]{dcast}} function.
#' @inheritParams subjectProfileSummaryPlot
#' @inheritParams formatSummaryStatisticsTableFlextable
#' @inheritParams computeSummaryStatisticsTable
#' @return summaryTable reformatted to wide format
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom plyr colwise
#' @importFrom dplyr n_distinct
#' @importFrom stats as.formula
#' @importFrom glpgStyle convertVectToBinary
formatSummaryStatisticsTable <- function(
	summaryTable,
	# row
	rowVar = getAttribute(summaryTable, "rowVar"),
	# column
	colVar = getAttribute(summaryTable, "colVar"),
	colTotalLab = getAttribute(summaryTable, "colTotalLab", default = "Total"),
	colHeaderTotalInclude = TRUE,
	# stats
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = "row",
	statsValueLab = "StatisticValue",
	emptyValue = "-"){
		
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(summaryTable, function(summaryTableI){
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			do.call(formatSummaryStatisticsTable, inputParamsBy)		
		}, simplify = FALSE)	
		return(res)
		
	}
		
	statsLayout <- match.arg(statsLayout, choices = c("row", "col", "rowInSepCol"))
		
	## format table
	if("isTotal" %in% colnames(summaryTable)){
		
		if(!is.null(colVar) & colHeaderTotalInclude){
			
			# add total in column header
			colVarWithCount <- colVar[length(colVar)] 
			
			dataWithTotal <- ddply(summaryTable, colVar, function(x){
				idxTotal <- which(x$isTotal)
				if(length(idxTotal) == 1){
					# for the total column, include the N in all columns (to be merged afterwards)
					colToModif <- if(all(!is.na(x[, colVar])) && all(x[, colVar] == colTotalLab))	colVar	else	colVarWithCount
					for(col in colToModif)
						x[, col] <- paste0(x[, col], "\n(N=",  x[idxTotal , "statN"], ")")
					x[-idxTotal, ]
				}else x
			})
		
			# ensure that order of columns with Total is as specified in levels of the factor originally
			for(col in colVar){
				colVarWithCountEl <- unique(dataWithTotal[, col])
				colVarInit <-  summaryTable[, col]
				colVarEl <- if(is.factor(colVarInit))	levels(colVarInit)	else	unique(colVarInit)	
				colVarWithCountElOrdered <- colVarWithCountEl[
					order(match(sub("(.+)\n\\(N=.+\\)", "\\1", colVarWithCountEl), colVarEl))
				]
				dataWithTotal[, col] <- factor(dataWithTotal[, col], levels = colVarWithCountElOrdered)
			}
	
		}else{
			idxTotal <- which(summaryTable$isTotal)
			nTotal <- summaryTable[idxTotal, "statN"]
			dataWithTotal <- summaryTable[-idxTotal, ]
		}
		
	}else{
		dataWithTotal <- summaryTable
		nTotal <- NA
	}
	
	# convert from wide to long format
	statsVar <- if(is.null(statsVar)){
		setdiff(colnames(dataWithTotal),  
			c(rowVar, colVar, "variable", "variableGroup", "isTotal")
		)
	}else{statsVar}

	if(statsValueLab == "Statistic")
		stop("'statsValueLab' should be different than 'Statistic'.")
	
	# in case only one statistic is computed and 'stats' was named
	# retain the original name, otherwise use statsValueLab
	statsValueNewName <- if(length(statsVar) == 1 && statsVar != "Statistic"){
		statsVar
	}else	statsValueLab
	dataLong <- melt(
		data = dataWithTotal, 
		id.vars = c(rowVar, colVar),
		measure.vars = statsVar,
		value.name = statsValueNewName,
		variable.name = "Statistic"
	)
	
	if(length(statsVar) == 1 && n_distinct(dataLong$Statistic) == 1)	dataLong$Statistic <- NULL
	
	emptyStats <- which(is.na(dataLong[, statsValueNewName]))
	if(length(emptyStats) > 0)
		dataLong <- dataLong[-emptyStats, ]
	
	# if only one 'stats' and no named, set 'Statistic' to NA 
	# e.g. in DM table: count per sub-group for categorical variable
	if(!is.null(rowVar) & "Statistic" %in% colnames(dataLong)){
		idxUniqueStatNotNamed <- which(!duplicated(dataLong[, c(rowVar, colVar)]) & dataLong$Statistic == "Statistic")
		if(length(idxUniqueStatNotNamed) > 0)
			dataLong[idxUniqueStatNotNamed, "Statistic"] <- NA
	}
	
	# format statistic value
	if(is.numeric(dataLong[, statsValueNewName]))
		dataLong[, statsValueNewName] <- formatC(dataLong[, statsValueNewName])
	
	# put elements in 'colVar' in different columns (long -> wide format)
	if(!is.null(colVar) | (statsLayout == "col" & length(statsVar) > 1)){
		rowVarForm <- c(
			if(!is.null(rowVar)) paste(rowVar, collapse = " + "), 
			if(length(statsVar) > 1 & statsLayout != "col")	"Statistic"
		)
		if(is.null(rowVarForm))	rowVarForm <- "."
		colVarUsed <- c(colVar, if(length(statsVar) > 1 & statsLayout == "col")	"Statistic")
		formulaWithin <- as.formula(paste(
			paste(rowVarForm, collapse = "+"),
			"~", 
			paste(colVarUsed, collapse = " + ")
		))
		varsFm <- all.vars(formulaWithin)
		isDupl <- duplicated(dataLong[, varsFm])
		if(any(isDupl)){
			dataDupl <- merge(dataLong, dataLong[isDupl, varsFm, drop = FALSE])
			stop("Table formatting to multiple columns failed because of ",
				"duplicated records for each row/col:\n",
				paste(capture.output(print(dataDupl)), collapse = "\n")
			)
		}
		dataLong <- dcast(dataLong, formula = formulaWithin, 
			value.var = statsValueNewName, fill = emptyValue
		)
		if(all(rowVarForm == "."))	dataLong["."] <- NULL
	}else{
		if(colHeaderTotalInclude)
			colnames(dataLong)[match(statsValueNewName, colnames(dataLong))] <- 
				paste0(statsValueNewName, "\n(N=",  nTotal, ")")
	}
	
	attributes(dataLong)$summaryTable <- attributes(summaryTable)$summaryTable
	attributes(dataLong)$summaryTable$statsLayout <- statsLayout
	
	return(dataLong)
	
}