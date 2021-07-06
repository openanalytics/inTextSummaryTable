#' Format summary statistics table for export
#' @param colHeaderTotalInclude Logical, if TRUE include the total of number of patients
#' (\code{'statN'}) in the column header.
#' @param emptyValue String with placeholder used to fill the table for missing values, '-' by default.
#' This value is typically used e.g. if not all statistics are computed for all specified
#' row/col/var variables.
#' @inheritParams subjectProfileSummaryPlot
#' @inheritParams inTextSummaryTable-common-args
#' @return summaryTable reformatted to wide format
#' @author Laure Cougnaud
#' @importFrom clinUtils getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom plyr colwise
#' @importFrom dplyr n_distinct
#' @importFrom stats as.formula
#' @importFrom utils capture.output
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
	statsLabInclude = NULL,
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
			
			# fix in case 'colVar' contains space(s)
			colVarQuote <- if(!is.null(colVar))	paste0("`", colVar, "`")
			
			dataWithTotal <- ddply(summaryTable, colVarQuote, function(x){
				idxTotal <- which(x$isTotal)
				if(length(idxTotal) == 1){
					# for the total column, include the N in all columns (to be merged afterwards)
					colToModif <- if(all(!is.na(x[, colVar])) && all(x[, colVar] == colTotalLab))	colVar	else	colVarWithCount
					for(col in colToModif){
						x[, col] <- factor(paste0(x[, col], "\n(N=",  x[idxTotal , "statN"], ")"))
					}
					x[-idxTotal, ]
				}else x
			})
		
			# ensure that order of columns with Total is as specified in levels of the factor originally
			for(col in colVar){
				colVarWithCountEl <- levels(dataWithTotal[, col])
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
			if(colHeaderTotalInclude && length(nTotal) > 1)
				stop("Multiple values for the header total but",
					" no column variables ('colVar') are specified.")
			if(length(idxTotal) > 0){
				dataWithTotal <- summaryTable[-idxTotal, ]
			}else{
				dataWithTotal <- summaryTable
			}
		}
		
	}else{
		dataWithTotal <- summaryTable
		nTotal <- NA
	}
	
	# Note: best way would be to make this function works for
	# empty data.frame but dcast returns: 
	# 'Error in dim(ordered) <- ns : dims [product 1] do not match the length of object [0]'
	if(nrow(dataWithTotal) == 0){
		warning("No data remain after filtering of total rows.")
		return(invisible())
	}

	# convert from wide to long format
	if(is.null(statsVar)){
		statsVar <- setdiff(colnames(dataWithTotal),  
			c(rowVar, colVar, "variable", "variableGroup", "isTotal")
		)
	}

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
	
	# Is the label for the statistic required?
	isStatsLabRequired <- !(length(statsVar) == 1 && n_distinct(dataLong$Statistic) == 1)
	if(is.null(statsLabInclude)){
		statsLabInclude <- isStatsLabRequired
	}else	if(!statsLabInclude & isStatsLabRequired){
		warning(paste("Statistic label is included ('statsLabInclude' set to TRUE)",
			"because more than one statistic variable is available in the table."))
		statsLabInclude <- TRUE
	}
	
	if(!statsLabInclude)	dataLong$Statistic <- NULL
	
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
		rowVarFm <- c(
			if(!is.null(rowVar)) paste(paste0("`", rowVar, "`"), collapse = " + "), 
			if(statsLabInclude & statsLayout != "col")	"Statistic"
		)
		if(is.null(rowVarFm))	rowVarFm <- "."
		colVarFm <- c(colVar, if(statsLabInclude & statsLayout == "col")	"Statistic")
		colVarFm <- if(!is.null(colVarFm))	paste0("`", colVarFm, "`")
		formulaWithin <- as.formula(paste(
			paste(rowVarFm, collapse = "+"),
			"~", 
			paste(colVarFm, collapse = " + ")
		))
		varsFm <- all.vars(formulaWithin)
		varsFm <- setdiff(varsFm, ".")
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
		if(all(rowVarFm == "."))	dataLong["."] <- NULL
	}else{
		if(colHeaderTotalInclude)
			colnames(dataLong)[match(statsValueNewName, colnames(dataLong))] <- 
				paste0(statsValueNewName, "\n(N=",  nTotal, ")")
	}
	
	attributes(dataLong)$summaryTable <- attributes(summaryTable)$summaryTable
	attributes(dataLong)$summaryTable$statsLayout <- statsLayout
	attributes(dataLong)$summaryTable$statsLabInclude <- statsLabInclude
	
	return(dataLong)
	
}