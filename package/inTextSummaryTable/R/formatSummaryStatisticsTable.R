#' Format summary statistics table for export
#' @param summaryTable Summary table, created with the \code{\link{computeSummaryStatisticsTable}} function.
#' @param rowTotalLab label for the row with total
#' @param rowAutoMerge Logical, if TRUE (by default) automatically merging of rows,
#' e.g. in case there is only one sub-category (e.g. categorical variable with only one group)
#' or only one statistic per category
#' @param colHeaderTotalInclude Logical, if TRUE include the total of number of patients
#' (\code{'statN'}) in the header.
#' @param statsValueLab String with label for the statistic value, 
#' in case no \code{colVar} is specified: 'StatisticValue' by default.
#' @param emptyValue Value used to fill the table for missing values, '-' by default.
#' See the \code{fill} parameter of the \code{\link[reshape2]{dcast}} function.
#' @param rowVarFormat Named list with special formatting for the \code{rowVar}.
#' Currently, only possibility is to set the variable elements in bold, with:
#' list(var1 = "bold").
#' (Use 'variable' for \code{var} or 'variableGroup' for group within categorical variables.)
#' @param vline String mentioning how vertical lines should be included, either: 
#' \itemize{
#' \item{'none' (default): }{no vertical lines included}
#' \item{'auto': }{vertical lines included between sub-groups}
#' }
#' @inheritParams subjectProfileSummaryPlot
#' @inheritParams mergeSummaryTableRowsFt
#' @inheritParams computeSummaryStatisticsTable
#' @return summaryTable reformatted in long format, with extra attributes:
#' \itemize{
#' \item{'header': }{data.frame with header for each column}
#' \item{'padParams': }{list of list of parameters to be passed to the 
#' \code{\link[flextable]{padding}} function}
#' \item{'rowVar': }{column of output with row variable}
#' \item{'rowVarInSepCol': }{column(s) of output with row variable in separated column(s)}
#' \item{'vlineParams' and 'hlineParams': }{
#' list of list with correspondingly parameters for
#' vertical and horizontal lines
#' }
#' \item{'vline': }{\code{vline} parameter}
#' \item{'formatParams': }{list of list with special formatting for the table,
#' currently only used if \code{rowVarFormat} if specified.}
#' }
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of corresponding summary tables in long format.
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
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = NULL,
	# total
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"), 
	rowVarTotalInSepRow = getAttribute(summaryTable, "rowVarTotalInSepRow"),
	rowTotalLab = NULL,
	# column
	colVar = getAttribute(summaryTable, "colVar"),
	colTotalLab = getAttribute(summaryTable, "colTotalLab", default = "Total"),
	colHeaderTotalInclude = TRUE,
	labelVars = NULL,
	# stats
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = c("row", "col", "rowInSepCol"),
	statsValueLab = "StatisticValue",
	emptyValue = "-",
	outputType = c("flextable", "DT", "data.frame"),
	vline = c("none", "auto"),
	rowAutoMerge = TRUE,
	rowVarFormat = NULL,
	...
	){
		
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(summaryTable, function(summaryTableI){
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			do.call(formatSummaryStatisticsTable, inputParamsBy)		
		}, simplify = FALSE)	
		return(res)
		
	}
		
	statsLayout <- match.arg(statsLayout)
	
	if(statsLayout == "col" & "variableGroup" %in% rowVar){
		warning("The layout of the statistics cannot be 'column' if categorical variable(s) are used, the statistics are set in rows.")
		statsLayout <- "row"
	}
		
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
		
	# ID variables
	idVars <- c(rowVar, colVar)
	
	# convert from wide to long format
	statsVar <- if(is.null(statsVar)){
		setdiff(colnames(dataWithTotal),  
			c(rowVar, colVar, "variable", "variableGroup", "isTotal")
		)
	}else{statsVar}

	if(statsValueLab == "Statistic")
		stop("'statsValueLab' should be different than 'Statistic'.")
	
	dataLong <- melt(dataWithTotal, 
		id.vars = idVars,
		measure.vars = statsVar,
		value.name = statsValueLab,
		variable.name = "Statistic"
	)
	
	if(length(statsVar) == 1 && n_distinct(dataLong$Statistic) == 1)	dataLong$Statistic <- NULL
	
	emptyStats <- which(is.na(dataLong[, statsValueLab]))
	if(length(emptyStats) > 0)
		dataLong <- dataLong[-emptyStats, ]
	
	# if only one 'stats' and no named, set 'Statistic' to NA 
	# e.g. in DM table: count per sub-group for categorical variable
	if(!is.null(rowVar) & "Statistic" %in% colnames(dataLong)){
		idxUniqueStatNotNamed <- which(!duplicated(dataLong[, c(rowVar, colVar)]) & dataLong$Statistic == "Statistic")
		dataLong[idxUniqueStatNotNamed, "Statistic"] <- NA
	}
	
	# format statistic value
	if(is.numeric(dataLong[, statsValueLab]))
		dataLong[, statsValueLab] <- formatC(dataLong[, statsValueLab])
	
	# put elements in 'colVar' in different columns (long -> wide format)
	if(!is.null(colVar) | statsLayout == "col"){
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
		dataLong <- dcast(dataLong, formula = formulaWithin, 
			value.var = statsValueLab, fill = emptyValue
		)
		if(all(rowVarForm == "."))	dataLong["."] <- NULL
	}else{
		if(colHeaderTotalInclude)
			colnames(dataLong)[match(statsValueLab, colnames(dataLong))] <- 
				paste0(statsValueLab, "\n(N=",  nTotal, ")")
	}
	
	res <- switch(outputType,
			
		'data.frame' = {
			
			dataLong
			
		},
		
		'flextable' = {
		
			dataLong <- formatSummaryStatisticsTableFt(
				data = dataLong,
				rowVar = rowVar, rowVarInSepCol = rowVarInSepCol, rowVarTotalInclude = rowVarTotalInclude,
				statsLayout = statsLayout, statsVar = statsVar, 
				rowVarLab = rowVarLab,
				vline = vline,
				rowAutoMerge = rowAutoMerge, rowVarFormat = rowVarFormat,
				rowVarTotalInSepRow = rowVarTotalInSepRow
			)
			
		}
		
	)
	
	return(res)
	
}