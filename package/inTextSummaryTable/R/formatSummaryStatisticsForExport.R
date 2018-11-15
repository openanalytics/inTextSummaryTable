#' Format summary statistics table for export
#' @param summaryTable Summary table, created with the \code{\link{computeSummaryStatisticsTable}} function.
#' @param colVar Character vector with variable(s) used for the columns.
#' If multiple variables are specified, the variables should be sorted in hierarchical order,
#' and are included in multi-columns layout.
#' @param rowVar Character vector with variable(s) used for the rows.
#' If multiple variables are specified, the variables should be sorted in hierarchical order.
#' The variables are included in rows, excepted if specified in \code{rowVarInSepCol}. 
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @param rowTotalLab label for the row with total
#' @inheritParams subjectProfileSummaryPlot
#' @inheritParams computeSummaryStatisticsTable
#' @return summaryTable reformatted in long format, with extra attributes:
#' \itemize{
#' \item{'header': }{data.frame with header for each column}
#' \item{'padParams': }{list of list of parameters to be passed to the 
#' \code{\link[flextable]{padding}} function}
#' \item{'rowVar': }{column of output with row variable}
#' \item{'vlineParams' and 'hlineParams': }{
#' list of list with correspondingly parameters for
#' vertical and horizontal lines}
#' }
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom plyr colwise
#' @importFrom stats as.formula
formatSummaryStatisticsTable <- function(
	summaryTable,
	rowVar = attributes(summaryTable)$rowVar, 
	rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL,
	rowTotalInclude = FALSE, rowTotalLab = NULL,
	rowSubtotalInclude = FALSE, 
	colVar = NULL,
	labelVars = NULL
	){
		
	## format table
	
	if(!is.null(colVar)){
		
		# add total in column header
		colVarWithCount <- colVar[length(colVar)] 
		
		dataWithTotal <- ddply(summaryTable, colVar, function(x){
			idxTotal <- which(x$isTotal)
			if(length(idxTotal) == 1){
				x[, colVarWithCount] <- paste0(x[, colVarWithCount], "\n(N=",  x[idxTotal , "N"], ")")
				x[-idxTotal, ]
			}else x
		})
	
		# ensure that order of columns with Total is as specified in levels of the factor originally
		colVarWithCountEl <- unique(dataWithTotal[, colVarWithCount])	
		colVarInit <-  summaryTable[, colVarWithCount]
		colVarEl <- if(is.factor(colVarInit))	levels(colVarInit)	else	unique(colVarInit)	
		colVarWithCountElOrdered <- colVarWithCountEl[
			order(match(sub("(.+)\n\\(N=.+\\)", "\\1", colVarWithCountEl), colVarEl))
		]
		dataWithTotal[, colVarWithCount] <- factor(dataWithTotal[, colVarWithCount], levels = colVarWithCountElOrdered)

	}else{
		idxTotal <- which(summaryTable$isTotal)
		nTotal <- summaryTable[idxTotal, "N"]
		dataWithTotal <- summaryTable[-idxTotal, ]
	}
		
	# convert from wide to long format
	statsVar <- if(is.null(attributes(summaryTable)$statsVar))
		setdiff(colnames(dataWithTotal),  c(rowVar, colVar, "variable", "variableGroup", "isTotal"))	else	
		attributes(summaryTable)$statsVar
	dataLong <- melt(dataWithTotal, 
		id.vars = c(rowVar, colVar),
		measure.vars = statsVar,
		value.name = "StatisticValue",
		variable.name = "Statistic"
	)
	
	emptyStats <- which(is.na(dataLong$StatisticValue))
	if(length(emptyStats) > 0)
		dataLong <- dataLong[-emptyStats, ]
	
	# format statistic value
	if(is.numeric(dataLong$StatisticValue))
		dataLong$StatisticValue <- formatC(dataLong$StatisticValue)
	
	# put elements in 'colVar' in different columns (long -> wide format)
	if(!is.null(colVar)){
		rowVarForm <- c(
			if(!is.null(rowVar)) paste(rowVar, collapse = " + "), 
			if(length(statsVar) > 1)	"Statistic"
		)
		if(is.null(rowVarForm))	rowVarForm <- "."
		formulaWithin <- as.formula(paste(
			paste(rowVarForm, collapse = "+"),
			"~", 
			paste(colVar, collapse = " + ")
		))
		dataLong <- dcast(dataLong, formula = formulaWithin, value.var = "StatisticValue")
		if(all(rowVarForm == "."))	dataLong["."] <- NULL
	}else{
		colnames(dataLong)[match("StatisticValue", colnames(dataLong))] <- 
			paste0("StatisticValue\n(N=",  nTotal, ")")
	}
	
	getTotalRow <- function(data)
		which(rowSums(data[, rowVar] == "Total") == length(rowVar))
	
	if(!is.null(rowVar)){
		
		# sort data.frame with specified row variables
		dataLong <- ddply(dataLong, rowVar)
		
		# in case than rowVar are factor, can have issues to include additional rows (*invalid factor*), so convert them as character
		rowVarFact <- names(which(sapply(dataLong[, rowVar, drop = FALSE], is.factor)))
		if(length(rowVarFact) > 0)
			dataLong[, rowVarFact] <- colwise(.fun = as.character)(dataLong[, rowVarFact, drop = FALSE])
	
		# if more than one rowVar, convert them to different rows
		rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
		rowVarFinal <- rowVarInRow[length(rowVarInRow)]
		rowVarToModify <- rowVarInRow[-length(rowVarInRow)]
		if(length(rowVarToModify) > 0){
			
			dataLong$rowPadding <- rowPadding <- length(rowVarInRow)-1
			if(rowTotalInclude)
				dataLong[getTotalRow(data = dataLong), "rowPadding"] <- 0
				
			for(i in rev(seq_along(rowVarToModify))){
					
				var <- rowVarToModify[i]
				varX <- dataLong[, var]
				
				# add new rows
				idxRowToRepl <- which(!duplicated(interaction(dataLong[, rowVarToModify[seq_len(i)]]))) # indices of rows to replicates
				if(rowTotalInclude)
					idxRowToRepl <- setdiff(idxRowToRepl, getTotalRow(data = dataLong))
				
				if(!rowSubtotalInclude){
					
					dataLong <- dataLong[sort(c(idxRowToRepl, seq_len(nrow(dataLong)))), ]
					# fill columns
					idxNewRow <- idxRowToRepl + seq_along(idxRowToRepl)-1 # indices of replicates rows in new df
					# set var element in final row column
					# convert to character in case is a factor
					val <- as.character(varX[idxRowToRepl])
					dataLong[idxNewRow, rowVarFinal] <- ifelse(is.na(val) | val == "", "Non specified", val)
					# and set to rest to NA
					dataLong[idxNewRow, !colnames(dataLong) %in% c(rowVarFinal, rowVarToModify)] <- NA
					# save the padding for flextable
					dataLong[idxNewRow, "rowPadding"] <- rowPadding <- rowPadding - 1
				
				}else{
					# include the variable in the final column
					dataLong[idxRowToRepl, "rowPadding"] <- rowPadding <- rowPadding - 1
					dataLong[idxRowToRepl, rowVarFinal] <- dataLong[idxRowToRepl, var]					
				}
									
			}
			
			dataLong[, rowVarToModify] <- rownames(dataLong) <- NULL
			
			# save indices of rows to set padding in flextable
			padParams <- lapply(setdiff(unique(dataLong$rowPadding), 0), function(pad)
				list(i = which(dataLong$rowPadding == pad), j = 1, part = "body", padding.left = pad)				
			)
			
		}else	padParams <- list()
	
		## extract extra parameters for flextable (including header)
		
		# extract horizontal lines
		idxHLine <- if(length(statsVar) > 1){
			which(diff(as.numeric(factor(dataLong[, rowVarFinal], exclude = ""))) != 0)
		}else{
			if(length(rowVarToModify) > 0){
				rowsDiffPad <- which(diff(dataLong$rowPadding) != 0)
				if(!rowSubtotalInclude){
					rowsDiffPad
				}else{
					c(
						rowsDiffPad,
						which(dataLong$rowPadding == 0) - 1 # all rows with sub-total
					)
				}
			}else if(length(rowVarInSepCol) > 0){
				which(diff(as.numeric(factor(dataLong[, rowVarFinal], exclude = ""))) != 0)
			}
		}
		idxHLine <- unique(idxHLine[idxHLine > 0])
		
		if(rowTotalInclude){
			idxRowTotal <- which(dataLong[, rowVarFinal] == "Total")
			idxHLine <- c(idxHLine, idxRowTotal[length(idxRowTotal)])
			if(is.null(rowTotalLab))	rowTotalLab <- paste("Any", rowVarLab[rowVarFinal])
			dataLong[idxRowTotal, rowVarFinal] <- rowTotalLab
		}
		
		dataLong$rowPadding <- NULL
		if(length(idxHLine) > 0)
			attributes(dataLong)$hlineParams <- list(
				list(i = idxHLine, part = "body", j = 1:ncol(dataLong))
			)
			
		# label header for rows
		colnames(dataLong)[match(rowVarFinal, colnames(dataLong))] <- headerRow <-
			paste(rowVarLab[rowVarInRow], collapse = "_")
		colnames(dataLong)[match(rowVarInSepCol, colnames(dataLong))] <- rowVarLab[rowVarInSepCol]
		
	}
	
	# extract header (in case multiple 'colVar' specified)
	header <- strsplit(colnames(dataLong), split = "_")
	nRowsHeader <- max(sapply(header, length))
	headerDf <- as.data.frame(
		do.call(cbind, 
			lapply(header, function(x)	c(rep("", nRowsHeader - length(x)), x))
		)
	)
	colnames(headerDf) <- colnames(dataLong)
	attributes(dataLong)$header <- headerDf
	
	# extract vertical lines (specified by the right border)
	attributes(dataLong)$vlineParams <- c(
		list(
			list(i = nRowsHeader, part = "header", j = 1:ncol(dataLong))
		),
		lapply(seq_len(nrow(headerDf)-1), function(i){
			j <- which(diff(as.numeric(factor(unlist(headerDf[i, ])))) == 1)
			list(i = i, part = "header", j = j)
		})
	)
	
	if(!is.null(rowVar)){
		
		if(length(headerRow) > 0 && headerRow != ""){
		
			# save padding of header
			idxRowHeaderForPad <- which(headerDf[, headerRow] != "")[-1] # consider header for row column
			padParams <- c(
				padParams,
				lapply(seq_along(idxRowHeaderForPad), function(i)
					list(i = idxRowHeaderForPad[i], j = 1, part = "header", padding.left = i)
				)
			)
			
			attributes(dataLong)$rowVar <- headerRow
			
		}
		
		if(length(padParams) > 0)
			attributes(dataLong)$padParams <- padParams

	}
	
	return(dataLong)
	
}