#' Format summary statistics table for export
#' @param summaryTable Summary table, created with the \code{\link{computeSummaryStatisticsTable}} function.
#' @param colVar Character vector with variable(s) used for the columns.
#' If multiple variables are specified, the variables should be sorted in hierarchical order,
#' and are included in multi-columns layout.
#' @param rowVar Character vector with variable(s) used for the rows.
#' If multiple variables are specified, the variables should be sorted in hierarchical order.
#' The variables are included in rows, excepted if specified in \code{rowVarInSepCol}. 
#' @param rowVarInSepCol Variable(s) of \code{rowVar} which should be 
#' included in separated column in the table, NULL by default. 
#' This is only available if \code{rowVar} if not specified.
#' Note that the row total (if \code{rowTotalInclude} is TRUE) is computed 
#' separately by this variable.
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @param rowTotalLab label for the row with total
#' @param rowAutoMerge Logical, if TRUE (by default) automatically merging of rows,
#' e.g. in case there is only one sub-category (e.g. categorical variable with only one group)
#' or only one statistic per category
#' @param statsLayout String with layout for the statistics names 
#' (in case more than one statistic is included), among:
#' \itemize{
#' \item{row: }{Statistics are included in rows in the first column of the table}
#' \item{'col': }{Statistics are included in columns (last row of the header).
#' This option is not compatible with categorical variable(s).}
#' \item{'rowInSepCol': }{Statistics are included in rows, but in a separated column than
#' the \code{rowVar} variable(s)}
#' }
#' @param colHeaderTotalInclude Logical, if TRUE include the total of number of patients
#' (\code{'statN'}) in the header.
#' @param statsValueLab String with label for the statistic value, 
#' in case no \code{colVar} is specified: 'StatisticValue' by default.
#' @param emptyValue Value used to fill the table for missing values.
#' See the \code{fill} parameter of the \code{\link[reshape2]{dcast}} function.
#' @param rowVarTotalInSepRow Character vector with \code{rowVarTotalInclude}
#' (not in \code{rowVarInSepCol}) for which the total should be included in a separated row labelled 'Total'.
#' Otherwise (by default) the total is included in the header row of each category.
#' @inheritParams subjectProfileSummaryPlot
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
#' vertical and horizontal lines}
#' }
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of corresponding summary tables in long format.
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom plyr colwise
#' @importFrom stats as.formula
formatSummaryStatisticsTable <- function(
	summaryTable,
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = NULL,
	# total
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"), 
	rowVarTotalInSepRow = NULL,
	rowTotalLab = NULL,
	
	rowAutoMerge = TRUE,
	colVar = getAttribute(summaryTable, "colVar"),
	colTotalLab = getAttribute(summaryTable, "colTotalLab", default = "Total"),
	colHeaderTotalInclude = TRUE,
	labelVars = NULL,
	statsLayout = c("row", "col", "rowInSepCol"),
	statsValueLab = "StatisticValue",
	emptyValue = NULL
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
	
	statsVar <- attributes(summaryTable)$summaryTable$statsVar
	if(statsLayout == "col" & "variableGroup" %in% rowVar){
		warning("The layout of the statistics cannot be 'column' if categorical variable(s) are used, the statistics are set in rows.")
		statsLayout <- "row"
	}
		
	## format table
	
	if(!is.null(colVar) & colHeaderTotalInclude){
		
		# add total in column header
		colVarWithCount <- colVar[length(colVar)] 
		
		dataWithTotal <- ddply(summaryTable, colVar, function(x){
			idxTotal <- which(x$isTotal)
			if(length(idxTotal) == 1){
				# for the total column, include the N in all columns (to be merged afterwards)
				colToModif <- if(all(x[, colVar] == colTotalLab))	colVar	else	colVarWithCount
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
		
	# convert from wide to long format
	statsVar <- if(is.null(statsVar)){
		setdiff(colnames(dataWithTotal),  
			c(rowVar, colVar, "variable", "variableGroup", "isTotal")
		)
	}else{statsVar}
	dataLong <- melt(dataWithTotal, 
		id.vars = c(rowVar, colVar),
		measure.vars = statsVar,
		value.name = statsValueLab,
		variable.name = "Statistic"
	)
	
	emptyStats <- which(is.na(dataLong[, statsValueLab]))
	if(length(emptyStats) > 0)
		dataLong <- dataLong[-emptyStats, ]
	
	# if only one 'stats' and no named, set 'Statistic' to NA 
	# e.g. in DM table: count per sub-group for categorical variable
	if(!is.null(rowVar)){
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
	
	getTotalRow <- function(data)
		which(rowSums(data[, rowVar] == "Total") == length(rowVar))
	
	hlineParams <- mergeParams <- NULL
	mergeRows <- !is.null(rowVar) | (statsLayout != "rowInSepCol" & length(statsVar) > 1)
	if(mergeRows){
		
		rowVarUsed <- c(rowVar, if(statsLayout == "row" & length(statsVar) > 1)	"Statistic")
		
		# important: sort data.frame with specified row variables!
		dataLong <- ddply(dataLong, rowVar)
		if(".id" %in% colnames(dataLong))	dataLong$`.id` <- NULL
		
		# in case than rowVar are factor, can have issues to include additional rows (*invalid factor*), so convert them as character
		rowVarUsedFact <- names(which(sapply(dataLong[, rowVarUsed, drop = FALSE], is.factor)))
		if(length(rowVarUsedFact) > 0)
			dataLong[, rowVarUsedFact] <- colwise(.fun = as.character)(dataLong[, rowVarUsedFact, drop = FALSE])
	
		# if more than one rowVar, convert them to different rows
		rowVarInRow <- setdiff(rowVarUsed, rowVarInSepCol) # row variables to merge
		rowVarFinal <- rowVarInRow[length(rowVarInRow)] # final column = more nested row variable
		rowVarToModify <- rowVarInRow[-length(rowVarInRow)] # variables to merge
		
		if(length(rowVarToModify) > 0){
			
			# save initial value of rowVarFinal for later
			dataLong$rowVarFinal <- dataLong[, rowVarFinal]
			
			dataLong$rowPadding <- rowPadding <- length(rowVarInRow)-1
			idxTotalRow <- getTotalRow(data = dataLong)
			dataLong[idxTotalRow, "rowPadding"] <- 0
				
			# include the value in each rowVar in the column of the more nested variable
			# start by the second more nested variable
			for(i in rev(seq_along(rowVarToModify))){
					
				var <- rowVarToModify[i]
				varX <- dataLong[, var]
				
				## add new rows
				
				# get indices of rows to replicates
				dataVarI <- dataLong[, rowVarToModify[seq_len(i)]]
				# fix in case value in one column is NA
				# -> interaction is set to NA (without concatenating other columns)
				dataVarI[is.na(dataVarI)] <- ""
				idxRowToRepl <- which(!duplicated(interaction(dataVarI))) 
				idxRowToRepl <- setdiff(idxRowToRepl, idxTotalRow)
				
				# if the sub-total for this variable is computed and not included in separated row
				varNested <- rowVarInRow[match(var, rowVarInRow)+1]
				if(varNested %in% rowVarTotalInclude & !varNested %in% rowVarTotalInSepRow){
					
					# include the variable in the final column
					dataLong[idxRowToRepl, "rowPadding"] <- rowPadding <- rowPadding - 1
					if(any(dataLong[idxRowToRepl, rowVarFinal] != "Total"))
						stop("Missing total sub-category")
					dataLong[idxRowToRepl, rowVarFinal] <- dataLong[idxRowToRepl, var]
					
				}else{
					
					# new element:
					# convert to character in case is a factor
					val <- as.character(varX[idxRowToRepl])
					isNAVal <- which(is.na(val))
					if(length(isNAVal) > 0){
						val <- val[-isNAVal]
						idxRowToRepl <- idxRowToRepl[-isNAVal]
					}
					
					dataLong <- dataLong[sort(c(idxRowToRepl, seq_len(nrow(dataLong)))), ]
					# fill columns
					idxRowToRepl <- idxRowToRepl + seq_along(idxRowToRepl)-1 # indices of replicates rows in new df
					# set var element in final row column
					dataLong[idxRowToRepl, rowVarFinal] <- val #ifelse(is.na(val) | val == "", "Non specified", val)
					# and set to rest to NA
					dataLong[idxRowToRepl, !colnames(dataLong) %in% c(rowVarFinal, rowVarToModify)] <- NA
					# save the padding for flextable
					dataLong[idxRowToRepl, "rowPadding"] <- rowPadding <- rowPadding - 1
				
				}
									
			}
			
			# if only one element in last nested rowVar (e.g. only one Statistic, or counts with categories)
			# merge with previous row (concatenate values)
			if(rowAutoMerge && statsLayout != "rowInSepCol" && (length(statsVar) > 1 | "variableGroup" %in% rowVar)){
				idxNARVF <- which(!is.na(dataLong$rowVarFinal))
				idxMore2El <- which(diff(idxNARVF) == 1) # consecutive NAs
				if(length(idxMore2El) > 0)
					idxNARVF <- idxNARVF[-unique(c(idxMore2El, idxMore2El+1))]
				if(length(idxNARVF) > 0){
					# get value of only element (next row)
					dataNARVF <- dataLong[idxNARVF, ] 
					# for final rowVar, concatenate the different values
					dataNARVF[, rowVarFinal] <- paste(
						dataLong[idxNARVF-1, rowVarFinal], 
						dataLong[idxNARVF, rowVarFinal]
					)
					# get the padding value of the last row
					dataNARVF[, "rowPadding"] <- dataLong[idxNARVF-1, "rowPadding"]
					dataLong[idxNARVF-1, ] <- dataNARVF
					dataLong <- dataLong[-idxNARVF, ]
				}
			}
			
			# merge rows, e.g. unique statistic for a subgroup are fill in the sub-group directly
			colContentTable <- setdiff(colnames(dataLong), c(rowVar, rowVarFinal, "rowVarFinal", "rowPadding"))
			idxIsNA <- which(is.na(dataLong[, rowVarFinal]))
			if(length(idxIsNA) > 0){
				# check if the table content to fill is all NA
				if(any(!is.na(dataLong[idxIsNA-1, colContentTable])))
					stop("Issue while merging rows.")
				dataIsNa <- dataLong[idxIsNA, ]
				dataIsNa[, c("rowPadding", rowVarFinal)] <- dataLong[idxIsNA-1,  c("rowPadding", rowVarFinal)]
				dataLong[idxIsNA-1, ] <- dataIsNa
				dataLong <- dataLong[-idxIsNA, ] 
			}
			
			dataLong[, c(rowVarToModify, "rowVarFinal")] <- rownames(dataLong) <- NULL
			
			# order columns (in case rowVarInSepCol and row stats)
			rowColsFinal <- c(rowVarFinal, rowVarInSepCol)
			dataLong <- dataLong[, c(rowColsFinal, setdiff(colnames(dataLong), rowColsFinal))]
			
			# adjust padding:
			# in case missing element in one of the nested variable
			# padding should be reduced
			dataLong$rowPadding <- smoothPadding(pad = dataLong$rowPadding)
			
			# save indices of rows to set padding in flextable
			padParams <- lapply(setdiff(unique(dataLong$rowPadding), 0), function(pad)
				list(i = which(dataLong$rowPadding == pad), j = 1, part = "body", padding.left = pad)				
			)
			
		}else	padParams <- list()
	
		## extract extra parameters for flextable (including header)
		
		## extract horizontal lines
		# rows with indent set to 0 (and no lines if all rows have no indent)
		idxHLine <- if(length(rowVarToModify) > 0 & !all(dataLong$rowPadding == 0))
			which(dataLong$rowPadding == 0)-1
		idxHLine <- unique(idxHLine[idxHLine > 0])
		if(!is.null(rowVarTotalInclude)){
			idxRowTotal <- which(dataLong[, rowVarFinal] == "Total" & dataLong$rowPadding == 0)
			idxHLine <- c(idxHLine, idxRowTotal[length(idxRowTotal)]) # + include horizontal line after row total
			if(is.null(rowTotalLab))	rowTotalLab <- paste("Any", rowVarLab[rowVarFinal])
			dataLong[idxRowTotal, rowVarFinal] <- rowTotalLab
		}
		
		dataLong$rowPadding <- NULL
		if(length(idxHLine) > 0)
			hlineParams <- c(hlineParams, list(
				list(i = idxHLine, part = "body", j = 1:ncol(dataLong))
			))
		for(var in rowVarInSepCol[-length(rowVarInSepCol)]){
			i <- which(diff(as.numeric(factor(dataLong[, var], exclude = ""))) != 0)
			if(length(i) > 0)
				hlineParams <- c(hlineParams, list(
					list(
						i = i, 
						part = "body", 
						j = seq(from = which(colnames(dataLong) == var), to = ncol(dataLong))
					)
				))
		}
		
		# merge rows for rowVarInSepCol
		for(var in rowVarInSepCol){
			varPrev <- intersect(rowVar[seq_len(match(var, rowVar))], colnames(dataLong))
			varPrevBin <- convertVectToBinary(interactionCustom(dataLong[, varPrev, drop = FALSE])$x)
			idx <- which(diff(varPrevBin) == 0)
			if(length(idx) > 0){
				idxFact <- cut(seq_along(idx), breaks = c(-Inf, which(diff(idx) != 1), Inf))
				for(l in levels(idxFact)){
					i <- idx[idxFact == l]
					i <- c(i, max(i)+1)
					mergeParams <- c(mergeParams, list(list(
						i = i,
						j = match(var, colnames(dataLong)),
						part = "body"
					)))
				}
			}
		}
			
		# label header for rows
		rowVarLabs <- c(
			rowVarLab[setdiff(rowVarInRow, "Statistic")], 
			if(statsLayout == "row")	rowVarLab["Statistic"]
		)
		colnames(dataLong)[match(rowVarFinal, colnames(dataLong))] <- headerRow <-
			paste(rowVarLabs, collapse = "_")
		colnames(dataLong)[match(rowVarInSepCol, colnames(dataLong))] <- headerRowVarInSepCol <- rowVarLab[rowVarInSepCol]
		
	}
	
	# extract header (in case multiple 'colVar' specified)
	header <- sapply(colnames(dataLong), function(x){
		res <- strsplit(x, split = "_")[[1]]
		# remove empty header
		res[!res %in% c("", "NA")] #formatSummaryStatisticsTable
	}, simplify = FALSE)
	nRowsHeader <- max(sapply(header, length))
	# in less elements in one column, replicate the first element (to have it merged in final ft)
	headerDf <- as.data.frame(
		do.call(cbind, 
			lapply(header, function(x)	c(rep(x[1], nRowsHeader - length(x)), x))
		)
	)
	colnames(headerDf) <- colnames(dataLong)
	attributes(dataLong)$summaryTable$header <- headerDf
	
	if(!is.null(hlineParams))
		attributes(dataLong)$summaryTable$hlineParams <- hlineParams
	
	if(!is.null(mergeParams))
		attributes(dataLong)$summaryTable$mergeParams <- mergeParams
	
	# extract vertical lines (specified by the right border)
	vLineParams <- lapply(seq_len(nrow(headerDf)-1), function(i){
		idx <- diff(as.numeric(factor(unlist(headerDf[i, ]), exclude = "")))
		j <- which(idx != 0)
		if(!all(idx == 0))	list(i = i:nrow(headerDf), part = "header", j = j)
	})
	if(length(vLineParams) > 0)
		vLineParams <- c(vLineParams, list(list(j = vLineParams[[length(vLineParams)]]$j, part = "body")))
	attributes(dataLong)$summaryTable$vlineParams <- vLineParams
	
	if(mergeRows){
		
		if(length(headerRow) > 0 && headerRow != ""){
		
			# save padding of header
			idxRowHeaderForPad <- which(headerDf[, headerRow] != "")[-1] # consider header for row column
			padParams <- c(
				padParams,
				lapply(seq_along(idxRowHeaderForPad), function(i)
					list(i = idxRowHeaderForPad[i], j = 1, part = "header", padding.left = i)
				)
			)
			
			attributes(dataLong)$summaryTable$rowVar <- headerRow
			
		}
		
		if(length(padParams) > 0)
			attributes(dataLong)$summaryTable$padParams <- padParams

	}
	
	return(dataLong)
	
}


#' Smooth padding, e.g. remove padding bigger than 1
#' @param pad Integer vector with padding.
#' @return Integer vector with 'smooth' padding.
#' @author Laure Cougnaud
smoothPadding <- function(pad){

	# extract indices in vector which have padding > 1 with previous row
	idxPaddingTooBig <- which(diff(pad) > 1)+1
	while(length(idxPaddingTooBig) > 0){
		# if next rows have same padding, include also their indices
		idxPad <- lapply(idxPaddingTooBig, function(i){
			idxExtra <- which(cumsum(abs(diff(pad[seq(from = i, to = length(pad))]))) == 0) + i
			idx <- c(i, idxExtra)
		})
		# set padding to padding of previous row
		pad[unlist(idxPad)] <- rep(pad[idxPaddingTooBig-1]+1, times = sapply(idxPad, length))
		idxPaddingTooBig <- which(diff(pad) > 1)
	}
	
	return(pad)

}