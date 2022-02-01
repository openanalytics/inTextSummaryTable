#' Merge nested rows of a summary table
#' for a format compatible with \code{flextable} 
#' @inheritParams inTextSummaryTable-common-args
#' @inheritParams inTextSummaryTable-flextable-args
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
#' @importFrom clinUtils getLabelVar
#' @author Laure Cougnaud
formatSummaryStatisticsTableFlextable <- function(
	summaryTable,
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarInSepCol = NULL, 
	rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
	statsLayout = "row", 
	statsVar = getAttribute(summaryTable, "statsVar"), 	
	statsLabInclude = getAttribute(summaryTable, "statsLabInclude", default = length(statsVar) > 1),
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarTotalInSepRow = NULL,
	vline = c("none", "auto"),
	hline = c("none", "auto"),
	rowAutoMerge = TRUE,
	rowVarFormat = NULL,
	rowTotalLab = NULL,
	labelVars = NULL){

	statsLayout <- match.arg(statsLayout, choices = c("row", "col", "rowInSepCol"))
		
	vline <- match.arg(vline)
	hline <- match.arg(hline)
	
	if(is.null(summaryTable)){
		return(invisible())
	}
	
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(summaryTable, function(summaryTableI){
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			do.call(formatSummaryStatisticsTableFlextable, inputParamsBy)		
		}, simplify = FALSE)	
		return(res)
		
	}
	
	if(length(rowVarTotalInclude) > 0){
		rowVarTotalWrongFormat <- names(which(sapply(
			summaryTable[, rowVarTotalInclude, drop = FALSE], function(x){
			!(is.factor(x) && levels(x)[1] == "Total")
		})))
		if(length(rowVarTotalWrongFormat) > 0){
			
			warning(paste(
				"Row variable(s):", toString(shQuote(rowVarTotalWrongFormat)),
				"with total should be formatted",
				"as factor with 'Total' as the first level,",
				"these variable(s) are not considered for total formatting."
			))
			
			rowVarTotalInclude <- setdiff(rowVarTotalInclude, rowVarTotalWrongFormat)
		}
	}
	
	hlineParams <- mergeParams <- formatParams <- NULL	
	
	mergeRows <- !is.null(rowVar) | (statsLayout != "rowInSepCol" & statsLabInclude)
	if(mergeRows){
	
		rowVarUsed <- c(rowVar, if(statsLayout == "row" & statsLabInclude)	"Statistic")
		
		# important: sort summaryTable.frame with specified row variables!
		summaryTable <- ddply(summaryTable, rowVar)
		if(".id" %in% colnames(summaryTable))	summaryTable$`.id` <- NULL
		
		# in case than rowVar are factor, can have issues to include additional rows (*invalid factor*), so convert them as character
		rowVarUsedFact <- names(which(sapply(summaryTable[, rowVarUsed, drop = FALSE], is.factor)))
		if(length(rowVarUsedFact) > 0)
			summaryTable[, rowVarUsedFact] <- colwise(.fun = as.character)(summaryTable[, rowVarUsedFact, drop = FALSE])
		
		# if more than one rowVar, convert them to different rows
		rowVarInRow <- setdiff(rowVarUsed, rowVarInSepCol) # row variables to merge
		rowVarFinal <- rowVarInRow[length(rowVarInRow)] # final column = more nested row variable
		rowVarToModify <- rowVarInRow[-length(rowVarInRow)] # variables to merge
		
		getTotalRow <- function(summaryTable){
			# if only variableGroup is specified, no total row
			rowVarForTotal <- setdiff(rowVar, c("variable", "variableGroup"))
			if(length(rowVarForTotal) > 0){
				which(rowSums(summaryTable[, rowVar, drop = FALSE] == "Total") == length(rowVar))
			}else	integer()
		}
		
		if(length(rowVarToModify) > 0){
			
			# save initial value of rowVarFinal for later
			summaryTable$rowVarFinal <- summaryTable[, rowVarFinal]
			
			summaryTable$rowPadding <- rowPadding <- length(rowVarInRow)-1
			idxTotalRow <- getTotalRow(summaryTable = summaryTable)
			summaryTable[idxTotalRow, "rowPadding"] <- 0
			
			# include the value in each rowVar in the column of the more nested variable
			# start by the second more nested variable
			for(i in rev(seq_along(rowVarToModify))){
				
				rowPadding <- rowPadding - 1
				
				var <- rowVarToModify[i]
				varX <- summaryTable[, var]
				
				## add new rows
				
				# get indices of rows to replicates
				dataVarI <- summaryTable[, rowVarToModify[seq_len(i)]]
				# fix in case value in one column is NA
				# -> interaction is set to NA (without concatenating other columns)
				dataVarI[is.na(dataVarI)] <- ""
				idxRowToMove <- which(!duplicated(interaction(dataVarI))) 
				idxRowToMove <- setdiff(idxRowToMove, idxTotalRow)
				
				# 1) the total should be included in the header row
				# ( == the sub-total for this variable is computed and not included in separated row)
				varNested <- rowVarInRow[match(var, rowVarInRow)+1]
				isVarTotalNotInSepRow <- varNested %in% rowVarTotalInclude & !varNested %in% rowVarTotalInSepRow
				if(isVarTotalNotInSepRow){
					
					# in case multiple records for total, add extra rows
					varNestedAll <- rowVarInRow[seq(from = match(var, rowVarInRow)+1, to = length(rowVarInRow))]
					idxTotalNested <- which(rowSums(summaryTable[, varNestedAll, drop = FALSE] == "Total") == length(varNestedAll))
					idxTotalNested <- idxTotalNested[which(diff(idxTotalNested) == 1) + 1]
					idxRowToMove <- sort(unique(c(idxRowToMove, idxTotalNested)))
					
					if(var == "variable" & varNested == "variableGroup"){
						
						# if variable total is included only for certain variables
						idxCorrect <- which(summaryTable[idxRowToMove, rowVarFinal] %in% c("Total", ""))
						idxRowToFill <- idxRowToMove[idxCorrect]
						idxRowToRepl <- setdiff(idxRowToMove[-idxCorrect], idxTotalNested)
						
					}else{
						if(any(summaryTable[idxRowToMove, rowVarFinal] != "Total"))
								stop("Missing total sub-category")
						idxRowToFill <- idxRowToMove
						idxRowToRepl <- integer()
					}
							
					summaryTable[idxRowToFill, rowVarFinal] <- summaryTable[idxRowToFill, var]
					summaryTable[idxRowToFill, "rowPadding"] <- rowPadding
					
				}else	idxRowToRepl <- idxRowToMove
				
				# 2) if total should not be included in the header: create new rows to include the header
				if(!isVarTotalNotInSepRow || length(idxRowToRepl) > 0){
					
					# new element:
					# convert to character in case is a factor
					val <- as.character(varX[idxRowToRepl])
					isNAVal <- which(is.na(val))
					if(length(isNAVal) > 0){
						val <- val[-isNAVal]
						idxRowToRepl <- idxRowToRepl[-isNAVal]
					}
					
					summaryTable <- summaryTable[sort(c(idxRowToRepl, seq_len(nrow(summaryTable)))), ]
					# fill columns
					idxRowToRepl <- idxRowToRepl + seq_along(idxRowToRepl)-1 # indices of replicates rows in new df
					# set var element in final row column
					summaryTable[idxRowToRepl, rowVarFinal] <- val #ifelse(is.na(val) | val == "", "Non specified", val)
					# and set to rest to NA
					summaryTable[idxRowToRepl, !colnames(summaryTable) %in% c(rowVarFinal, rowVarToModify)] <- NA
					# save the padding for flextable
					summaryTable[idxRowToRepl, "rowPadding"] <- rowPadding
					
				}
				
			}
			
			# if only one element in last nested rowVar (e.g. only one Statistic, or counts with categories)
			# merge with previous row (concatenate values)
			if(rowAutoMerge && statsLayout != "rowInSepCol" && (length(statsVar) > 1 | "variableGroup" %in% rowVar)){
				idxNARVF <- which(!is.na(summaryTable$rowVarFinal))
				idxMore2El <- which(diff(idxNARVF) == 1) # consecutive NAs
				if(length(idxMore2El) > 0)
					idxNARVF <- idxNARVF[-unique(c(idxMore2El, idxMore2El+1))]
				# doesn't consider the case if row with total is the first row already
				idxNARVF <- setdiff(idxNARVF, 1)
				if(length(idxNARVF) > 0){
					# get value of only element (next row)
					dataNARVF <- summaryTable[idxNARVF, ] 
					# for final rowVar, concatenate the different values
					dataNARVF[, rowVarFinal] <- paste(
						summaryTable[idxNARVF-1, rowVarFinal], 
						summaryTable[idxNARVF, rowVarFinal]
					)
					# get the padding value of the last row
					dataNARVF[, "rowPadding"] <- summaryTable[idxNARVF-1, "rowPadding"]
					summaryTable[idxNARVF-1, ] <- dataNARVF
					summaryTable <- summaryTable[-idxNARVF, ]
				}
			}
			
			# merge rows, e.g. unique statistic for a subgroup are fill in the sub-group directly
			idxIsNA <- which(is.na(summaryTable[, rowVarFinal]))
			if(length(idxIsNA) > 0){
				
				colContentTable <- setdiff(colnames(summaryTable), c(rowVar, rowVarFinal, "rowVarFinal", "rowPadding"))
				
				# first case: values are the same than in previous row
				# e.g. only total included (e.g. rest is NA)
				idxIsNaSameValueAsPreviousRow <- rowSums(
					summaryTable[idxIsNA-1, colContentTable, drop = FALSE] == 
						summaryTable[idxIsNA, colContentTable, drop = FALSE]
				) == length(colContentTable)
				
				# second case: table content to fill (previous row) is all NA
				idxIsNaButNotContent <- rowSums(is.na(summaryTable[idxIsNA-1, colContentTable, drop = FALSE])) == length(colContentTable)
				
				idxIsNaToRemove <- which(!idxIsNaButNotContent & !idxIsNaSameValueAsPreviousRow)
				
				# don't consider the case that it is NA, e.g. if subgroup in input summaryTable is NA
				if(length(idxIsNaToRemove) > 0)
					idxIsNA  <- idxIsNA[-idxIsNaToRemove]
				if(length(idxIsNA) > 0){
					dataIsNa <- summaryTable[idxIsNA, ]
					dataIsNa[, c("rowPadding", rowVarFinal)] <- summaryTable[idxIsNA-1,  c("rowPadding", rowVarFinal)]
					summaryTable[idxIsNA-1, ] <- dataIsNa
					summaryTable <- summaryTable[-idxIsNA, ] 
				}
			}
			
			summaryTable[, c(rowVarToModify, "rowVarFinal")] <- rownames(summaryTable) <- NULL
			
			# order columns (in case rowVarInSepCol and row stats)
			rowColsFinal <- c(rowVarFinal, rowVarInSepCol)
			summaryTable <- summaryTable[, c(rowColsFinal, setdiff(colnames(summaryTable), rowColsFinal))]
			
			# adjust padding:
			# in case missing element in one of the nested variable
			# padding should be reduced
			summaryTable$rowPadding <- smoothPadding(pad = summaryTable$rowPadding)
			
			# save indices of rows to set padding in flextable
			padParams <- lapply(setdiff(unique(summaryTable$rowPadding), 0), function(pad)
				list(i = which(summaryTable$rowPadding == pad), j = 1, part = "body", padding.left = pad)				
			)
			
		}else	padParams <- list()
		
		## extract extra parameters for flextable (including header)
		
		## extract horizontal lines
		
		# rows with indent set to 0 (and no lines if all rows have no indent)
		idxHLine <- if(length(rowVarToModify) > 0 & !all(summaryTable$rowPadding == 0))	which(summaryTable$rowPadding == 0)-1
		idxHLine <- unique(idxHLine[idxHLine > 0])
		
		# remove lines between rows with same indent (e.g. multiple total rows)
		if(length(rowVarInSepCol) > 0)
			idxHLine <- idxHLine[idxHLine %in% cumsum(rle(summaryTable[, rowVarFinal])$lengths)]
		
		# set the label for the total row (only if total is included for one of the original row total)
		if(!is.null(rowVarTotalInclude) && !all(rowVarTotalInclude %in% "variableGroup")){
			idxRowTotal <- which(summaryTable[, rowVarFinal] == "Total" & summaryTable$rowPadding == 0)
			idxHLine <- c(idxHLine, idxRowTotal[length(idxRowTotal)]) # + include horizontal line after row total
			if(is.null(rowTotalLab)){
				rowTotalLabVars <- rowVarLab[rowVarInRow]
				rowTotalLabVars <- rowTotalLabVars[!is.na(rowTotalLabVars)]
				rowTotalLab <- paste("Any", toString(rowTotalLabVars))
			}
			summaryTable[idxRowTotal, rowVarFinal] <- rowTotalLab
		}
		
		# extract special formatting
		# if rowVar to format has been merged, extract corresponding rows based on padding
		rowVarFormat <- rowVarFormat[names(rowVarFormat) %in% rowVar]
		for(var in names(rowVarFormat)){
			# if variable has been merged, first column and extract row indices based on padding
			if(var %in% rowVarInRow){
				if(length(rowVarToModify) > 0){
					pad <- match(var, rowVarInRow)-1
					i <- which(summaryTable$rowPadding == pad)
				}else{
					i <- seq_len(nrow(summaryTable))
				}
				j <- 1
			# otherwise, extract column idx and consider all rows
			}else{
				j <- match(var, colnames(summaryTable))
				i <- seq_len(nrow(summaryTable))
			}
			formatParams <- c(formatParams, 
				list(list(i = i, j = j, part = "body", type = unname(rowVarFormat[[var]])))
			)
		}	
		
		summaryTable$rowPadding <- NULL
		if(length(idxHLine) > 0)
			hlineParams <- c(hlineParams, list(
				list(i = idxHLine, part = "body", j = seq_len(ncol(summaryTable)))
			))
		for(var in rowVarInSepCol[-length(rowVarInSepCol)]){
			i <- which(diff(as.numeric(factor(summaryTable[, var], exclude = ""))) != 0)
			if(length(i) > 0)
				hlineParams <- c(hlineParams, list(
					list(
						i = i, 
						part = "body", 
						j = seq(from = which(colnames(summaryTable) == var), to = ncol(summaryTable))
					)
				))
		}
		
		# merge rows for rowVarInSepCol
		for(var in rowVarInSepCol){
			varPrev <- intersect(rowVar[seq_len(match(var, rowVar))], colnames(summaryTable))
			varPrevBin <- convertVectToBinary(interactionCustom(summaryTable[, varPrev, drop = FALSE])$x)
			idx <- which(diff(varPrevBin) == 0)
			if(length(idx) > 0){
				idxFact <- cut(seq_along(idx), breaks = c(-Inf, which(diff(idx) != 1), Inf))
				for(l in levels(idxFact)){
					i <- idx[idxFact == l]
					i <- c(i, max(i)+1)
					mergeParams <- c(mergeParams, list(list(
						i = i,
						j = match(var, colnames(summaryTable)),
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
		colnames(summaryTable)[match(rowVarFinal, colnames(summaryTable))] <- headerRow <-
			paste(rowVarLabs, collapse = "_")
		colnames(summaryTable)[match(rowVarInSepCol, colnames(summaryTable))] <- headerRowVarInSepCol <- rowVarLab[rowVarInSepCol]
		
	}
	
	# extract header (in case multiple 'colVar' specified)
	header <- sapply(colnames(summaryTable), function(x){
		res <- strsplit(x, split = "_")[[1]]
		# remove empty header
		res[!res %in% c("", "NA")] #formatSummaryStatisticsTable
	}, simplify = FALSE)
	nRowsHeader <- max(sapply(header, length))
	# in less elements in one column, replicate the first element (to have it merged in final ft)
	headerDf <- as.data.frame(
		do.call(cbind, 
			lapply(header, function(x){
				x1 <- if(length(x) == 0)	""	else	x[1] # fix in case var is empty
				c(rep(x1, nRowsHeader - length(x)), x)
			}
		))
	, stringsAsFactors = FALSE)
	colnames(headerDf) <- colnames(summaryTable)
	
	# attributes required when converting table to flextable:
	attributes(summaryTable)$summaryTable$header <- headerDf
	
	attributes(summaryTable)$summaryTable$hlineParams <- hlineParams
	attributes(summaryTable)$summaryTable$hline <- hline
	
	if(!is.null(mergeParams))
		attributes(summaryTable)$summaryTable$mergeParams <- mergeParams
	
	if(!is.null(formatParams))
		attributes(summaryTable)$summaryTable$formatParams <- formatParams
	
	# extract vertical lines (specified by the right border)
	if(vline == "auto"){
		vLineParams <- lapply(seq_len(nrow(headerDf)-1), function(i){
			idx <- diff(as.numeric(factor(unlist(headerDf[i, ]), exclude = "")))
			j <- which(idx != 0)
			if(!all(idx == 0))	list(i = i:nrow(headerDf), part = "header", j = j)
		})
		if(length(vLineParams) > 0)
			vLineParams <- c(vLineParams, list(list(j = vLineParams[[length(vLineParams)]]$j, part = "body")))
	}else	vLineParams <- NULL
	attributes(summaryTable)$summaryTable$vlineParams <- vLineParams
	attributes(summaryTable)$summaryTable$vline <- vline
	
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
			
			attributes(summaryTable)$summaryTable$rowVar <- headerRow
			
		}
		
		if(length(padParams) > 0)
			attributes(summaryTable)$summaryTable$padParams <- padParams
		
	}
	
	
	return(summaryTable)
	
}

#' Smooth padding, e.g. remove padding bigger than 1
#' @param pad Integer vector with padding.
#' @return Integer vector with 'smooth' padding.
#' @author Laure Cougnaud
#' @keywords internal
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