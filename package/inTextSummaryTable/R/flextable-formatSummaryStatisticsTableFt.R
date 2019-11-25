#' Merge nested rows of a summary table
#' for a format compatible with \code{flextable} 
#' @param data data.frame with data
#' @param rowVar Character vector with variable(s) used for the rows.
#' If multiple variables are specified, the variables should be sorted in hierarchical order.
#' The variables are included in rows, excepted if specified in \code{rowVarInSepCol}. 
#' @param rowVarInSepCol Variable(s) of \code{rowVar} which should be 
#' included in separated column in the table, NULL by default. 
#' To include the groups within a \code{var} variable in a separated column, set: rowVarInSepCol == 'variableGroup'.
#' This is only available if \code{rowVar} if not specified.
#' Note that the row total (if \code{rowTotalInclude} is TRUE) is computed 
#' separately by this variable.
#' @inheritParams computeSummaryStatisticsTable
#' @param statsLayout String with layout for the statistics names 
#' (in case more than one statistic is included), among:
#' \itemize{
#' \item{row: }{Statistics are included in rows in the first column of the table}
#' \item{'col': }{Statistics are included in columns (last row of the header).
#' This option is not compatible with categorical variable(s).}
#' \item{'rowInSepCol': }{Statistics are included in rows, but in a separated column than
#' the \code{rowVar} variable(s)}
#' }
#' @param statsVar Character vector with columns of \code{summaryTable} with
#' statistic variables.
#' @param rowVarLab Label for the \code{rowVar} variable(s).
#' @inheritParams computeSummaryStatisticsTable
#' @return updated \code{data}
#' @author Laure Cougnaud
formatSummaryStatisticsTableFt <- function(data,
	rowVar, rowVarInSepCol, rowVarTotalInclude,
	statsLayout, statsVar, rowVarLab,
	rowVarTotalInSepRow = NULL,
	vline = c("none", "auto"),
	rowAutoMerge = TRUE,
	rowVarFormat = NULL){
		
	vline <- match.arg(vline)
	
	hlineParams <- mergeParams <- formatParams <- NULL	
	
	mergeRows <- !is.null(rowVar) | (statsLayout != "rowInSepCol" & length(statsVar) > 1)
	if(mergeRows){
	
		rowVarUsed <- c(rowVar, if(statsLayout == "row" & length(statsVar) > 1)	"Statistic")
		
		# important: sort data.frame with specified row variables!
		data <- ddply(data, rowVar)
		if(".id" %in% colnames(data))	data$`.id` <- NULL
		
		# in case than rowVar are factor, can have issues to include additional rows (*invalid factor*), so convert them as character
		rowVarUsedFact <- names(which(sapply(data[, rowVarUsed, drop = FALSE], is.factor)))
		if(length(rowVarUsedFact) > 0)
			data[, rowVarUsedFact] <- colwise(.fun = as.character)(data[, rowVarUsedFact, drop = FALSE])
		
		# if more than one rowVar, convert them to different rows
		rowVarInRow <- setdiff(rowVarUsed, rowVarInSepCol) # row variables to merge
		rowVarFinal <- rowVarInRow[length(rowVarInRow)] # final column = more nested row variable
		rowVarToModify <- rowVarInRow[-length(rowVarInRow)] # variables to merge
		
		getTotalRow <- function(data)
			which(rowSums(data[, rowVar, drop = FALSE] == "Total") == length(rowVar))
		
		if(length(rowVarToModify) > 0){
			
			# save initial value of rowVarFinal for later
			data$rowVarFinal <- data[, rowVarFinal]
			
			data$rowPadding <- rowPadding <- length(rowVarInRow)-1
			idxTotalRow <- getTotalRow(data = data)
			data[idxTotalRow, "rowPadding"] <- 0
			
			# include the value in each rowVar in the column of the more nested variable
			# start by the second more nested variable
			for(i in rev(seq_along(rowVarToModify))){
				
				var <- rowVarToModify[i]
				varX <- data[, var]
				
				## add new rows
				
				# get indices of rows to replicates
				dataVarI <- data[, rowVarToModify[seq_len(i)]]
				# fix in case value in one column is NA
				# -> interaction is set to NA (without concatenating other columns)
				dataVarI[is.na(dataVarI)] <- ""
				idxRowToRepl <- which(!duplicated(interaction(dataVarI))) 
				idxRowToRepl <- setdiff(idxRowToRepl, idxTotalRow)
				
				# if the sub-total for this variable is computed and not included in separated row
				varNested <- rowVarInRow[match(var, rowVarInRow)+1]
				if(varNested %in% rowVarTotalInclude & !varNested %in% rowVarTotalInSepRow){
					
					# in case multiple records for total, add extra rows
					idxTotalNested <- which(data[, varNested] == "Total")
					idxTotalNested <- idxTotalNested[which(diff(idxTotalNested) == 1) + 1]
					idxRowToRepl <- unique(c(idxRowToRepl, idxTotalNested))
					
					# include the variable in the final column
					data[idxRowToRepl, "rowPadding"] <- rowPadding <- rowPadding - 1
					check <- ifelse(
						var == "variable" & varNested == "variableGroup",
						# if variable total is included only for certain variables
						any(!data[idxRowToRepl, rowVarFinal] %in% c("Total", "")),
						# otherwise, should be 'Total'
						any(data[idxRowToRepl, rowVarFinal] != "Total")
					)
					if(check)	stop("Missing total sub-category")
					data[idxRowToRepl, rowVarFinal] <- data[idxRowToRepl, var]
					
				}else{
					
					# new element:
					# convert to character in case is a factor
					val <- as.character(varX[idxRowToRepl])
					isNAVal <- which(is.na(val))
					if(length(isNAVal) > 0){
						val <- val[-isNAVal]
						idxRowToRepl <- idxRowToRepl[-isNAVal]
					}
					
					data <- data[sort(c(idxRowToRepl, seq_len(nrow(data)))), ]
					# fill columns
					idxRowToRepl <- idxRowToRepl + seq_along(idxRowToRepl)-1 # indices of replicates rows in new df
					# set var element in final row column
					data[idxRowToRepl, rowVarFinal] <- val #ifelse(is.na(val) | val == "", "Non specified", val)
					# and set to rest to NA
					data[idxRowToRepl, !colnames(data) %in% c(rowVarFinal, rowVarToModify)] <- NA
					# save the padding for flextable
					data[idxRowToRepl, "rowPadding"] <- rowPadding <- rowPadding - 1
					
				}
				
			}
			
			# if only one element in last nested rowVar (e.g. only one Statistic, or counts with categories)
			# merge with previous row (concatenate values)
			if(rowAutoMerge && statsLayout != "rowInSepCol" && (length(statsVar) > 1 | "variableGroup" %in% rowVar)){
				idxNARVF <- which(!is.na(data$rowVarFinal))
				idxMore2El <- which(diff(idxNARVF) == 1) # consecutive NAs
				if(length(idxMore2El) > 0)
					idxNARVF <- idxNARVF[-unique(c(idxMore2El, idxMore2El+1))]
				if(length(idxNARVF) > 0){
					# get value of only element (next row)
					dataNARVF <- data[idxNARVF, ] 
					# for final rowVar, concatenate the different values
					dataNARVF[, rowVarFinal] <- paste(
							data[idxNARVF-1, rowVarFinal], 
							data[idxNARVF, rowVarFinal]
					)
					# get the padding value of the last row
					dataNARVF[, "rowPadding"] <- data[idxNARVF-1, "rowPadding"]
					data[idxNARVF-1, ] <- dataNARVF
					data <- data[-idxNARVF, ]
				}
			}
			
			# merge rows, e.g. unique statistic for a subgroup are fill in the sub-group directly
			colContentTable <- setdiff(colnames(data), c(rowVar, rowVarFinal, "rowVarFinal", "rowPadding"))
			idxIsNA <- which(is.na(data[, rowVarFinal]))
			if(length(idxIsNA) > 0){
				
				# first case: values are the same than in previous row
				# e.g. only total included (e.g. rest is NA)
				idxIsNaSameValueAsPreviousRow <- rowSums(
					data[idxIsNA-1, colContentTable, drop = FALSE] == 
						data[idxIsNA, colContentTable, drop = FALSE]
				) == length(colContentTable)
				
				# second case: table content to fill (previous row) is all NA
				idxIsNaButNotContent <- rowSums(is.na(data[idxIsNA-1, colContentTable, drop = FALSE])) == length(colContentTable)
				
				idxIsNaToRemove <- which(!idxIsNaButNotContent & !idxIsNaSameValueAsPreviousRow)
				
				# don't consider the case that it is NA, e.g. if subgroup in input data is NA
				if(length(idxIsNaToRemove) > 0)
					idxIsNA  <- idxIsNA[-idxIsNaToRemove]
				if(length(idxIsNA) > 0){
					dataIsNa <- data[idxIsNA, ]
					dataIsNa[, c("rowPadding", rowVarFinal)] <- data[idxIsNA-1,  c("rowPadding", rowVarFinal)]
					data[idxIsNA-1, ] <- dataIsNa
					data <- data[-idxIsNA, ] 
				}
			}
			
			data[, c(rowVarToModify, "rowVarFinal")] <- rownames(data) <- NULL
			
			# order columns (in case rowVarInSepCol and row stats)
			rowColsFinal <- c(rowVarFinal, rowVarInSepCol)
			data <- data[, c(rowColsFinal, setdiff(colnames(data), rowColsFinal))]
			
			# adjust padding:
			# in case missing element in one of the nested variable
			# padding should be reduced
			data$rowPadding <- smoothPadding(pad = data$rowPadding)
			
			# save indices of rows to set padding in flextable
			padParams <- lapply(setdiff(unique(data$rowPadding), 0), function(pad)
				list(i = which(data$rowPadding == pad), j = 1, part = "body", padding.left = pad)				
			)
			
		}else	padParams <- list()
		
		## extract extra parameters for flextable (including header)
		
		## extract horizontal lines
		
		# rows with indent set to 0 (and no lines if all rows have no indent)
		idxHLine <- if(length(rowVarToModify) > 0 & !all(data$rowPadding == 0))	which(data$rowPadding == 0)-1
		idxHLine <- unique(idxHLine[idxHLine > 0])
		
		# remove lines between rows with same indent (e.g. multiple total rows)
		if(length(rowVarInSepCol) > 0)
			idxHLine <- idxHLine[idxHLine %in% cumsum(rle(data[, rowVarFinal])$lengths)]
		
		if(!is.null(rowVarTotalInclude)){
			idxRowTotal <- which(data[, rowVarFinal] == "Total" & data$rowPadding == 0)
			idxHLine <- c(idxHLine, idxRowTotal[length(idxRowTotal)]) # + include horizontal line after row total
			if(is.null(rowTotalLab)){
				rowTotalLabVars <- rowVarLab[rowVarInRow]
				rowTotalLabVars <- rowTotalLabVars[!is.na(rowTotalLabVars)]
				rowTotalLab <- paste("Any", toString(rowTotalLabVars))
			}
			data[idxRowTotal, rowVarFinal] <- rowTotalLab
		}
		
		# extract special formatting
		# if rowVar to format has been merged, extract corresponding rows based on padding
		rowVarFormat <- rowVarFormat[names(rowVarFormat) %in% rowVar]
		for(var in names(rowVarFormat)){
			# if variable has been merged, first column and extract row indices based on padding
			if(var %in% rowVarInRow){
				if(length(rowVarToModify) > 0){
					pad <- match(var, rowVarInRow)-1
					i <- which(data$rowPadding == pad)
				}else{
					i <- seq_len(nrow(data))
				}
				j <- 1
				# otherwise, extract column idx and consider all rows
			}else{
				j <- match(var, colnames(data))
				i <- seq_len(nrow(data))
			}
			formatParams <- c(formatParams, 
					list(list(i = i, j = j, part = "body", type = unname(rowVarFormat[[var]])))
			)
		}	
		
		data$rowPadding <- NULL
		if(length(idxHLine) > 0)
			hlineParams <- c(hlineParams, list(
				list(i = idxHLine, part = "body", j = 1:ncol(data))
			))
		for(var in rowVarInSepCol[-length(rowVarInSepCol)]){
			i <- which(diff(as.numeric(factor(data[, var], exclude = ""))) != 0)
			if(length(i) > 0)
				hlineParams <- c(hlineParams, list(
					list(
						i = i, 
						part = "body", 
						j = seq(from = which(colnames(data) == var), to = ncol(data))
					)
				))
		}
		
		# merge rows for rowVarInSepCol
		for(var in rowVarInSepCol){
			varPrev <- intersect(rowVar[seq_len(match(var, rowVar))], colnames(data))
			varPrevBin <- convertVectToBinary(interactionCustom(data[, varPrev, drop = FALSE])$x)
			idx <- which(diff(varPrevBin) == 0)
			if(length(idx) > 0){
				idxFact <- cut(seq_along(idx), breaks = c(-Inf, which(diff(idx) != 1), Inf))
				for(l in levels(idxFact)){
					i <- idx[idxFact == l]
					i <- c(i, max(i)+1)
					mergeParams <- c(mergeParams, list(list(
						i = i,
						j = match(var, colnames(data)),
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
		colnames(data)[match(rowVarFinal, colnames(data))] <- headerRow <-
			paste(rowVarLabs, collapse = "_")
		colnames(data)[match(rowVarInSepCol, colnames(data))] <- headerRowVarInSepCol <- rowVarLab[rowVarInSepCol]
		
	}
	
	# extract header (in case multiple 'colVar' specified)
	header <- sapply(colnames(data), function(x){
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
	colnames(headerDf) <- colnames(data)
	
	# attributes required when converting table to flextable:
	attributes(data)$summaryTable$header <- headerDf
	
	if(!is.null(hlineParams))
		attributes(data)$summaryTable$hlineParams <- hlineParams
	
	if(!is.null(mergeParams))
		attributes(data)$summaryTable$mergeParams <- mergeParams
	
	if(!is.null(formatParams))
		attributes(data)$summaryTable$formatParams <- formatParams
	
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
	attributes(data)$summaryTable$vlineParams <- vLineParams
	attributes(data)$summaryTable$vline <- vline
	
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
			
			attributes(data)$summaryTable$rowVar <- headerRow
			
		}
		
		if(length(padParams) > 0)
			attributes(data)$summaryTable$padParams <- padParams
		
	}
	
	
	return(data)
	
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