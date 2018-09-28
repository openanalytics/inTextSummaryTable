#' Export a summary table in \code{docx} format.
#' @param file string with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @inheritParams formatSummaryStatisticsForExport
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @import officer
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(summaryTable, 
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL,
	colVar = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = FALSE, 
	margin = 1, rowPadBase = 2,
	title = "Table: Descriptive statistics",
	footer = NULL){

	## format table
	summaryTableLong <- formatSummaryStatisticsForExport(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		colVar = colVar
	)

	# create flextable only with header to extract dimensions header
	summaryTableFt <- convertSummaryStatisticsTableToFlextable(
		summaryTable = summaryTableLong,
		landscape = landscape, margin = margin, rowPadBase = rowPadBase,
		title = title, footer = footer
	)	
	
	# include the tables in a Word document
	if(!is.null(file)){	
		
		doc <- read_docx()
		if(landscape)	doc <- doc %>% body_end_section_landscape()
		
		doc <- doc %>% body_add_flextable(value = summaryTableFt)
		
		if(landscape){
			doc <- doc %>%
				# a paragraph needs to be included after the table otherwise the layout is not landscape
				body_add_par(value = "", style = "Normal") %>%
				body_end_section_landscape()
		}
		print(doc, target = file)
		
	}
	
	return(summaryTableFt)
	
}

#' Format summary statistics table for export
#' @param summaryTable summary table, created with the \code{\link{computeSummaryStatistics}} function.
#' @param colVar character vector with variable(s) used for the columns.
#' If multiple variables are specified, the variables should be sorted in hierarchical order,
#' and are included in multi-columns layout.
#' @param rowVar character vector with variable(s) used for the rows.
#' If multiple variables are specified, the variables should be sorted in hierarchical order.
#' The variables are included in rows, excepted if specified in \code{rowVarInSepCol}. 
#' @param rowVarLab label for the \code{rowVar} variable(s)
#' @param rowVarInSepCol variable(s) of \code{rowVar} which should be 
#' included in separated column in the table.
#' NULL by default. 
#' @inheritParams subjectProfileSummaryPlot
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
formatSummaryStatisticsForExport <- function(
	summaryTable,
	rowVar = NULL, 
	rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL,
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
		if(is.factor(summaryTable[, colVarWithCount])){
			colVarWithCountEl <- unique(dataWithTotal[, colVarWithCount])		
			colVarWithCountElOrdered <- colVarWithCountEl[order(match(sub("(.+)\n\\(N=.+\\)", "\\1", colVarWithCountEl), levels(summaryTable[, colVarWithCount])))]		
			dataWithTotal[, colVarWithCount] <- factor(dataWithTotal[, colVarWithCount], levels = colVarWithCountElOrdered)
		}
		
	}else{
		idxTotal <- which(summaryTable$isTotal)
		nTotal <- summaryTable[idxTotal, "N"]
		dataWithTotal <- summaryTable[-idxTotal, ]
	}
		
	# convert from wide to long format
	statsVar <- if(is.null(attributes(summaryTable)$statsVar))
		setdiff(colnames(dataWithTotal),  c(rowVar, colVar))	else	attributes(summaryTable)$statsVar
	dataLong <- melt(dataWithTotal, 
		id.vars = c(rowVar, colVar),
		measure.vars = statsVar,
		value.name = "StatisticValue",
		variable.name = "Statistic"
	)
	
	# format statistic value
	dataLong$StatisticValue <- formatC(dataLong$StatisticValue)
	
	# put elements in 'colVar' in different columns (long -> wide format)
	if(!is.null(colVar)){
		rowVarForm <- c(
			if(!is.null(rowVar)) paste(rowVar, collapse = " + "), 
			if(length(statsVar) > 1)	"Statistic"
		)
		formulaWithin <- as.formula(paste(
			paste(rowVarForm, collapse = "+"),
			"~", 
			paste(colVar, collapse = " + ")
		))
		dataLong <- dcast(dataLong, formula = formulaWithin, value.var = "StatisticValue")
	}else{
		colnames(dataLong)[match("StatisticValue", colnames(dataLong))] <- 
			paste0("StatisticValue\n(N=",  nTotal, ")")
	}
	
	if(!is.null(rowVar)){
		
		# sort data.frame with specified row variables
		dataLong <- ddply(dataLong, rowVar)
		
		# in case than rowVar are factor, can have issues to include additional rows (*invalid factor*), so convert them as character
		rowVarFact <- names(which(sapply(dataLong[, rowVar], is.factor)))
		if(length(rowVarFact) > 0)
			dataLong[, rowVarFact] <- colwise(.fun = as.character)(dataLong[, rowVarFact, drop = FALSE])
	
		# if more than one rowVar, convert them to different rows
		rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
		rowVarFinal <- rowVarInRow[length(rowVarInRow)]
		rowVarToModify <- rowVarInRow[-length(rowVarInRow)]
		if(length(rowVarToModify) > 0){
			
			dataLong$rowPadding <- rowPadding <- length(rowVarInRow)-1
			for(i in rev(seq_along(rowVarToModify))){
				
				var <- rowVarToModify[i]
				varX <- dataLong[, var]
				# add new rows
				idxRowToRepl <- which(!duplicated(interaction(dataLong[, rowVarToModify[seq_len(i)]]))) # indices of rows to replicates
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
			which(!duplicated(dataLong[, rowVarFinal]))[-1]-1
		}else	if(length(rowVarToModify) > 0){
				which(diff(dataLong$rowPadding) != 0)
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

#' Convert summary statistics table to flextable
#' @param summaryTable summary statistics table in long format,
#' as returned by \code{\link{formatSummaryStatisticsForExport}}
#' @param title character vector with title(s) for the table.
#' Set to NULL if no title should be included.
#' @param footer character vector with footer(s) for the table.
#' Set to NULL (by default) of no footer should be included.
#' @param rowPadBase base padding for row (number of spaces)
#' @param fontname string with font name, 'Times' by default
#' @inheritParams getDimPage
#' @inheritParams formatSummaryStatisticsForExport
#' @return \code{\link[flextable]{flextable}} object with summary table
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @author Laure Cougnaud
convertSummaryStatisticsTableToFlextable <- function(
	summaryTable, 
	landscape = FALSE, 
	margin = 1, rowPadBase = 2,
	title = "Table: Descriptive statistics",
	footer = NULL,
	fontname = "Times"
	){
	
	# column border
	bd <- fp_border()
	
	# re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
	colsDataFt <- colnames(summaryTable)
	names(colsDataFt) <- paste0("col", seq_len(ncol(summaryTable)))
	colnames(summaryTable) <- names(colsDataFt)
	
	headerDf <- attributes(summaryTable)$header	
	if(!is.null(headerDf))	colnames(headerDf) <- names(colsDataFt)
	
	getNewCol <- function(initCol)
		na.omit(names(colsDataFt)[match(initCol, colsDataFt)])
	
	# base flextable
	ft <- flextable(summaryTable)
	
	## headers:
	setHeader <- function(ft, header){
		headerList <- as.list(
			if(is.matrix(header) | is.data.frame(header))	header	else
				setNames(rep(header, length(colsDataFt)), names(colsDataFt))
		)
		ft <- do.call(add_header, c(list(x = ft, top = TRUE), headerList))
		ft <- merge_h(x = ft, part = "header")
		return(ft)
	}
	if(!is.null(headerDf) & nrow(headerDf) > 1)	ft <- setHeader(ft, header = headerDf[-nrow(headerDf), ])
	
	# set to correct headers	
	newHeaders <- if(!is.null(headerDf))	headerDf[nrow(headerDf), ]	else	colsDataFt
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(newHeaders)))
	
	# set correct alignments
	rowVar <- attributes(summaryTable)$rowVar
	colsAlignLeft <- getNewCol(c("Statistic", rowVar))
	colsAlignCenter <- setdiff(names(colsDataFt), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	## padding
	if(length(attributes(summaryTable)$padParams) > 0)
		for(padParams in attributes(summaryTable)$padParams){
			padPars <- grep("^padding", names(padParams), value = TRUE)
			padParams[padPars] <- lapply(padPars, function(par) padParams[[par]] * rowPadBase)
			ft <- do.call(padding, c(list(x = ft), padParams))
		}
	
	# merge rows
	if(!is.null(rowVar))	ft <- merge_v(ft, j = getNewCol(rowVar)) 
	
	# add title and headers
	if(!is.null(title))	
		for(titleI in title)
			ft <- setHeader(ft, header = titleI)
	
	# borders
	ft <- border_remove(ft) %>%
		border_outer(border = bd, part = "all") %>%
		hline(border = bd, part = "header") %>%
		vline(border = bd, part = "body")

	# horizontal lines
	if(!is.null(attributes(summaryTable)$hlineParams))
		for(hlineParams in attributes(summaryTable)$hlineParams)
			ft <- do.call(hline, c(list(x = ft, border = bd), hlineParams))
	
	# vertical lines
	if(!is.null(attributes(summaryTable)$vlineParams))
		for(vlineParams in attributes(summaryTable)$vlineParams){
			vlineParams$i <- vlineParams$i + length(title)
			ft <- do.call(vline, c(list(x = ft, border = bd), vlineParams))
		}
	
	# add footer
	if(!is.null(footer))	
		for(footerI in footer){
			paramsFooter <- setNames(list(footer), names(colsDataFt[1]))
			ft <- do.call(add_footer, 
				c(list(x = ft), paramsFooter)
			) %>% merge_at(j = 1:ncol(summaryTable), part = "footer")
		}
	
	# set fontsize
	ft <- fontsize(ft, size = 8, part = "all")
	
	# set header in bold
	ft <- bold(ft, part = "header")
	
	# set font
	ft <- ft %>% font(fontname = fontname, part = "all")
	
	# adjust to fit in document:
	widthPage <- getDimPage(type = "width", landscape = landscape, margin = margin)
	varFixed <- getNewCol(intersect(c("Statistic", "Total"), colsDataFt))
	varFixedWidth <- 0.5
	ft <- width(ft, j = varFixed, width = 0.5)
	varsOther <- setdiff(names(colsDataFt), varFixed)
	varsOtherWidth <- (widthPage - length(varFixed) * varFixedWidth)/length(varsOther)
	ft <- width(ft, j = varsOther, width = varsOtherWidth)
			
	return(ft)
	
}

#' Return page dimension of interest
#' @param type string dimension of interest, 'width' or 'height'
#' @param landscape logical, if TRUE (by defaut) the table is presented in landscape
#' format
#' @param margin margin in the document in inches
#' @return integer with dimension of interest
#' @author Laure Cougnaud
getDimPage <- function(type = c("width", "height"), landscape = TRUE, margin = 1){
	# landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
	type <- match.arg(type)
	a4Dim <- c(21, 29.7)/2.54 # inches
	typeDim <- switch(type,
		'width' = ifelse(landscape, a4Dim[2], a4Dim[1]),
		'height' = ifelse(landscape, a4Dim[1], a4Dim[2])
	)
	dimPage <- typeDim - 2 * margin
	return(dimPage)
}