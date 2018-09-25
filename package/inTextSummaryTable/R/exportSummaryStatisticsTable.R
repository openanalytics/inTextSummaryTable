#' Export a summary table in \code{docx} format.
#' @param file string with path of the file where the table should be exported
#' @inheritParams formatSummaryStatisticsForExport
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @import officer
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(data, 
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	colVar = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = FALSE, 
	title = "Table: Descriptive statistics"){

	margin <- 1

	## format table
	summaryTableLong <- formatSummaryStatisticsForExport(
		data = data,
		rowVar = rowVar, rowVarLab = rowVarLab,
		colVar = colVar
	)
	
	convertSummaryStatisticsTableToFlextableCustom <- function(...){
		convertSummaryStatisticsTableToFlextable(...,
			landscape = landscape, margin = margin,
			title = title,
			rowVar = rowVarLab
		)	
	}
	
	# create flextable only with header to extract dimensions header
	summaryTableFt <- convertSummaryStatisticsTableToFlextableCustom(data = summaryTableLong)	
	
	##  split table between different sections
	# Note: there is an automated implementation in the officer package
	# to split table across pages
	
	# extract maximum width page and width of header and each row (in inches)
#	heightPage <- getDimPage(type = "height", landscape = landscape, margin = margin)
#	headerHeight <- sum(summaryTableFt$header$rowheights)
#	bodyHeights <- summaryTableFt$body$rowheights
	
	# extract rows where the table should be split
#	statsVar <- c("N", "Mean", "SD", "SE", "Median", "Min", "Max")
#	idxEndSection <- which(summaryTableLong$Statistic == statsVar[length(statsVar)]) # cut by section
#	bodyHeightsCumsum <- cumsum(bodyHeights)
#	heightPageForTable <- heightPage - headerHeight
#	breaks <- c(seq(from = 0, to = max(bodyHeightsCumsum), by = heightPageForTable), Inf)
#	idxSectionByPage <- findInterval(bodyHeightsCumsum[idxEndSection], breaks)
	
#	# build the list of tables
#	summaryTableFtList <- lapply(unique(idxSectionByPage), function(i){
#		iRowStart <- ifelse(i == 1, 1, max(idxEndSection[which(idxSectionByPage == (i - 1))]) + 1)
#		iRowEnd <-  max(idxEndSection[which(idxSectionByPage == i)])
#		table <- summaryTableLong[seq.int(from = iRowStart, to = iRowEnd), ]
#		convertSummaryStatisticsTableToFlextableCustom(data = table)
#	})

	# include the tables in a Word document
	if(!is.null(file)){	
		
		doc <- read_docx()
		if(landscape)	doc <- doc %>% body_end_section_landscape()
		
#		for(i in seq_along(summaryTableFtList)){
			doc <- doc %>% body_add_flextable(value = summaryTableFt)
#			if(i != length(summaryTableFtList))
#				doc <- doc %>% body_add_break()
#		}
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
#' @inheritParams subjectProfileSummaryPlot
#' @param colVar string with variable of \code{data} used for grouping in column.
#' @param rowVar character vector with variable(s) of \code{data}
#' used for grouping in rows.
#' @param rowVarLab label for each variable of \code{rowVar}.
#' @inheritParams subjectProfileSummaryPlot
#' @return data reformatted in long format
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @importFrom reshape2 melt dcast
#' @importFrom stats as.formula
formatSummaryStatisticsForExport <- function(data,
	rowVar = NULL, 
	rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	colVar = NULL,
	labelVars = NULL
	){
	
	# add total in column header
	dataWithTotal <- ddply(data, colVar, function(x){
		idxTotal <- which(x[, rowVar] == "Total")
		if(length(idxTotal) == 1){
			x[, colVar[length(colVar)]] <- paste0(x[, colVar[length(colVar)]], " (N=",  x[idxTotal , "N"], ")")
			x[-idxTotal, ]
		}else x
	})
		
	# convert from wide to long format
	statsVar <- c("N", "Mean", "SD", "SE", "Median", "Min", "Max")
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
		formulaWithin <- as.formula(paste(
			paste(rowVar, collapse = " + "), 
			"+ Statistic ~", 
			paste(colVar, collapse = " + ")
		))
		dataLong <- dcast(dataLong, formula = formulaWithin, value.var = "StatisticValue")
	}
	
	# label header for rows
	colnames(dataLong)[match(rowVar, colnames(dataLong))] <- rowVarLab
	
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
	
	return(dataLong)
	
}

#' Convert summary statistics table to flextable
#' @param data summary statistics table in long format,
#' as returned by \code{\link{formatSummaryStatisticsForExport}}
#' @param title string with title for the table.
#' Set to NULL if no title should be included.
#' @inheritParams getDimPage
#' @inheritParams formatSummaryStatisticsForExport
#' @return \code{\link[flextable]{flextable}} object
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @author Laure Cougnaud
convertSummaryStatisticsTableToFlextable <- function(
	data, 
	landscape = FALSE, margin = 1,
	title = "Table: Descriptive statistics",
	rowVar = NULL, 
	fontname = "Times"
	){
	
	# re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
	colsDataFt <- colnames(data)
	names(colsDataFt) <- paste0("col", seq_len(ncol(data)))
	colnames(data) <- names(colsDataFt)
	
	headerDf <- attributes(data)$header	
	if(!is.null(headerDf))	colnames(headerDf) <- names(colsDataFt)
	
	getNewCol <- function(initCol)
		names(colsDataFt)[match(initCol, colsDataFt)]
	
	ft <- flextable(data)
	
	if(!is.null(rowVar))
		ft <- merge_v(ft, j = getNewCol(rowVar)) # merge rows
	
	# set correct alignments
	colsAlignLeft <- getNewCol(c("Statistic", rowVar))
	colsAlignCenter <- setdiff(names(colsDataFt), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	# add title and headers
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
	if(!is.null(title))	
		for(titleI in title)
			ft <- setHeader(ft, header = titleI)
	
	# set to correct headers	
	newHeaders <- if(!is.null(headerDf))	headerDf[nrow(headerDf), ]	else	colsDataFt
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(newHeaders)))
	
	# set fontsize
	ft <- fontsize(ft, size = 8, part = "all")
	
	# set header in bold
	ft <- bold(ft, part = "header")
	
	# set font
	ft <- ft %>%
		font(fontname = fontname, part = "body") %>%
		font(fontname = fontname, part = "header")
	
	# adjust to fit in document:
	widthPage <- getDimPage(type = "width", landscape = landscape, margin = margin)
	varFixed <- getNewCol(intersect(c("Statistic", "Total"), colsDataFt))
	varFixedWidth <- 0.5
	ft <- width(ft, j = varFixed, width = 0.5)
	varsOther <- setdiff(names(colsDataFt), varFixed)
	varsOtherWidth <- (widthPage - length(varFixed) * varFixedWidth)/length(varsOther)
	ft <- width(ft, j = varsOther, width = varsOtherWidth)
	
	# borders
	ft <- border_remove(ft) %>%
		hline_top(border = fp_border(), part = "body") %>% 
		hline_bottom(border = fp_border(), part = "body") %>%
		hline_top(border = fp_border(), part = "header") %>% 
		hline(border = fp_border(), part = "header") 
	
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