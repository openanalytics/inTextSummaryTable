#' Convert summary statistics table to flextable
#' @param summaryTable Dummary statistics table in long format,
#' as returned by \code{\link{formatSummaryStatisticsTable}}.
#' @param title Character vector with title(s) for the table.
#' Set to NULL if no title should be included.
#' @param footer Character vector with footer(s) for the table.
#' Set to NULL (by default) of no footer should be included.
#' @param rowPadBase Base padding for row (number of spaces)
#' @param fontname String with font name, 'Times' by default
#' @inheritParams getDimPage
#' @inheritParams formatSummaryStatisticsTable
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
#' @param type String dimension of interest, 'width' or 'height'.
#' @param landscape Logical, if TRUE (by defaut) the table is presented in landscape
#' format.
#' @param margin Margin in the document in inches.
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