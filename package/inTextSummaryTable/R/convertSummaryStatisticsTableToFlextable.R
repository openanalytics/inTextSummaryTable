#' Convert summary statistics table to flextable
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' @param footer Character vector with footer(s) for the table.
#' Set to NULL (by default) of no footer should be included.
#' @param rowPadBase Base padding for row (number of spaces)
#' @inheritParams getDimPage
#' @inheritParams formatSuperscriptToFlextable
#' @inheritParams formatSummaryStatisticsTable
#' @return \code{\link[flextable]{flextable}} object with summary table
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of \code{\link[flextable]{flextable}}.
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @author Laure Cougnaud
convertSummaryStatisticsTableToFlextable <- function(
	summaryTable, 
	landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 4,
	title = NULL,
	footer = NULL,
	style = "report",
	fontname = "Times", #switch(style, 'report' = "Times", 'presentation = "Tahoma')
	fontsize = 8
){
	
	style <- match.arg(style, choices = c("report", "presentation"))
	
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(summaryTable, function(summaryTableI){
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			do.call(convertSummaryStatisticsTableToFlextable, inputParamsBy)		
		}, simplify = FALSE)	
		return(res)
		
	}
	
	# create base flextable with header
	headerDf <- attributes(summaryTable)$header
	ftWithHeader <- createFlextableWithHeader(
		data = summaryTable, 
		headerDf = headerDf
	)
	ft <- ftWithHeader$ft
	colsDataFt <- ftWithHeader$colsData
	getNewCol <- function(initCol)
		na.omit(names(colsDataFt)[match(initCol, colsDataFt)])
	
	# column border
	bd <- switch(style,
		'report' = fp_border(),
		'presentation' = fp_border(color = "white")
	)
	ft <- border_remove(ft) %>%
		border_outer(border = bd, part = "all") %>%
		hline(border = bd, part = "body") %>%
		vline(border = bd, part = "body")
	
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
	if(!is.null(footer)){
		for(iFoot in seq_along(footer)){
			paramsFooter <- setNames(list(footer[iFoot]), names(colsDataFt[1]))
			ft <- do.call(add_footer, 
				c(list(x = ft, top = FALSE), paramsFooter)
			)
			ft <- ft %>% merge_at(
				i = iFoot,
				j = seq_len(ncol(summaryTable)), part = "footer"
			)
		}
	}
	
	# Format superscript (if any)
	# for body
	ft <- formatSuperscriptToFlextable(
		dataTable = summaryTable, 
		ft = ft, 
		fontname = fontname,
		fontsize = fontsize
	)
	# for header
	ft <- formatSuperscriptToFlextable(
		dataTable = headerDf, ft = ft, 
		part = "header",
		fontname = fontname,
		fontsize = fontsize
	)
	
	# set style
	ft <- getGLPGFlextable(
		data = summaryTable, ft = ft, 
		border = FALSE, adjustWidth = FALSE, align = FALSE,
		style = style,
		fontname = fontname,
		fontsize = fontsize,
		landscape = landscape,
		margin = margin
	)
	
	# adjust to fit in document:
	widthPage <- getDimPage(
		type = "width", landscape = landscape, margin = margin,
		style = style
	)
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
#' @param style string with table style, either 'report' or 'presentation'
#' @return integer with dimension of interest
#' @author Laure Cougnaud
getDimPage <- function(
	type = c("width", "height"), 
	landscape = (style == "presentation"), 
	margin = 1,
	style = "report"){

	# landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
	type <- match.arg(type)
	
	style <- match.arg(style, choices = c("report", "presentation"))
	
	pageDimPortrait <- switch(style,
		'report' = c(21, 29.7)/2.54,
		'presentation' = c(7.5, 10.83)
	)
	
	typeDim <- switch(type,
		'width' = ifelse(landscape, pageDimPortrait[2], pageDimPortrait[1]),
		'height' = ifelse(landscape, pageDimPortrait[1], pageDimPortrait[2])
	)
	dimPage <- typeDim - 2 * margin
	
	return(dimPage)
}

#' Format superscript in a flextable.
#' Superscript should be indicated as 'a^{b}' in the input summary table.
#' @param dataTable data.frame with data used in table,
#' summary table for body or header data.frame for the header.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param fontname String with font name, 'Times' by default.
#' @param fontsize Integer with font size, 8 by default.
#' @return \code{\link[flextable]{flextable}} with superscript.
#' @importFrom stats as.formula
#' @importFrom officer fp_text
#' @importFrom flextable display
#' @author Laure Cougnaud
formatSuperscriptToFlextable <- function(
	dataTable, ft, 
	fontname = "Times",
	part = "body",
	fontsize = 8){

	# extract indices with superscript
	dataTableMat <- as.matrix(dataTable)
	idxSuperscriptMat <- grep("\\^\\{.+\\}", dataTableMat)
	
	# if any
	if(length(idxSuperscriptMat) > 0){
		
		# convert matrix indices to [row, col]
		idxSuperscriptAI <- arrayInd(idxSuperscriptMat, .dim = dim(dataTableMat))
		
		# for each element with superscript
		for(iSP in seq_along(idxSuperscriptMat)){
			
			textInit <- dataTableMat[idxSuperscriptMat[iSP]]
			# split text with before/after superscript
			idxMatches <- regexec(pattern = "(.+)\\^\\{(.+)\\}(.*)", textInit)
			textSplit <- regmatches(textInit, idxMatches)
			
			# for each superscript (in case multiple for the same text)
			for(el in textSplit){
				
				# create formatters: should be list of formula
				fm <- list(
					as.formula(paste0("value ~ as.character('", el[2], "')")),
					as.formula(paste0("pow ~ as.character('", el[3], "')"))
				)
				
				# set superscript in flextable
				ft <- ft %>% display(
					i = idxSuperscriptAI[iSP, 1],
					col_key = idxSuperscriptAI[iSP, 2],
					pattern = "{{value}}{{pow}}",
					formatters = fm,
					fprops = list(pow = 
						fp_text(
							vertical.align = "superscript", 
							font.size = fontsize,
							font.family = fontname
						)
					),
					part = part
				)
			}
		}
	}
	
	return(ft)

}

#' Create a flextable, setting the column names to syntactic names
#' if it is not the case.
#' @param data Data.frame with data.
#' @param headerDf (optional) Data.frame with header.
#' @return list with:
#' \itemize{
#' \item{'ft': }{\code{\link[flextable]{flextable}}}
#' \item{'colsData': }{Named vector with original column names,
#' with names set to new syntactic names.}
#' }
#' @author Laure Cougnaud
#' @import flextable
createFlextableWithHeader <- function(data, headerDf = NULL){
	
	# re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
	colsDataFt <- colnames(data)
	names(colsDataFt) <- paste0("col", seq_len(ncol(data)))
	colnames(data) <- names(colsDataFt)
	
	if(!is.null(headerDf))	colnames(headerDf) <- names(colsDataFt)
	
	# base flextable
	ft <- flextable(data)
	
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
	if(!is.null(headerDf) && nrow(headerDf) > 1)	ft <- setHeader(ft, header = headerDf[-nrow(headerDf), ])
	
	# set to correct headers	
	newHeaders <- if(!is.null(headerDf))	headerDf[nrow(headerDf), ]	else	colsDataFt
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(newHeaders)))
	
	res <- list(ft = ft, colsData = colsDataFt)
	return(res)
	
}

#' Format a flextable to fulfill GLPG style.
#' @param data data.frame with data used in table.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param border Logical, if TRUE add a border.
#' @param adjustWidth Logical, if TRUE adjust column widths.
#' @inheritParams getDimPage
#' @inheritParams formatSuperscriptToFlextable
#' @return \code{\link[flextable]{flextable}} with GLPG style.
#' @author Laure Cougnaud
#' @import flextable
#' @importFrom glpgStyle glpgColor
#' @importFrom officer fp_border
#' @export
getGLPGFlextable <- function(data, 
	ft = NULL, 
	border = TRUE,
	fontname = "Times", #switch(style, 'report' = "Times", 'presentation = "Tahoma')
	fontsize = 8,
	landscape = (style == "presentation"),
	style = "report",
	margin = 1,
	adjustWidth = TRUE,
	align = TRUE){
	
	if(is.null(ft))
		ft <- createFlextableWithHeader(data = data)$ft
	
	bd <- switch(style,
		'report' = fp_border(),
		'presentation' = fp_border(color = "white")
	)
	
	# set fontsize
	ft <- fontsize(ft, size = fontsize, part = "all")
	
	# set header in bold
	ft <- bold(ft, part = "header")
	
	# set font
	ft <- ft %>% font(fontname = fontname, part = "all")
	
	# set border
	if(border){
		ft <- border_remove(ft) %>%
			border_outer(border = bd, part = "all") %>%
			hline(border = bd, part = "body") %>%
			vline(border = bd, part = "body") %>%
			hline(border = bd, part = "header") %>%
			vline(border = bd, part = "header")
	}
	
	# change color text + background
	if(style == "presentation"){
		colorTable <- glpgColor(type = "table")
		idxRows <- seq_len(nrow(data))
		ft <- ft %>% 
			# header in white on green background
			bg(bg = colorTable["header"], part = "header") %>%
			color(color = "white", part = "header") %>%
			# footer with white background
			bg(bg = "white", part = "footer") %>%
			# body with alternated dark and light grey background
			bg(bg = colorTable["row1"], i = idxRows[idxRows %% 2 == 1], part = "body") %>%
			bg(bg = colorTable["row2"], i = idxRows[idxRows %% 2 == 0], part = "body")
	}
	
	if(adjustWidth){
		widthPage <- getDimPage(
			type = "width", landscape = landscape, margin = margin,
			style = style
		)
		width <- widthPage/ncol(data)
		ft <- width(ft, j = seq_len(ncol(data)), width = width)
	}
	
	if(align)
		ft <- align(ft, j = seq_len(ncol(data)), align = "center", part = "all")

	return(ft)
	
}