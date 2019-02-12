#' Convert summary statistics table to flextable
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' @param footer Character vector with footer(s) for the table.
#' Set to NULL (by default) of no footer should be included.
#' @param file String with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @param rowPadBase Base padding for row (in points), 14.4 by default (corresponds to 0.2 inches)
#' @param style string with table style in case \code{outputType} is 'flextable',
#'  either 'report' or 'presentation'.
#' This parameter affects the fontsize, font family, color of the text and background, 
#' and table dimensions of the table.
#' @param fontname String with font name, by default:
#' 'Times' if \code{style} is 'report' and 'Tahoma' if \code{style} is 'presentation'.
#' @param fontsize Integer with font size, by default:
#' 8 if \code{style} is 'report' and 10 if \code{style} is 'presentation'.
#' @param margin Margin in the document in inches, currently only used to specify the
#' width of the table: [width page extracted from \code{dimPage} - 2* margin]
#' @inheritParams getDimPage
#' @inheritParams formatSuperSubscriptToFlextable
#' @inheritParams formatSummaryStatisticsTable
#' @return \code{\link[flextable]{flextable}} object with summary table
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of \code{\link[flextable]{flextable}}.
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @author Laure Cougnaud
#' @export
convertSummaryStatisticsTableToFlextable <- function(
	summaryTable, 
	landscape = (style == "presentation"), 
	margin = 1, rowPadBase = 14.4,
	title = NULL, 
	footer = NULL,
	style = "report",
	colorTable = getColorTable(style = style),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	file = NULL, pageDim = NULL
){
	
	style <- match.arg(style, choices = c("report", "presentation"))
	
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(seq_along(summaryTable), function(i){
			summaryTableI <- summaryTable[[i]]
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			inputParamsBy$file <- NULL # export all tables at once
			inputParamsBy$title <- c(inputParams$title, names(summaryTable)[i])
			do.call(convertSummaryStatisticsTableToFlextable, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)

		if(!is.null(file))
			exportFlextableToDocx(object = res, file = file, landscape = landscape)
	
		return(res)
		
	}
	
	# create base flextable with header
	headerDf <- attributes(summaryTable)$summaryTable$header
	ftWithHeader <- createFlextableWithHeader(
		data = summaryTable, 
		headerDf = headerDf,
		title = title
	)
	ft <- ftWithHeader$ft
	colsDataFt <- ftWithHeader$colsData
	getNewCol <- function(initCol)
		na.omit(names(colsDataFt)[match(initCol, colsDataFt)])
	
	## borders
	bd <- fp_border(color = colorTable["line"])
	ft <- border_remove(ft)
	# if no vertical lines, only horizontal line 
	# between header/stub, top header and bottom stub
	vline <- attributes(summaryTable)$summaryTable$vline
	if(!is.null(vline) && vline == "none"){
		ft <- ft %>% 
			hline_top(border = bd, part = "header") %>%
			hline_bottom(border = bd, part = "header") %>%
			hline_bottom(border = bd, part = "body")
	}else ft <- border_outer(ft, border = bd, part = "all") # otherwise all border (stub + header)
	ft <- ft %>% border_inner_h(border = bd, part = "header")
	
	if(!is.null(title))
		ft <- hline(ft, i = length(title), border = bd, part = "header") 

	# set correct alignments
	rowVar <- attributes(summaryTable)$summaryTable$rowVar
	if(is.null(rowVar))	rowVar <- colnames(summaryTable)[1]
	colsAlignLeft <- getNewCol(c("Statistic", rowVar))
	colsAlignCenter <- setdiff(names(colsDataFt), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	## padding
	if(length(attributes(summaryTable)$summaryTable$padParams) > 0)
		for(padParams in attributes(summaryTable)$summaryTable$padParams){
			padPars <- grep("^padding", names(padParams), value = TRUE)
			padParams[padPars] <- sapply(padPars, function(par) padParams[[par]] * rowPadBase, simplify = FALSE)
			# if title is specified, shift row coordinate of padding by 1
			if(!is.null(title) && padParams$part == "header" && "i" %in% names(padParams))
				padParams$i <- padParams$i + 1
			ft <- do.call(padding, c(list(x = ft), padParams))
		}
	
	# horizontal lines
	if(!is.null(attributes(summaryTable)$summaryTable$hlineParams))
		for(hlineParams in attributes(summaryTable)$summaryTable$hlineParams){
			ft <- do.call(hline, c(list(x = ft, border = bd), hlineParams))
		}
	
	# vertical lines
	if(!is.null(attributes(summaryTable)$summaryTable$vlineParams))
		for(vlineParams in attributes(summaryTable)$summaryTable$vlineParams){
			if(!is.null(vlineParams$i))	vlineParams$i <- vlineParams$i + length(title)
			ft <- do.call(vline, c(list(x = ft, border = bd), vlineParams))
		}
	
	# merge rows
	rowVarToMerge <- c(rowVar, attributes(summaryTable)$summaryTable$summaryTable$rowVarInSepCol)
	if(!is.null(rowVarToMerge))
		ft <- merge_v(ft, j = getNewCol(rowVarToMerge))
	
	if(!is.null(attributes(summaryTable)$summaryTable$mergeParams)){
		for(params in attributes(summaryTable)$summaryTable$mergeParams)
			ft <- merge_at(ft, j = params$j, params$i, part = params$part)
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
	ft <- formatSuperSubscriptToFlextable(
		dataTable = summaryTable, 
		ft = ft, 
		fontname = fontname,
		fontsize = fontsize
	)
	# for header
	if(!is.null(headerDf))
		ft <- formatSuperSubscriptToFlextable(
			dataTable = headerDf, ft = ft, 
			part = "header",
			fontname = fontname,
			fontsize = fontsize,
			iBase = length(title)
		)
	
	# set style
	ft <- getGLPGFlextable(
		data = summaryTable, ft = ft, 
		border = FALSE, adjustWidth = FALSE, align = FALSE,
		style = style,
		fontname = fontname,
		fontsize = fontsize,
		landscape = landscape,
		margin = margin,
		colorTable = colorTable,
		pageDim = pageDim
	)
	
	# adjust to fit in document:
	widthPage <- getDimPage(
		type = "width", landscape = landscape, margin = margin,
		pageDim = pageDim,
		style = style
	)
	varFixed <- getNewCol(intersect(c("Statistic"), colsDataFt))
	varFixedWidth <- 0.5
	ft <- width(ft, j = varFixed, width = 0.5)
	varsOther <- setdiff(names(colsDataFt), varFixed)
	varsOtherWidth <- (widthPage - length(varFixed) * varFixedWidth)/length(varsOther)
	ft <- width(ft, j = varsOther, width = varsOtherWidth)
	
	if(!is.null(file))
		exportFlextableToDocx(object = ft, file = file, landscape = landscape)
	
	return(ft)
	
}

#' Return page dimension of interest
#' @param type String dimension of interest, 'width' or 'height'.
#' @param landscape Logical, if TRUE (by defaut) the table is presented in landscape
#' format.
#' @param margin Margin in the document in inches.
#' @param style string with table style, either 'report' or 'presentation'
#' @param pageDim Numeric vector of length 2 with page width and height in inches
#' in portrait format.
#' @return integer with dimension of interest
#' @author Laure Cougnaud
#' @export
getDimPage <- function(
	type = c("width", "height"), 
	landscape = (style == "presentation"), 
	margin = 1,
	pageDim = NULL,
	style = "report"){

	# landscape: 29.7 * 21 cm ~ 11 * 8 inches ~ 2138.4 * 1512 ptx
	type <- match.arg(type)
	
	style <- match.arg(style, choices = c("report", "presentation"))
	
	pageDimPortrait <- 	if(is.null(pageDim))
		switch(style,
			'report' = c(21, 29.7)/2.54,
			'presentation' = c(7.5, 10.83)
		)	else	pageDim
	
	typeDim <- switch(type,
		'width' = ifelse(landscape, pageDimPortrait[2], pageDimPortrait[1]),
		'height' = ifelse(landscape, pageDimPortrait[1], pageDimPortrait[2])
	)
	dimPage <- typeDim - 2 * margin
	
	return(dimPage)
}

#' Format superscript/subscripts in a flextable.
#' Superscript should be indicated as 'a^{b}' and subscript as 'a_{b}' the input summary table.
#' @param dataTable data.frame with data used in table,
#' summary table for body or header data.frame for the header.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param fontname String with font name, 'Times' by default.
#' @param fontsize Integer with font size, 8 by default.
#' @param part string with part of the table to consider, 
#' see \code{\link[flextable]{display}}.
#' @param iBase Integer with base row index (if different than 0).
#' @return \code{\link[flextable]{flextable}} with superscript/subscript.
#' @importFrom stats as.formula
#' @importFrom officer fp_text
#' @importFrom flextable display
#' @author Laure Cougnaud
formatSuperSubscriptToFlextable <- function(
	dataTable, ft, 
	fontname = "Times",
	part = "body",
	fontsize = 8,
	iBase = 0){

	patterns <- c("superscript" = "(.+)\\^\\{(.+)\\}(.*)", "subscript" = "(.+)_\\{(.+)\\}(.*)")
	
	for(patternName in names(patterns)){
		
		pattern <- patterns[patternName]

		# extract indices with superscript
		dataTableMat <- as.matrix(dataTable)
		idxSuperscriptMat <- grep(pattern, dataTableMat)
		
		# if any
		if(length(idxSuperscriptMat) > 0){
			
			# convert matrix indices to [row, col]
			idxSuperscriptAI <- arrayInd(idxSuperscriptMat, .dim = dim(dataTableMat))
			
			# for each element with superscript
			for(iSP in seq_along(idxSuperscriptMat)){
				
				textInit <- dataTableMat[idxSuperscriptMat[iSP]]
				# split text with before/after superscript
				idxMatches <- regexec(pattern = pattern, textInit)
				textSplit <- regmatches(textInit, idxMatches)
				
				# for each superscript (in case multiple for the same text)
				for(el in textSplit){
					
					# create formatters: should be list of formula
					fm <- c(
						list(
							as.formula(paste0("value ~ as.character('", el[2], "')")),
							as.formula(paste0("pow ~ as.character('", el[3], "')"))
						),
						if(el[4] != "")
							list(as.formula(paste0("value2 ~ as.character('", el[4], "')")))
					)
					
					# set superscript/subscript in flextable
					ft <- ft %>% display(
						i = idxSuperscriptAI[iSP, 1] + iBase,
						col_key = idxSuperscriptAI[iSP, 2],
						pattern = paste0("{{value}}{{pow}}", if(el[4] != "") "{{value2}}"),
						formatters = fm,
						fprops = list(pow = 
							fp_text(
								vertical.align = patternName, 
								font.size = fontsize,
								font.family = fontname
							)
						),
						part = part
					)
				}
			}
		}
	}
	
	return(ft)

}

#' Create a flextable, setting the column names to syntactic names
#' if it is not the case.
#' @param data Data.frame with data.
#' @param headerDf (optional) Data.frame with header.
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' @return list with:
#' \itemize{
#' \item{'ft': }{\code{\link[flextable]{flextable}}}
#' \item{'colsData': }{Named vector with original column names,
#' with names set to new syntactic names.}
#' }
#' @author Laure Cougnaud
#' @import flextable
createFlextableWithHeader <- function(data, 
	headerDf = NULL, title = NULL){
	
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
	if(!is.null(headerDf) && nrow(headerDf) > 1)
		ft <- setHeader(ft, header = headerDf[-nrow(headerDf), ])
	
	# set to correct headers	
	newHeaders <- if(!is.null(headerDf))	headerDf[nrow(headerDf), ]	else	colsDataFt
	ft <- do.call(set_header_labels, c(list(x = ft), as.list(newHeaders)))
	
	ft <- merge_v(ft, part = "header")
	
	# add title
	if(!is.null(title))	
		for(titleI in rev(title))
			ft <- setHeader(ft, header = titleI)
	
	res <- list(ft = ft, colsData = colsDataFt)
	return(res)
	
}

#' Format a flextable to fulfill GLPG style.
#' @param data data.frame with data used in table.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param border Logical, if TRUE add a border.
#' @param adjustWidth Logical, if TRUE adjust column widths.
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' Only available if \code{ft} is not specified.
#' @param align Logical, if TRUE (by default), default alignment is set.
#' @param colorTable Named character vector with color for the table,
#' see output of \code{\link{getColorTable}} for required elements.
#' @inheritParams getDimPage
#' @inheritParams formatSuperSubscriptToFlextable
#' @return \code{\link[flextable]{flextable}} with GLPG style.
#' @author Laure Cougnaud
#' @import flextable
#' @importFrom officer fp_border
#' @export
getGLPGFlextable <- function(data, 
	ft = NULL, 
	border = TRUE,
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	landscape = (style == "presentation"),
	style = "report",
	margin = 1,
	adjustWidth = TRUE,
	colorTable = getColorTable(style = style),
	align = TRUE,
	title = NULL,
	pageDim = NULL){
	
	if(is.null(ft))
		ft <- createFlextableWithHeader(data = data, title = title)$ft
	
	bd <- fp_border(color = colorTable["line"])
	
	# set fontsize
	ft <- fontsize(ft, size = fontsize, part = "all")
	
	# set header in bold
	ft <- bold(ft, part = "header")
	
	# set font
	ft <- ft %>% font(fontname = fontname, part = "all")
	
	# set border
	if(border){
		ft <- border_remove(ft) %>%
			border_outer(border = bd, part = "all")%>% 
			vline(border = bd, part = "body") %>%
			vline(border = bd, part = "header")
		if(style == "presentation")
			ft <- ft %>% hline(border = bd, part = "body")
		if(!is.null(title))
			ft <- ft %>% hline(j = length(title), border = bd, part = "header") 
	}
	
	# change color text + background
	ft <- ft %>% 
		# header
		color(color = colorTable["header"], part = "header") %>%
		bg(bg = colorTable["headerBackground"], part = "header") %>%
		# footer
		color(color = colorTable["footer"], part = "footer") %>%
		bg(bg = colorTable["footerBackground"], part = "footer") %>%
		# text color
		color(color = colorTable["body"], part = "body")
	
	if(all(c("bodyBackground1", "bodyBackground2") %in% names(colorTable))){
		# alternate background between elements of first column
		xBg <- convertVectToBinary(x = data[, 1])
		ft <- ft %>%
			bg(bg = colorTable["bodyBackground1"], i = which(xBg %% 2 == 0), part = "body") %>%
			bg(bg = colorTable["bodyBackground2"], i = which(xBg %% 2 == 1), part = "body")
	}else	ft <- ft %>% bg(bg = colorTable["bodyBackground"], part = "body")
	
	if(adjustWidth){
		widthPage <- getDimPage(
			type = "width", landscape = landscape, margin = margin,
			style = style,
			pageDim = NULL
		)
		width <- widthPage/ncol(data)
		ft <- width(ft, j = seq_len(ncol(data)), width = width)
	}
	
	if(align)
		ft <- align(ft, j = seq_len(ncol(data)), align = "center", part = "all")
	
	# by default, height of each header/footer (excepted the first one) line is quite big
	ft <- height(ft, height = dim_pretty(ft, part = "header")$heights, part = "header")
	ft <- height(ft, height = dim_pretty(ft, part = "footer")$heights, part = "footer")
	
	return(ft)
	
}

#' Get table color
#' @inheritParams getDimPage
#' @return Named character vector with color for the different parts of the table,
#' should at least contain font/background color for:
#' \itemize{
#' \item{header: }{'header'/'headerBackground'}
#' \item{body: }{'body' for body text and either 'bodyBackground' or 
#' 'bodyBackground1'/'bodyBackground2': for body common background or background 
#' for alternate rows}
#' \item{footer: }{'footer'/'footerBackground'}
#' \item{line: }{'line'}
#' }
#' @author Laure Cougnaud
#' @importFrom glpgStyle glpgColor
#' @export
getColorTable <- function(style = c("report", "presentation")){
	
	colorTable <- switch(style,
		'report' = {
			# black font on white background
			c(
				'header' = rgbCustom(0, 0, 0),
				'headerBackground' = rgbCustom(255, 255, 255),
				'body' = rgbCustom(0, 0, 0), 
				'bodyBackground' = rgbCustom(255, 255, 255),
				'footer' = rgbCustom(0, 0, 0),
				'footerBackground' = rgbCustom(255, 255, 255),
				'line' = rgbCustom(255, 255, 255)
			)
			
		},
		'presentation' = glpgColor(type = "table")
	)
	
}

#' Convert vector to a bincode of 0/1
#' based on consecutive values in the vector.
#' @param x Vector.
#' @return Integer vector of same length than \code{x}.
#' @author Laure Cougnaud
convertVectToBinary <- function(x){

	xBin <- rep(NA, length(x))
	idxChg <- c(1, which(diff(as.numeric(factor(x, exclude = FALSE))) != 0) + 1)
	xBin[idxChg] <- rep(c(0, 1), length.out = length(idxChg))
	for(i in seq_along(xBin)){
		if(is.na(xBin[i]))	xBin[i] <- xBin[i-1]
	}
	return(xBin)
	
}

#' Export flextable to docx filr
#' @param object \code{\link[flextable]{flextable}} object, or list of such objects
#' @param file String with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @param landscape Logical, if TRUE the file is in landscape format.
#' @return no returned value, the \code{object} is exported to a docx file.
#' @import officer
#' @importFrom magrittr "%>%"
#' @author Laure Cougnaud
#' @export
exportFlextableToDocx <- function(object, file, landscape = FALSE){
	
	isListTables <- !inherits(object, "flextable")
	
	if(!dir.exists(dirname(file)))	dir.create(dirname(file), recursive = TRUE)
	
	doc <- read_docx()
	if(landscape)	doc <- doc %>% body_end_section_landscape()
	
	if(isListTables){
		for(summaryTableFtI in object){
			doc <- doc %>% body_add_flextable(value = summaryTableFtI) %>% body_add_break()
		}
	}else	doc <- doc %>% body_add_flextable(value = object)
	
	if(landscape){
		doc <- doc %>%
			# a paragraph needs to be included after the table otherwise the layout is not landscape
			body_add_par(value = "", style = "Normal") %>%
			body_end_section_landscape()
	}
	print(doc, target = file)
	
}