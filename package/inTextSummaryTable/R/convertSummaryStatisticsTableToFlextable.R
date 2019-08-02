#' Convert summary statistics table to flextable
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' If vector > 1, specified for each element of \code{byVar} (in order of the levels).
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
#' @param colorTable Named character vector with color for the table background/body/text/line,
#' e.g. created with the \code{\link[glpgStyle]{getColorTable}} function.
#' @inheritParams glpgStyle::getDimPage
#' @inheritParams formatSuperSubscriptToFlextable
#' @inheritParams formatSummaryStatisticsTable
#' @return \code{\link[flextable]{flextable}} object with summary table
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of \code{\link[flextable]{flextable}}.
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom stats setNames
#' @importFrom glpgStyle getColorTable getGLPGFlextable createFlextableWithHeader getDimPage
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
			inputParamsBy$title <- if(length(title) > 1)
				inputParams$title[i]	else	c(
				inputParams$title, 
				strsplit(names(summaryTable)[i], split = "\n")[[1]]
			)
			do.call(convertSummaryStatisticsTableToFlextable, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)

		if(!is.null(file))
			exportFlextableToDocx(object = res, file = file, landscape = landscape)
	
		return(res)
		
	}
	
	sumTableAttr <- attributes(summaryTable)$summaryTable
	
	# create base flextable with header
	headerDf <- sumTableAttr$header
	ftWithHeader <- createFlextableWithHeader(
		data = summaryTable, 
		headerDf = headerDf,
		title = title,
		includeRownames = FALSE
	)
	ft <- ftWithHeader$ft
	colsDataFt <- ftWithHeader$colsData
	getNewCol <- function(initCol)
		na.omit(names(colsDataFt)[match(initCol, colsDataFt)])
	
	rowVar <- sumTableAttr$rowVar
	if(is.null(rowVar))	rowVar <- colnames(summaryTable)[1]
	
	# is there some padding specified?
	padParams <- sumTableAttr$padParams
	hasPadding <- length(padParams) > 0
	
	# special formatting (e.g. bold)
	for(el in sumTableAttr$formatParams){
		if("bold" %in% el$type)
			ft <- ft %>% bold(i = el$i, j = el$j, part = el$part)
	}
	
	# merge rows
	# important: merge rows before setting horizontal lines
	# otherwise might encounter issues
	rowVarToMerge <- c(rowVar, sumTableAttr$rowVarInSepCol)
	for(col in rowVarToMerge){
		j <- match(col, colnames(summaryTable))
		# vector with # duplicates
		countDupl <- rle(x = summaryTable[, j])$lengths
		countDuplIdx <- which(countDupl > 1) # only duplicated
		for(idx in countDuplIdx){
			# indices of duplicated rows
			i <- seq.int(
				from = ifelse(idx == 1, 1, cumsum(countDupl)[idx-1]+1), 
				length.out = countDupl[idx]
			)
			if(hasPadding){
				# extract padding spec for this column:
				idxPadCol <- sapply(padParams, function(x)
					x$part == "body" & 
					x$j == j
				)
				padParamsCol <- padParams[idxPadCol]
				if(length(padParamsCol) > 0){
					# extract padding for each row
					iPad <- sapply(i, function(iP){
						padIP <- unlist(lapply(padParamsCol, function(pad)
							if(iP %in% pad$i)	pad$padding.left	
						))
						ifelse(is.null(padIP), 0, max(padIP))
					})
					# remove row(s) which have a different padding
					iKept <- unique(unlist(lapply(which(diff(iPad) == 0), function(x) x+c(0, 1))))
					i <- i[iKept]
				}
			}
			# merge rows
			if(length(i) > 1){
				ft <- merge_at(ft, j = getNewCol(col), i = i)
			}
		}
	}
	
	if(!is.null(sumTableAttr$mergeParams)){
		for(params in sumTableAttr$mergeParams)
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

	# set correct alignments
	colsAlignLeft <- getNewCol(c("Statistic", rowVar))
	colsAlignCenter <- setdiff(names(colsDataFt), colsAlignLeft)
	ft <- align(ft, j = colsAlignLeft, align = "left", part = "all")
	ft <- align(ft, j = colsAlignCenter, align = "center", part = "all")
	
	## padding
	if(hasPadding)
		for(padParams in sumTableAttr$padParams){
			padPars <- grep("^padding", names(padParams), value = TRUE)
			padParams[padPars] <- sapply(padPars, function(par) padParams[[par]] * rowPadBase, simplify = FALSE)
			# if title is specified, shift row coordinate of padding by 1
			if(!is.null(title) && padParams$part == "header" && "i" %in% names(padParams))
				padParams$i <- padParams$i + 1
			ft <- do.call(padding, c(list(x = ft), padParams))
		}
	
	## borders
	bd <- fp_border(color = colorTable["line"])
	ft <- border_remove(ft)
	# if no vertical lines, only horizontal line 
	# between header/stub, top header and bottom stub
	vline <- sumTableAttr$vline
	if(!is.null(vline) && vline == "none"){
		ft <- ft %>% 
#			hline_top(border = bd, part = "header") %>%
			hline_bottom(border = bd, part = "body") %>%
			hline_bottom(border = bd, part = "header")
	}else ft <- border_outer(ft, border = bd, part = "all") # otherwise all border (stub + header)
	if(!is.null(title))
		ft <- ft %>% hline(i = length(title), border = bd, part = "header")
	
	# horizontal lines
	for(hlineParams in sumTableAttr$hlineParams)
		ft <- do.call(hline, c(list(x = ft, border = bd), hlineParams))
	
	# vertical lines
	for(vlineParams in sumTableAttr$vlineParams){
		if(!is.null(vlineParams$i))	vlineParams$i <- vlineParams$i + length(title)
		ft <- do.call(vline, c(list(x = ft, border = bd), vlineParams))
	}
	
	# important! in case cells are merged in a column, 
	# correct the position of horizontal lines
	ft <- ft %>% fix_border_issues()
	
	# Format superscript (if any)
	# for body
	ft <- formatSuperSubscriptToFlextable(
		dataTable = summaryTable, 
		ft = ft, 
		fontname = fontname,
		fontsize = fontsize
	)
	# for title
	if(!is.null(title))
		ft <- formatSuperSubscriptToFlextable(
			dataTable = data.frame(title), ft = ft, 
			part = "header",
			fontname = fontname,
			fontsize = fontsize,
			bold = TRUE
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
#	varFixed <- getNewCol(intersect(c("Statistic"), colsDataFt))
#	varFixedWidth <- 0.5
#	ft <- width(ft, j = varFixed, width = 0.5)
#	varsOther <- setdiff(names(colsDataFt), varFixed)
#	varsOtherWidth <- (widthPage - length(varFixed) * varFixedWidth)/length(varsOther)
	widthCol <- widthPage/length(colsDataFt)
	ft <- width(ft, j = names(colsDataFt), width = widthCol)
	
	if(!is.null(file))
		exportFlextableToDocx(object = ft, file = file, landscape = landscape)
	
	return(ft)
	
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
#' @param bold Logical (FALSE by default) should the superscript/subscript be indicated in bold?
#' @return \code{\link[flextable]{flextable}} with superscript/subscript.
#' @importFrom stats as.formula
#' @importFrom officer fp_text
#' @importFrom flextable display
#' @author Laure Cougnaud
#' @keywords internal
formatSuperSubscriptToFlextable <- function(
	dataTable, ft, 
	fontname = "Times",
	part = "body",
	fontsize = 8,
	iBase = 0,
	bold = FALSE){

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
								font.family = fontname,
								bold = bold
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

#' Export flextable to docx filr
#' @param object \code{\link[flextable]{flextable}} object, or list of such objects
#' @param file String with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @param landscape Logical, if TRUE the file is in landscape format.
#' @param breaksAfter In case \code{object} is list: 
#' integer vector with indices of list item after which a page break should 
#' be included in the final document.
#' @return no returned value, the \code{object} is exported to a docx file.
#' @import officer
#' @importFrom magrittr "%>%"
#' @author Laure Cougnaud
#' @export
exportFlextableToDocx <- function(
	object, file, landscape = FALSE,
	breaksAfter = if(!inherits(object, "flextable"))	seq_along(object)	else	1
	){
	
	isListTables <- !inherits(object, "flextable")
	
	if(!dir.exists(dirname(file)))	dir.create(dirname(file), recursive = TRUE)
	
	doc <- read_docx()
	if(landscape)	doc <- doc %>% body_end_section_landscape()
	
	if(isListTables){
		for(i in seq_along(object)){
			doc <- doc %>% body_add_flextable(value = object[[i]]) 
			if(i %in% breaksAfter)	
				doc <- doc %>% body_add_break()
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