#' Convert summary statistics table to flextable
#' @inheritParams inTextSummaryTable-common-args
#' @inheritParams inTextSummaryTable-flextable-args
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
	colorTable = getColorPaletteTable(style = style),
	fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
	fontsize = switch(style, 'report' = 8, 'presentation' = 10),
	file = NULL, pageDim = NULL
) {
	
	style <- match.arg(style, choices = c("report", "presentation"))
	
	if(is.null(summaryTable)) {
		return(invisible())
	}
	
	if(!is.data.frame(summaryTable)) {
		
		inputParams <- as.list(environment())
		res <- sapply(seq_along(summaryTable), function(i) {
			summaryTableI <- summaryTable[[i]]
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			inputParamsBy$title <- if(length(title) > 1)
				inputParams$title[i]	else	c(
				inputParams$title, 
				strsplit(names(summaryTable)[i], split = "\n")[[1]]
			)
			inputParamsBy$file <- if(!is.null(file)) {
				paste0(file_path_sans_ext(file), "_", i, ".", file_ext(file))
			}
			do.call(convertSummaryStatisticsTableToFlextable, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)
	
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
	for(el in sumTableAttr$formatParams) {
		if("bold" %in% el$type)
			ft <- ft %>% bold(i = el$i, j = el$j, part = el$part)
	}
	
	# merge rows
	# important: merge rows before setting horizontal lines
	# otherwise might encounter issues
	rowVarToMerge <- c(rowVar, sumTableAttr$rowVarInSepCol)
	for(col in rowVarToMerge) {
		j <- match(col, colnames(summaryTable))
		# vector with # duplicates
		countDupl <- rle(x = summaryTable[, j])$lengths
		countDuplIdx <- which(countDupl > 1) # only duplicated
		for(idx in countDuplIdx) {
			# indices of duplicated rows
			i <- seq.int(
				from = ifelse(idx == 1, 1, cumsum(countDupl)[idx-1]+1), 
				length.out = countDupl[idx]
			)
			if(hasPadding) {
				# extract padding spec for this column:
				idxPadCol <- sapply(padParams, function(x)
					x$part == "body" & 
					x$j == j
				)
				padParamsCol <- padParams[idxPadCol]
				if(length(padParamsCol) > 0) {
					# extract padding for each row
					iPad <- sapply(i, function(iP) {
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
			if(length(i) > 1) {
				ft <- merge_at(ft, j = getNewCol(col), i = i)
			}
		}
	}
	
	if(!is.null(sumTableAttr$mergeParams)) {
		for(params in sumTableAttr$mergeParams)
			ft <- merge_at(ft, j = params$j, params$i, part = params$part)
	}
	
	# add footer
	if(!is.null(footer)) {
		for(iFoot in seq_along(footer)) {
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
		for(padParams in sumTableAttr$padParams) {
			padPars <- grep("^padding", names(padParams), value = TRUE)
			padParams[padPars] <- sapply(padPars, function(par) padParams[[par]] * rowPadBase, simplify = FALSE)
			# if title is specified, shift row coordinate of padding by 1
			if(!is.null(title) && padParams$part == "header" && "i" %in% names(padParams))
				padParams$i <- padParams$i + length(title)
			ft <- do.call(padding, c(list(x = ft), padParams))
		}
	
	## borders
	bd <- fp_border(color = colorTable["line"])
	ft <- border_remove(ft)
	# if no vertical lines, only horizontal line 
	# between header/stub, top header and bottom stub
	isVline <- sumTableAttr$vline
	if(!is.null(isVline) && isVline == "none") {
		ft <- ft %>% 
#			hline_top(border = bd, part = "header") %>%
			hline_bottom(border = bd, part = "body") %>%
			hline_bottom(border = bd, part = "header")
	}else ft <- border_outer(ft, border = bd, part = "all") # otherwise all border (stub + header)
	if(!is.null(title))
		ft <- ft %>% hline(i = length(title), border = bd, part = "header")
	
	# horizontal lines
	isHline <- sumTableAttr$hline
	if(!(!is.null(isHline) && isHline == "none")) {
		for(hlineParams in sumTableAttr$hlineParams)
			ft <- do.call(hline, c(list(x = ft, border = bd), hlineParams))
	}
	
	# vertical lines
	for(vlineParams in sumTableAttr$vlineParams) {
		if(!is.null(vlineParams$i))	vlineParams$i <- vlineParams$i + length(title)
		ft <- do.call(vline, c(list(x = ft, border = bd), vlineParams))
	}
	
	# important! in case cells are merged in a column, 
	# correct the position of horizontal lines
	ft <- ft %>% fix_border_issues()
	
	# Custom formatting (if any)
	# for body
	ft <- formatCustomFlextable(
		dataTable = summaryTable, 
		ft = ft, 
		fontname = fontname,
		fontsize = fontsize,
		part = "body"
	)
	# for footnote
	if(!is.null(footer))
		ft <- formatCustomFlextable(
			dataTable = data.frame(footer), 
			ft = ft, 
			fontname = fontname,
			fontsize = fontsize,
			part = "footer"
		)
	
	# for title
	if(!is.null(title))
		ft <- formatCustomFlextable(
			dataTable = data.frame(title), ft = ft, 
			part = "header",
			fontname = fontname,
			fontsize = fontsize,
			bold = TRUE
		)
	# for header
	if(!is.null(headerDf))
		ft <- formatCustomFlextable(
			dataTable = headerDf, ft = ft, 
			part = "header",
			fontname = fontname,
			fontsize = fontsize,
			iBase = length(title)
		)
	
	# set style
	ft <- getListing(
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

#' Format superscript/subscripts/bold cells in a flextable.
#' Superscript should be indicated as 'a^{b}',
#' subscript as 'a_{b}' and bold as bold{a} in the
#'  the input summary table.
#' @param dataTable data.frame with data used in table,
#' summary table for body or header data.frame for the header.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param fontname String with font name, 'Times' by default.
#' @param fontsize Integer with font size, 8 by default.
#' @param part string with part of the table to consider, 
#' see \code{\link[flextable]{compose}}.
#' @param iBase Integer with base row index (if different than 0).
#' @param bold Logical (FALSE by default) should the superscript/subscript 
#' be indicated in bold?
#' @return \code{\link[flextable]{flextable}} with superscript/subscript.
#' @importFrom stats as.formula
#' @importFrom officer fp_text
#' @importFrom flextable compose as_b as_sup as_sub as_paragraph
#' @importFrom utils packageVersion
#' @author Laure Cougnaud
#' @keywords internal
formatCustomFlextable <- function(
	dataTable, ft, 
	fontname = "Times",
	part = "body",
	fontsize = 8,
	iBase = 0,
	bold = FALSE) {

	patterns <- c(
		"bold" = "(.*)bold\\{(.+)\\}(.*)",
		"superscript" = "(.+)\\^\\{(.+)\\}(.*)", 
		"subscript" = "(.+)_\\{(.+)\\}(.*)"
	)
	
	for(patternName in names(patterns)) {
		
		pattern <- patterns[patternName]

		# extract indices with superscript
		dataTableMat <- as.matrix(dataTable)
		idxPatternMat <- grep(pattern, dataTableMat)
		
		# if any
		if(length(idxPatternMat) > 0) {
			
			# convert matrix indices to [row, col]
			idxPatternAI <- arrayInd(idxPatternMat, .dim = dim(dataTableMat))
			
			# for each element with superscript
			for(idx in seq_along(idxPatternMat)) {
				
				textInit <- dataTableMat[idxPatternMat[idx]]
				# split text with before/after superscript
				idxMatches <- regexec(pattern = pattern, textInit)
				textSplit <- regmatches(textInit, idxMatches)
				
				# for each superscript (in case multiple for the same text)
				for(el in textSplit) {
			
					iEl <- idxPatternAI[idx, 1] + iBase
					jEl <- idxPatternAI[idx, 2]
						
					fctFm <- switch(patternName,
						bold = flextable::as_b,
						superscript = flextable::as_sup, 
						subscript = flextable::as_sub
					)
					listValues <- c(
						if(el[2] != "")	list(as_chunk(el[2])),
						list(fctFm(el[3])),
						if(el[4] != "")	list(as_chunk(el[4]))
					)
					para <- do.call(flextable::as_paragraph, listValues)
					ft <- ft %>% flextable::compose(
						i = iEl, j = jEl,
						value = para,
						part = part
					)
					
				}
			}
		}
	}
	
	return(ft)

}

#' Export flextable to docx file
#' @param object \code{\link[flextable]{flextable}} object, or list of such objects
#' @param breaksAfter In case \code{object} is list: 
#' integer vector with indices of list item after which a page break should 
#' be included in the final document.
#' @inheritParams inTextSummaryTable-flextable-args
#' @return no returned value, the \code{object} is exported to a docx file.
#' @import officer
#' @importFrom magrittr "%>%"
#' @author Laure Cougnaud
#' @export
exportFlextableToDocx <- function(
	object, file, landscape = FALSE,
	breaksAfter = if(!inherits(object, "flextable"))	seq_along(object)	else	1
	) {
	
	isListTables <- !inherits(object, "flextable")
	
	if(!dir.exists(dirname(file)))	dir.create(dirname(file), recursive = TRUE)
	
	doc <- read_docx()
	if(landscape)	doc <- doc %>% body_end_section_landscape()
	
	if(isListTables) {
		for(i in seq_along(object)) {
			doc <- doc %>% body_add_flextable(value = object[[i]]) 
			if(i %in% breaksAfter)	
				doc <- doc %>% body_add_break()
		}
	}else	doc <- doc %>% body_add_flextable(value = object)
	
	if(landscape) {
		doc <- doc %>%
			# a paragraph needs to be included after the table otherwise the layout is not landscape
			body_add_par(value = "", style = "Normal") %>%
			body_end_section_landscape()
	}
	print(doc, target = file)
	
}