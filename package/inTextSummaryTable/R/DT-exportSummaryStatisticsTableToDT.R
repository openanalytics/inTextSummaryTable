#' Export summary table to an interactive DT table,
#' e.g. to be exported into an html document.
#' @param summaryTable Summary table as provided by the 
#' \code{\link{formatSummaryStatisticsTable}}.
#' @param ... (DT output) Extra parameters passed to the 
#' \code{\link[clinUtils]{getClinDT}}
#' @inheritParams inTextSummaryTable-common-args
#' @inheritParams inTextSummaryTable-DT-args
#' @inherit clinUtils::getClinDT return
#' @author Laure Cougnaud
#' @importFrom utils head
#' @importFrom clinUtils getLabelVar getClinDT
#' @importFrom tools file_path_sans_ext file_ext
#' @export
exportSummaryStatisticsTableToDT <- function(
	summaryTable, 
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = NULL, 
	statsVar = getAttribute(summaryTable, "statsVar"),
	statsLayout = getAttribute(summaryTable, "statsLayout", default = "col"),
	statsValueLab = "StatisticValue",
	title = NULL,
	expandVar = NULL, noEscapeVar = NULL, barVar = NULL,
	pageDim = NULL,
	labelVars = NULL,
	file = NULL,
	...){

	statsLayout <- match.arg(statsLayout, choices = c("row", "col", "rowInSepCol"))
	
	if(!is.data.frame(summaryTable)){
		
		inputParams <- as.list(environment())
		res <- sapply(seq_along(summaryTable), function(i){
			summaryTableI <- summaryTable[[i]]
			inputParamsBy <- inputParams
			inputParamsBy$summaryTable <- summaryTableI
			inputParamsBy$title <- if(length(title) > 1){
				inputParams$title[i]	
			}else{c(
					inputParams$title, 
					strsplit(names(summaryTable)[i], split = "\n")[[1]]
				)
			}
			inputParamsBy$file <- if(!is.null(file)){
				paste0(file_path_sans_ext(file), "_", i, ".", file_ext(file))
			}
			do.call(exportSummaryStatisticsTableToDT, inputParamsBy)		
		}, simplify = FALSE)	
		if(!is.null(names(summaryTable)))
			names(res) <- names(summaryTable)
		
		return(res)
		
	}
	
	# important: sort data.frame with specified row variables!
	if(!is.null(rowVar))
		summaryTable <- ddply(summaryTable, rowVar)
	
	# nesting in row variables specified via: 'rowGroup' parameter for DT
	rowVarInRow <- setdiff(rowVar, rowVarInSepCol)
	if(statsLayout == "row" & "Statistic" %in% colnames(summaryTable))
		rowVarInRow <- c(rowVarInRow, "Statistic")
	# consider all row variables, excepted the last one as row group
	rowGroupVar <- head(rowVarInRow, -1)
	if(length(rowGroupVar) == 0)	rowGroupVar <- NULL
	if(length(rowGroupVar) > 1){
		rowGroupVar <- head(rowGroupVar, 1)
		warning(paste0("Currently multi-level row grouping row variable is not available in the 'DT' package",
			", so the rows are grouped by ", rowGroupVar, " only."))
	}
	
	## extract column(s) containing statistics (for expand or escape)

	# if no colVar is specified, results are stored in the column: statsValueLab
	colStat <- colnames(summaryTable)[match(statsValueLab, 
		sub("(.+)(\n\\(N=\\d{1,}\\))", "\\1", colnames(summaryTable)))
	]
	# otherwise we should extract the groups of colVar
	if(is.na(colStat)){
		colStat <- setdiff(colnames(summaryTable), c(rowVar, statsVar, "Statistic"))
	}
		
	## format expandVar
	expandIdx <- expandVarDT <- NULL
	if(!is.null(expandVar)){
		
		# extract expandVar which are in the column names
		expandVarDT <- intersect(colnames(summaryTable), expandVar)
		
		# in case expandVar is one statistic,
		# extract indices if statistics are in rows
		expandVarStats <- intersect(statsVar, expandVar)
		if(length(expandVarStats) > 0){
			specifyExpand <- 
				statsLayout %in% c("row", "rowInSepCol") & 
				"Statistic" %in% colnames(summaryTable)
		
			if(specifyExpand){
			
				idxRow <- which(summaryTable$Statistic %in% expandVarStats)
				idxColStat <- which(colnames(summaryTable) %in% colStat)
				if(length(idxColStat) == 0)	
					stop("Issue with extraction statistic value column during the formatting 'expandVar'.")
				expandIdx <- as.matrix(expand.grid(idxRow, idxColStat))
				colnames(expandIdx) <- c("row", "col")
			}
		}
		
	}
	
	## format escape
	escape <- if(!is.null(noEscapeVar)){
		
		if(is.character(noEscapeVar)){
			
			if(statsLayout %in% c("row", "rowInSepCol") && noEscapeVar %in% statsVar){
				noEscapeVar <- colStat
			}
		
			escape <- which(!colnames(summaryTable) %in% noEscapeVar)
			if(length(escape) == 0)	{
				FALSE
			}else	escape
			
		}else	noEscapeVar
		
	}else	TRUE

	# convert 'numeric-like' column to numeric in order to have nice filters
	isColNum <- sapply(seq_len(ncol(summaryTable)), function(iCol){
		x <- summaryTable[, iCol]
		if(is.character(x)){
			tryCatchW <- tryCatch(expr = as.numeric(x), warning = function(w) w)
			!inherits(x = tryCatchW, what = "warning")
		}else	FALSE
	})
	idxColNum <- which(isColNum)
	summaryTable[, idxColNum] <- sapply(summaryTable[, idxColNum, drop = FALSE],
		as.numeric, simplify = FALSE)

	# set row variable labels
	colnamesDT <- colnames(summaryTable)
	rowVarLabs <- c(
		rowVarLab[setdiff(rowVar, "Statistic")], 
		if(statsLayout != "col" && "Statistic" %in% colnames(summaryTable))	rowVarLab["Statistic"]
	)
	rowVarLabs <- rowVarLabs[!is.na(rowVarLabs)]
	colnamesDT <- if(length(rowVarLabs) > 0)
		setNames(names(rowVarLabs), rowVarLabs)
	
	# page length
	pageLength <- if(!is.null(pageDim)){
		if(length(pageDim) != 2)
			stop("'pageDim' should be of length 2.") 
		pageDim[2]
	}
	
	# bar
	barVar <- intersect(barVar, colnames(summaryTable))
	if(length(barVar) == 0) barVar <- NULL
	
	if(!is.null(title) && is.character(title) && length(title) > 1)
		title <- paste(title, collapse = " ")
	
	argsDT <- c(
		list(
			data = summaryTable,
			rowGroupVar = rowGroupVar,
			caption = title,
			escape = escape,
			barVar = barVar,
			file = file
		),
		if(!is.null(pageLength))	list(pageLength = pageLength),
		if(length(expandIdx) > 0)	list(expandIdx = expandIdx),
		if(length(expandVarDT) > 0)	list(expandVar = expandVarDT),
		if(length(colnamesDT) > 0)	list(colnames = colnamesDT)
	)
	
	# check if extra parameters for the 'getClinDT' function 
	# specified by the user are not already specified in this function
	argsDTExtra <- list(...)
	isArgsDupl <- names(argsDTExtra) %in% names(argsDT)
	if(any(isArgsDupl)){
		warning(paste("Parameter(s):", toString(sQuote(names(argsDTExtra)[isArgsDupl])),
			"are already specified internally, you may consider using in-text table specific",
			"parameters instead.")
		)
	}
	argsDT <- c(argsDT, argsDTExtra[!isArgsDupl])
	
	# create DT
	res <- do.call(getClinDT, argsDT)
	
	return(res)
	
}