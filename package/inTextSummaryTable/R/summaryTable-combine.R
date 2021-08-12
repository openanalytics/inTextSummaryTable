#' Combine objects
#' @param ... Extra parameters for the corresponding
#' method.
#' @seealso \code{\link{combine.summaryTable}}
#' to combine \code{\link{summaryTable}} objects.
#' @export
combine <- function(...) UseMethod("combine")

#' Combine summary statistics table
#' 
#' \itemize{
#' \item{A new table is created, combining the tables by rows.}
#' \item{The attributes of the combined summary table are 
#' obtained by combining the attributes of all summary tables
#' (and removing duplicates).}
#' }
#' 
#' \itemize{
#' \item{In case only a set of tables contain categorical variable,
#' in nested rows, so the \code{variable} and \code{variableGroup}
#' variables, these variables are included as last in 
#' the 'row variables' attribute of the combined table.}
#' \item{Only one row is retained for the columns totals
#' per column variable (the first one in order of appearance).\cr
#' The column totals are not included if the column
#' variable(s) are not the same across tables, or
#' if the totals differ between tables.}
#' }
#' @param ... \code{\link{summaryTable}} objects.
#' @param summaryTables List of \code{\link{summaryTable}} objects.
#' @param combineVar (optional) String with name
#' of a new variable tracking from which table
#' each record originally come from.\cr
#' The label for each table is extracted from the names
#' of the \code{summaryTables} list, or 1:length(tables)
#' if the list is not named.\cr
#' If not specified (by default), the tables will be combined
#' but the information on which input table:
#' each record from the combined table belongs to will
#' not be retained.
#' @param combineDir String indicating
#' on which direction: 'row' or 'col' (a.k.a column)
#' the information on the table appartenance 
#' (\code{combineVar}) will
#' be displayed when the table is exported.
#' @return A combined \code{\link{summaryTable}}.
#' @author Laure Cougnaud
#' @importFrom plyr rbind.fill
#' @method combine summaryTable
#' @example inst/examples/summaryTable-combine.R
#' @export
combine.summaryTable <- function(..., 
	summaryTables, 
	combineVar = NULL,
	combineDir = c("row", "col")){

	combineDir <- match.arg(combineDir)
	
	if(missing(summaryTables))
		summaryTables <- list(...)
	
	# get attributes
	attrs <- lapply(summaryTables, attr, 
		which = "summaryTable", exact = TRUE
	)
	
	# in case only one categorical variable is specified
	# the variable should be included as well
	
	# add label to identify the tables (if specified)
	if(!is.null(combineVar)){
		
		if(combineVar %in% unlist(lapply(summaryTables, colnames)))
			stop(shQuote(combineVar), 
				" is already available in the summary table, ",
				"please specify a different name for 'combineVar'.")
		
		if(is.null(names(summaryTables))){
			names(summaryTables) <- as.character(seq_along(summaryTables))
		}
		
		summaryTables <- sapply(names(summaryTables), function(name){
			summaryTable <- summaryTables[[name]]
			summaryTable[[combineVar]] <- factor(
				name, levels = names(summaryTables)
			)
			summaryTable
		}, simplify = FALSE)

	}
	
	# combine tables
	summaryTableCombine <- do.call(plyr::rbind.fill, summaryTables)
		
	# combine attributes
	attrNames <- lapply(attrs, names)
	hasEmptyAttrNames <-
		sapply(attrNames, is.null) | 
		sapply(attrNames, function(x) any(x == ""))
	if(any(hasEmptyAttrNames))
		stop("The summary table attribute should be named for all tables.")
	# add 'combine' variable as row or column variable
	if(!is.null(combineVar)){
		attrCombine <- switch(
			combineDir, 
			`row` = "rowVar", 
			`col` = "colVar"
		)
		attrs <- sapply(attrs, function(x){
			x[[attrCombine]] <- c(combineVar, x[[attrCombine]])	
			x
		}, simplify = FALSE)
	}
	attrsCombine <- tapply(
		X = unlist(attrs, use.names = FALSE, recursive = FALSE), 
		INDEX = unlist(lapply(attrs, names)),
		FUN = uniqueVarWithOrder
	)
	
	# combine the column total across tables
	# Note: attr should have been updated with combine variable
	# (in case combineVar is included in columns)
	summaryTableCombine <- combineColTotal(
		summaryTable = summaryTableCombine,
		attrs = attrs
	)
	
	# Check that tables have sufficient information to identify records uniquely
	rowColVars <- c(attrsCombine[["rowVar"]], attrsCombine[["colVar"]])
	idxTotal <- which(summaryTableCombine$isTotal)
	summaryTableNoTotal <- if(length(idxTotal) > 0){
		summaryTableCombine[-idxTotal, ]
	}else	summaryTableCombine
	rowColVarsCombine <- apply(summaryTableNoTotal[, rowColVars, drop = FALSE], 1, paste, collapse = ".")
	if(any(rowColVarsCombine == "NA") || any(duplicated(rowColVarsCombine)))
		stop("Summary tables cannot be combined.\n",
			"You might want to set combineVar or ",
			"create your table with varLabInclude to TRUE.")
		
	attr(summaryTableCombine, "summaryTable") <- attrsCombine
	class(summaryTableCombine) <- c("summaryTable", class(summaryTableCombine))
	
	return(summaryTableCombine)
	
}


#' Get unique variables with meaningful order.
#' 
#' The following framework is followed:
#' \enumerate{
#' \item{get the unique elements in the vectors}
#' \item{for each of this element: get the average order across
#' the different vectors}
#' \item{put \code{variable}, if present as second to last element}
#' \item{put \code{variableGroup}, if present, as last element}
#' \item{order the unique elements based on the extracted order}
#' }
#' @param ... Lists
#' @return Vector with unique and ordered elements.
#' @author Laure Cougnaud
uniqueVarWithOrder <- function(...){
	
	x <- c(...)
	if(!is.list(x))
		stop("Input should be lists.")
	
	# extract unique elements
	xUnique <- unique(unlist(x))
	
	# match per position
	xUniqueAll <- lapply(x, match, x = xUnique)
	# extract mean position
	xOrder <- do.call(mapply, c(xUniqueAll, list(FUN = function(...){mean(c(...), na.rm = TRUE)})))
	
	# set variable & variableGroup as last elements
	xOrder[which(xUnique == "variableGroup")] <- length(xUnique)
	xOrder[which(xUnique == "variable")] <- length(xUnique)-1
	
	# order vector
	xUniqueOrdered <- xUnique[order(xOrder, decreasing = FALSE)]
	
	return(xUniqueOrdered)
	
}

#' Combine the column total for a combined summary table
#' @param summaryTable Combined summary table
#' @param attrs Nested list with attributes of each
#' summary table.
#' @return summary table, with combined total columns
#' @importFrom plyr daply
#' @author Laure Cougnaud
combineColTotal <- function(summaryTable, attrs){
	
	# extract column variable(s) for each table
	attrsColVar <- lapply(attrs, function(x) 
		if(hasName(x, "colVar"))	x[["colVar"]])

	# are the column variable(s) identical for all tables?
	isIdenticalList <- function(x, y){
		if(identical(x, y)){
			x
		}else{ 
			warning("Column variable(s) are not identical across tables,",
				" so the column totals are discarded.")
		}
	}
	res <- tryCatch(
		Reduce(isIdenticalList, attrsColVar),
		warning = function(w) return(w)
	)
	# if column variable(s) are not identical, the column
	# totals are removed with a warning 
	if(inherits(res, "warning")){
		warning(res)
		summaryTable <- subset(summaryTable, !isTotal)
	}else{
		colVar <- unique(unlist(attrsColVar))
		nDiffSubjPerCol <- daply(
			subset(summaryTable, isTotal),
			colVar,
			function(x){
				length(unique(x[, "statN"]))
			}
		)
		isTotal <- which(summaryTable$isTotal)
		
		# if the total are the same for all columns
		# replicated rows with column total are removed
		if(all(nDiffSubjPerCol == 1)){
			isTotalDupl <- which(
				duplicated(summaryTable[isTotal, c(colVar, "statN")])
			)
			isTotalDupl <- isTotal[isTotalDupl]
			if(length(isTotalDupl) > 0){
				summaryTable <- summaryTable[-isTotalDupl, ]
			}
		# if the column total differ between tables
		# totals are removed with a warning 
		}else{
			warning("The column totals differ between the ",
				"tables, so these are discarded.")
			summaryTable <- subset(summaryTable, !isTotal)
		}
	}
	
	return(summaryTable)
	
}
