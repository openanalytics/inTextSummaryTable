#' Format or create flextable for listings.
#' 
#' Flextable version >= 0.4.7 and pandoc >= 2.4
#' is required to included such table in a Rmarkdown document.
#' @param data data.frame with data used in table.
#' @param ft Corresponding \code{\link[flextable]{flextable}}.
#' @param border Logical, if TRUE add a border.
#' @param highlight Integer vector with index(ices) of column(s) to highlight
#' (only applies for \code{style}: 'presentation').
#' 0 for rownames (if present).
#' Colors for:
#' \itemize{
#' \item{highlighted columns is specified in \code{colorTable["headerBackgroundHighlight"]}}
#' \item{non highlighted columns is specified in \code{colorTable["headerBackground"]}}
#' }
#' @param bgVar String with the column of the \code{data} used for
#' alternating the body background colors of the table. 
#' @param fontname String with font name, 'Times' by default.
#' @param fontsize Integer with font size, 8 by default.
#' @param adjustWidth Logical, if TRUE adjust column widths,
#' to comply to specification of \code{landscape},
#' \code{margin} and \code{pageDim}
#' (only set to \code{FALSE} if e.g. table dimensions 
#' are pre-set with the specified \code{ft}).
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' Only available if \code{ft} is not specified.
#' @param align Logical, if TRUE (by default), 
#' default alignment is set ('center' in all table).
#' @param colorTable Named character vector with color for the table,
#' see output of \code{\link{getColorPaletteTable}} for required elements.
#' @examples
#' # style: report or presentation
#' getListing(data = head(mtcars), style = "report")
#' getListing(data = head(mtcars), style = "presentation")
#' # remove rownames (included by default)
#' getListing(data = head(mtcars), style = "presentation", includeRownames = FALSE)
#' # highlight:
#' #  all columns
#' getListing(data = head(mtcars), style = "presentation", highlight = seq_along(mtcars))
#' # rownames
#' getListing(data = head(mtcars), style = "presentation", highlight = 0)
#' # specific columns
#' getListing(data = head(mtcars), style = "presentation", highlight = c(2, 4))
#' @inheritParams getDimPage
#' @inheritParams createFlextableWithHeader
#' @return \code{\link[flextable]{flextable}} with style.
#' @import flextable
#' @importFrom officer fp_border
#' @importFrom magrittr "%>%"
#' @export
getListing <- function(
    data, ft, 
    border = TRUE,
    highlight = integer(),
    bgVar = NULL,
    fontname = switch(style, 'report' = "Times", 'presentation' = "Tahoma"),
    fontsize = switch(style, 'report' = 8, 'presentation' = 10),
    landscape = (style == "presentation"),
    style = "report",
    margin = 1,
    adjustWidth = TRUE,
    colorTable = getColorPaletteTable(style = style),
    align = TRUE,
    title = NULL,
    pageDim = NULL,
    includeRownames = TRUE
) {
  
  style <- match.arg(style, choices = c("report", "presentation"))
  
  if(missing(data) & missing(ft))
    stop("Input 'data' or flextable object (as 'ft') should be specified.")
  
  isColorElSpec <- function(el)
    !is.null(colorTable) && !is.na(colorTable[el])
  
  shiftCol <- if(missing(ft)) {
        ft <- createFlextableWithHeader(data = data, title = title, includeRownames = includeRownames)$ft
        hasRN <- includeRownames && !is.null(rownames(data))
        if(!hasRN & 0 %in% highlight){
          warning("Rownames are not highlighted because no rownames or 'includeRownames' is set to FALSE.")
          highlight <- setdiff(highlight, 0)
        }
        as.integer(hasRN)
      } else	0
  
  # by default, bottom and top padding are set to 2
  ft <- ft %>% padding(padding.top = 0, padding.bottom = 0)
  
  if(isColorElSpec("line")) {
    bd <- fp_border(color = colorTable["line"])
  } else	bd <- fp_border()
  
  # set fontsize
  ft <- fontsize(ft, size = fontsize, part = "all")
  
  # set header in bold
  ft <- bold(ft, part = "header")
  
  # set font
  ft <- ft %>% font(fontname = fontname, part = "all")
  
  # set border
  if(border) {
    ft <- border_remove(ft) %>%
        border_outer(border = bd, part = "all")%>% 
        vline(border = bd, part = "body") %>%
        vline(border = bd, part = "header")
    if(style == "presentation")
      ft <- ft %>% hline(border = bd, part = "body")
    if(!is.null(title))
      ft <- ft %>% hline(j = length(title), border = bd, part = "header") 
  }
  
  ## change color text + background
  # header text color:
  if(isColorElSpec("header"))
    ft <- ft %>% color(color = colorTable["header"], part = "header")
  # footer text color
  if(isColorElSpec("footer"))
    ft <- ft %>% color(color = colorTable["footer"], part = "footer")
  # footer background color
  if(isColorElSpec("footerBackground"))
    ft <- ft %>% bg(bg = colorTable["footerBackground"], part = "footer")
  # general text color
  if(isColorElSpec("body"))
    ft <- ft %>% color(color = colorTable["body"], part = "body")
  
  # highlight columns:
  nCol <- length(ft$body$dataset)
  idxHigh <- highlight + shiftCol
  idxNonHigh <- setdiff(seq_len(nCol), highlight + shiftCol) 
  if(length(idxHigh) > 0) {
    if(isColorElSpec("headerBackgroundHighlight")) {
      ft <- ft %>% 
          bg(
              bg = colorTable["headerBackgroundHighlight"], 
              part = "header", j = idxHigh
          )
    } else {
      warning(
          "Header background color for highlighted columns",
          " should be specified",
          " (in 'headerBackgroundHighlight' in colorTable),",
          " highlighting is ignored."
      )
    }
  }
  if(length(idxNonHigh) > 0) {
    if(isColorElSpec("headerBackground")) {
      ft <- ft %>% 
          bg(
              bg = colorTable["headerBackground"], 
              part = "header", j = idxNonHigh
          )
    } else {
      warning(
          "Header background color for non highlighted columns",
          " should be specified",
          " (in 'headerBackground' in colorTable),",
          " bg color for non-highlighted columns is ignored."
      )
    }
  }
  
  # alternate background between elements of either column 'bgVar' of first column
  if(!is.null(bgVar)) bgIdx <- which(colnames(data) %in% bgVar) else bgIdx <- 1
  xBg <- convertVectToBinary(x = data[, bgIdx])
  if(length(idxNonHigh) > 0) {
    if(isColorElSpec("bodyBackground1")) {
      ft <- ft %>%
          bg(
              bg = colorTable["bodyBackground1"], 
              i = which(xBg %% 2 == 0), 
              part = "body", 
              j = idxNonHigh
          )
    }
    if(isColorElSpec("bodyBackground2")) {
      ft <- ft %>%
          bg(
              bg = colorTable["bodyBackground2"], 
              i = which(xBg %% 2 == 1), 
              part = "body", 
              j = idxNonHigh
          )
    }
    
    if(length(idxHigh) > 0){
      if(isColorElSpec("bodyBackgroundHighlight1")) {
        ft <- ft %>%
            bg(
                bg = colorTable["bodyBackgroundHighlight1"], 
                i = which(xBg %% 2 == 0), 
                part = "body", 
                j = idxHigh
            )
      }
      if(isColorElSpec("bodyBackgroundHighlight2")) {
        ft <- ft %>%
            bg(
                bg = colorTable["bodyBackgroundHighlight2"], 
                i = which(xBg %% 2 == 1), 
                part = "body", 
                j = idxHigh
            )
      }
    }
    
    if(isColorElSpec("bodyBackground"))
      ft <- ft %>% bg(bg = colorTable["bodyBackground"], part = "body")
  }
  
  if(adjustWidth) {
    widthPage <- getDimPage(
        type = "width", landscape = landscape, margin = margin,
        style = style, pageDim = pageDim
    )
    width <- widthPage/nCol
    ft <- width(ft, width = width)
  }
  
  if(align)
    ft <- align(ft, align = "center", part = "all")
  
  # by default, height of each header/footer (excepted the first one) line is quite big
  ft <- height(ft, height = dim_pretty(ft, part = "header")$heights, part = "header")
  ft <- height(ft, height = dim_pretty(ft, part = "footer")$heights, part = "footer")
  
  return(ft)
  
}



#' Convert vector to a bincode of 0/1
#' based on consecutive values in the vector.
#' @param x Vector.
#' @return Integer vector of same length than \code{x}.
#' @author Laure Cougnaud
#' @examples 
#' x <- c("group1", "group1", "group1", "group2", "group2", "group3", "group4", "group4")
#' convertVectToBinary(x = x)
#' @export
convertVectToBinary <- function(x) {
  
  xBin <- rep(NA, length(x))
  idxChg <- c(1, which(diff(as.numeric(factor(x, exclude = FALSE))) != 0) + 1)
  xBin[idxChg] <- rep(c(0, 1), length.out = length(idxChg))
  for(i in seq_along(xBin)) {
    if(is.na(xBin[i]))	xBin[i] <- xBin[i-1]
  }
  return(xBin)
  
}

#' Create a flextable, setting the column names to syntactic names
#' if it is not the case.
#' @param data Data.frame with data.
#' @param headerDf (optional) Data.frame with header.
#' This should contain the same number of columns than \code{data}
#' (+ if \code{includeRownames} is TRUE) and optionally multiple rows. 
#' Neighbouring cells with same content
#' will be represented merged in the output.
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' @param includeRownames Logical, if TRUE (by default)
#' rownames are included in the \code{\link[flextable]{flextable}} object.
#' @inheritParams inTextSummaryTable-flextable-args
#' @return list with:
#' \itemize{
#' \item{'ft': \code{\link[flextable]{flextable}}}
#' \item{'colsData': Named vector with original column names,
#' with names set to new syntactic names.}
#' }
#' @author Laure Cougnaud
#' @importFrom magrittr "%>%"
#' @import flextable
#' @importFrom stats setNames
#' @export
createFlextableWithHeader <- function(data, 
  headerDf = NULL, title = NULL,
  includeRownames = TRUE,
  colHeaderMerge = TRUE) {
  
  # bind rownames with data (not included in flextable by default)
  if(includeRownames) {
    
    colnamesInit <- colnames(data)
    data <- cbind.data.frame(rownames(data), data, stringsAsFactors = FALSE)
    colnames(data) <- c("", colnamesInit)
    
  }
  
  if(!is.null(headerDf) && ncol(headerDf) != ncol(data)) {
    stop(
        "Header should have the same number of columns than the data.",
        if(includeRownames)	" Did you forget to specify header for the rows?"
    )
  }
  
  # re-label the columns to avoid the error: 'invalid col_keys, flextable support only syntactic names'
  colsDataFt <- setNames(colnames(data), paste0("col", seq_len(ncol(data))))
  colnames(data) <- names(colsDataFt)
  
  if(!is.null(headerDf)) {
    colnames(headerDf) <- names(colsDataFt)
    # convert each column to character to avoid merging issue with 'rbind.data.frame'
    headerDf[, seq_len(ncol(headerDf))] <- lapply(headerDf, as.character)
  } else {
    headerDf <- as.data.frame(t(colsDataFt), stringsAsFactors = FALSE)
  }
  
  # add title
  if(!is.null(title) && !(length(title) == 1 && title == "")) {
    titleDf <- replicate(length(colsDataFt), title)
    if(is.matrix(titleDf))	colnames(titleDf) <- names(colsDataFt)
    headerDf <- rbind.data.frame(titleDf, headerDf, stringsAsFactors = FALSE)
  }
  
  mapping <- as.data.frame(t(headerDf), stringsAsFactors = FALSE)
  mapping$`col_keys` <- rownames(mapping)
  
  # base flextable
  ft <- flextable(data)
  
  ft <- set_header_df(x = ft, mapping = mapping)
  
  # merge cells with similar content in the header
  if(colHeaderMerge)
    ft <- mergeHeaderCols(x = ft, mapping = mapping)
  
  res <- list(ft = ft, colsData = colsDataFt)
  return(res)
  
}

#' Merge columns and rows in the header of a flextable object.
#' 
#' This:
#' \itemize{
#' \item{(1) for each row: checks that subsequent columns are identical (and corresponding
#' sub-header overlap), so should be merged}
#' \item{(2) for each column: checks that subsequent rows are identical, so should be
#' merged}
#' \item{filters cells planned to be merged across rows (2) if they are already
#' merged across columns (1)}
#' }
#' @inheritParams flextable::set_header_df
#' @return Updated flextable object with merged cells in the header.
#' @importFrom flextable set_header_df
#' @keywords internal
mergeHeaderCols <- function(x, mapping){
  
  headerCnt <- mapping[, setdiff(colnames(mapping), "col_keys"), drop = FALSE]
  headerCnt <- t(headerCnt)
  nRows <- nrow(headerCnt)
  nCols <- ncol(headerCnt)
  
  # get indices of replicated elements in a vector
  getDuplEl <- function(x){
    xRle <- rle(x)$lengths
    end <- cumsum(xRle)
    start <- end - xRle + 1
    idx <- mapply(FUN = c, start, end, SIMPLIFY = FALSE)
    idx <- idx[which(start != end)]
    if(length(x) > 0){
      # add intermediate indices
      idx <- lapply(idx, function(x) seq(from = x[1], to = x[2]))
    }
    return(idx)
  }
  
  ## column merging
  idxColToMerge <- lapply(seq_len(nRows), function(iRow){
    # consider the current and previous column headers
    xColHead <- apply(headerCnt[seq_len(iRow), , drop = FALSE], 2, paste, collapse = ".")
    lapply(getDuplEl(xColHead), function(x) list(row = iRow, col = x))
  })
  idxColToMerge <- unlist(idxColToMerge, recursive = FALSE)
  
  # if columns to be merged are identical across consecutive rows
  # merge the rows together into one single merging item
  colsToMerge <- sapply(idxColToMerge, function(x) paste(x[["col"]], collapse = "."))
  idxToRemove <- c()
  for(colComb in unique(colsToMerge)){
    idxCols <- which(colsToMerge == colComb)
    if(length(idxCols) > 1){
      rows <- unique(unlist(lapply(idxColToMerge[idxCols], `[[`, "row")))
      cols <- unique(unlist(lapply(idxColToMerge[idxCols], `[[`, "col")))
      for(row in rows){
        # if last row contains the same columns to merge...
        if( (row-1) %in% rows){
          # ... and the same elements
          if(length(unique(c(headerCnt[rows, cols]))) == 1){
            # combine the two mergings
            idx1 <- idxCols[which(rows == (row-1))]
            idx2 <- idxCols[which(rows == row)]
            idxColToMerge[[idx1]][["row"]] <- c(idxColToMerge[[idx1]][["row"]], idxColToMerge[[idx2]][["row"]])
            idxToRemove <- c(idxToRemove, idx2)
          }
        }
      }
    }
  }
  if(length(idxToRemove) > 0)
    idxColToMerge <- idxColToMerge[-idxToRemove]
  
  ## row merging
  idxRowToMerge <- lapply(seq_len(nCols), function(iCol){
    idxRowToMerge <- lapply(seq_len(nRows), function(iRow){
      if(iRow != 1){
        # consider the current and previous column headers
        xColHead <- c(headerCnt[seq_len(iRow), iCol, drop = FALSE])
        lapply(getDuplEl(xColHead), function(x) list(row = x, col = iCol))
      }
    })
    idxRowToMerge <- unlist(idxRowToMerge, recursive = FALSE)
  })
  idxRowToMerge <- unlist(idxRowToMerge, recursive = FALSE)
  
  ## remove row to merge which are already included in column merging
  
  # get cell index
  getCellIdx <- function(x){
    xGrid <- do.call(expand.grid, x)
    xInteract <- do.call(interaction, c(xGrid, list(drop = FALSE)))
    return(levels(xInteract))
  }
  idxColCells <- unlist(lapply(idxColToMerge, getCellIdx))
  idxRowToMerge <- lapply(idxRowToMerge, function(x){
    if(!any(getCellIdx(x) %in% idxColCells))
      x
  })
  
  # index of cells to be merged across rows or columns
  idxMerge <- c(idxColToMerge, idxRowToMerge)
  idxMerge <- idxMerge[!sapply(idxMerge, is.null)]
  
  # merge cells in flextable
  for(idx in idxMerge){
    x <- x %>% flextable::merge_at(i = idx[["row"]], j = idx[["col"]], part = "header")
  }

  return(x)
  
}