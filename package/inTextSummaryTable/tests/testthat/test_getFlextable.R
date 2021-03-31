context("Get GLPG flextable")

data <- head(mtcars)

library(flextable)
library(xml2)

# export table and get JSON table
exportAndGetFlextableXML <- function(ft, label){
  file <- paste0("table-", label, ".html")
  flextable::save_as_html(ft, path = file)
  tableXML <- xml2::read_xml(file)
  unlink(file)
  return(tableXML)
}

test_that("a data or flextable object should be specified", {
      
      expect_error(ft <- getFlextable())
      
    })

test_that("a flextable is created from data", {
      
      expect_silent(ft <- getFlextable(data = data))
      
      expect_is(ft, "flextable")
      
      tableXML <- exportAndGetFlextableXML(ft, label = "default")
      tableCells <- xml_find_all(tableXML, './/p') # extract paragraphs
      tableCellsCntStyle <- sapply(tableCells, function(x) xml_attr(xml_contents(x), "style"))
      
      font <- unique(sub(".*font-family:'(.+)'(;|$).*", "\\1", tableCellsCntStyle))
      expect_equal(font, "Times")
      fontsize <- sub(".+font-size:([[:alnum:]]{1,});.+", "\\1", tableXML)
      fontsize <- as.numeric(sub("^(\\d{1,}).+", "\\1", fontsize))
      expect_equal(fontsize, 8)
      
    })

test_that("table is created in presentation style", {
      
      expect_silent(ftPres <- getFlextable(data = data, style = "presentation"))
      expect_is(ftPres, "flextable")
      
      tableXML <- exportAndGetFlextableXML(ftPres, label = "presentation")
      tableCells <- xml_find_all(tableXML, './/p') # extract paragraphs
      tableCellsCntStyle <- sapply(tableCells, function(x) xml_attr(xml_contents(x), "style"))
      
      font <- unique(sub(".*font-family:'(.+)'(;|$).*", "\\1", tableCellsCntStyle))
      expect_equal(font, "Tahoma")
      fontsize <- sub(".+font-size:([[:alnum:]]{1,});.+", "\\1", tableXML)
      fontsize <- as.numeric(sub("^(\\d{1,}).+", "\\1", fontsize))
      expect_equal(fontsize, 10)
      
    })

test_that("columns and rows are highlighted", {
      
      ### highlight columns	
      
      idxHighlight <- c(2, 3)
      
      # missing color
      expect_warning(
          ftHighlight <- getFlextable(
              data = data, 
              highlight = idxHighlight, 
              style = "presentation",
              colorTable = NULL
          )
      )
      
      # correct spec
      expect_silent(
          ftHighlight <- getFlextable(
              data = data, 
              highlight = idxHighlight, 
              style = "presentation",
              includeRownames = FALSE
          )
      )
      
      tableXML <- exportAndGetFlextableXML(ftHighlight, label = "highlight")
      
      # utility wrappers
      getBgColors <- function(x){
        bgColors <- lapply(strsplit(x, split = ";"), 
            grep, pattern = "background-color", value = TRUE, fixed = TRUE
        )
        bgColors <- unlist(bgColors)
        bgColors <- sub("background-color:", "", bgColors)
      }
      testBgColors <- function(bgColors, idx){
        # is background color specified?
        expect_gt(length(bgColors), 0)
        # is background color for header of highlighted columns != non highlighted columns
        expect_false(unique(bgColors[idx]) == unique(bgColors[-idx]))
      }
      
      ## header
      header <- xml_find_first(tableXML, ".//thead")
      headerCols <- xml_find_all(header, ".//td")
      headerColsStyle <- sapply(headerCols, xml_attr, "style")
      headerBgColors <- getBgColors(headerColsStyle)
      testBgColors(headerBgColors, idx = idxHighlight)
      
      ## body
      body <- xml_find_first(tableXML, ".//tbody")
      body <- xml_children(body)
      bodyTd <- lapply(body, xml_find_all, xpath = ".//td")
      bodyStyles <- lapply(bodyTd, xml_attr, "style")
      bodyBgColors <- lapply(bodyStyles, getBgColors)
      tmp <- lapply(bodyBgColors, testBgColors, idx = idxHighlight)
      
      ### highlight rows
      
      # no rownames -> warning
      dataNoRn <- data
      rownames(dataNoRn) <- NULL
      expect_warning(
          ftHighlight <- getFlextable(
              data = dataNoRn, 
              highlight = 0, 
              style = "presentation",
              includeRownames = FALSE
          )
      )
      
      # highlight rownames
      dataWithRn <- data
      rownames(dataWithRn) <- seq_len(nrow(dataWithRn))
      expect_silent(
          ftHighlight <- getFlextable(
              data = dataWithRn, 
              highlight = 0, 
              style = "presentation",
              includeRownames = TRUE
          )
      )
      tableXML <- exportAndGetFlextableXML(ftHighlight, label = "highlight")
      body <- xml_find_first(tableXML, ".//tbody")
      body <- xml_children(body)
      bodyTd <- lapply(body, xml_find_all, xpath = ".//td")
      bodyStyles <- lapply(bodyTd, xml_attr, "style")
      bodyBgColors <- lapply(bodyStyles, getBgColors)
      tmp <- lapply(bodyBgColors, testBgColors, idx = 1)
      
    })

test_that("table is created in landscape format", {
      
      # check than width table in landscape > width table in portrait
      layouts <- c("portrait", "landscape")
      tableWidths <- numeric()
      for(lay in layouts){
        
        ft <- getFlextable(data = data, landscape = (lay == "landscape"))
        
        tableXML <- exportAndGetFlextableXML(ft, label = lay)
        
        header <- xml_find_first(tableXML, ".//thead")
        headerStyle <- lapply(xml_find_all(header, ".//td"), xml_attr, "style")
        headerWidths <- sub(".*width:([[:alnum:]]{1,});.*", "\\1", headerStyle)
        headerWidths <- as.numeric(sub("^(\\d{1,}).+", "\\1", headerWidths))
        tableWidths[lay] <- sum(headerWidths)
        
      }
      
      expect_gt(tableWidths["landscape"], tableWidths["portrait"])
      
    })

test_that("table width is properly adjusted", {
      
      # Table width in px in output HTML and specified in inches as input (in*res = px)
      # so doesn't check if dim figure is exact in output
      # but check ratio of width for two tables with specified width:
      widths <- c(30, 125)
      tableWidths <- numeric()
      for(wid in widths){
        
        ft <- getFlextable(
            data = data, 
            pageDim = c(wid, 50), margin = 0,
            adjustWidth = TRUE
        )
        tableXML <- exportAndGetFlextableXML(ft, label = as.character(wid))
        
        header <- xml_find_first(tableXML, ".//thead")
        headerStyle <- lapply(xml_find_all(header, ".//td"), xml_attr, "style")
        headerWidths <- sub(".*width:([[:alnum:]]{1,});.*", "\\1", headerStyle)
        headerWidths <- as.numeric(sub("^(\\d{1,}).+", "\\1", headerWidths))
        tableWidths[as.character(wid)] <- sum(headerWidths)
        
      }
      
      expect_identical(Reduce("/", tableWidths), Reduce("/", as.numeric(names(tableWidths))))
      
    })

test_that("a title is specified", {
      
      title <- "Subset of the cars dataset"
      ft <- getFlextable(data = data, title = title)
      expect_setequal(
          ft$header$dataset[1, ],
          title
      )
      # a border
      expect_setequal(
          ft$header$styles$cells$border.color.bottom$data[1, ],
          c("#000000", "black")
      )
      
    })


test_that("a vector is converted to a binary vector", {
      
      x <- c("group1", "group1", "group1", "group2", "group2", "group3", "group4", "group4")			
      expect_equivalent(
          convertVectToBinary(x = x),
          c(0, 0, 0, 1, 1, 0, 1, 1)
      )
      
    })

