context("Create a flextable with header")

library(flextable)
library(xml2)

data <- iris

test_that("Create a flextable simple header", {		
      
      # no header
      expect_is(res <- createFlextableWithHeader(data = data), "list")
      expect_is(res$ft, "flextable")
      expect_named(res$colsData)
      expect_identical(setdiff(res$colsData, ""), colnames(data))
      
      # in case ncol(headerDf) != ncol(data)
      headerDf <- as.data.frame(t(colnames(data)))
      expect_error(
          ft <- createFlextableWithHeader(
              data = iris, header = headerDf, 
              includeRownames = TRUE
          )
      )
      
      # simple header without rownames
      expect_is(
          ft <- createFlextableWithHeader(
              data = iris, header = headerDf, 
              includeRownames = FALSE
          )$ft,
          "flextable"
      )
      
      # simple header with rownames
      headerDfWithRownames <- cbind("rownames", headerDf)
      expect_is(
          ft <- createFlextableWithHeader(
              data = iris, header = headerDfWithRownames, 
              includeRownames = TRUE
          )$ft,
          "flextable"
      )
      
    })

test_that("Create a flextable with header and title", {
      
      title <- c("Table: content of the iris dataset", "This is an informative subtitle")
      headerDf <- as.data.frame(t(colnames(data)))
      expect_is(
          ftWithTitle <- createFlextableWithHeader(
              data = iris, header = headerDf, 
              title = title,
              includeRownames = FALSE
          )$ft,
          "flextable"
      )
      
      file <- "tableWithTitle.html"
      save_as_html(ftWithTitle, path = file)
      
      tableXML <- read_xml(file)
      header <- xml_find_first(tableXML, ".//thead")
      
      for(i in seq_along(title)){
        
        titleEl <- xml_find_first(xml_child(header, search = i), ".//td")
        expect_identical(xml_attr(titleEl, "colspan"), as.character(ncol(data)), label = "size of title in table")
        expect_identical(xml_text(titleEl), title[i], label = "title in table")
        
      }
      
      unlink(file) # clean
      
    })

test_that("Create a flextable with multiple row/column header", {
      
      # example table with multi-column and multi-row
      multiColTitle <- "Flower characteristics"
      multiRowTitle <- "Species"
      headerDf <- as.data.frame(rbind(
              c(rep(multiColTitle, 4), multiRowTitle),
              colnames(iris)
          ))
      
      expect_silent(
          ftHeader <- createFlextableWithHeader(
              data = iris, 
              headerDf = headerDf, 
              includeRownames = FALSE
          )$ft
      )
      
      file <- "tableWithHeader.html"
      save_as_html(ftHeader, path = file)
      
      tableXML <- read_xml(file)
      header <- xml_find_first(tableXML, ".//thead")
      
      ## first row header should contain multi column and multi row
      headerMultiple <- xml_child(x = header, search = 1)
      
      # multi-column columns
      multiCol <- xml_child(headerMultiple, 1)
      expect_identical(xml_attr(multiCol, "colspan"), "4", label = "size of multi-column in table")
      expect_identical(xml_text(multiCol), multiColTitle, label = "title of multi-column in table")
      
      # multi-row column ('Species')
      multiRow <- xml_child(headerMultiple, 2)
      expect_equal(xml_attr(multiRow, "rowspan"), "2", label = "size of multi-row in table")
      expect_identical(xml_text(multiRow), multiRowTitle, label = "title of multi-row in table")
      
      ## second row header should contain table column names
      headerWithColnames <- xml_child(x = header, search = 2)
      headerWithColnamesCnt <- sapply(xml_children(headerWithColnames), xml_text)
      expect_identical(headerWithColnamesCnt, setdiff(colnames(iris), "Species"))
      
      unlink(file) # clean
      
    })
