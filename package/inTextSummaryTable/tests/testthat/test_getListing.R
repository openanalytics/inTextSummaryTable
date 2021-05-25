context("Get a listing")

data <- head(mtcars)

library(flextable)

test_that("a data or flextable object should be specified", {
      
      expect_error(ft <- getListing())
      
    })

test_that("a flextable is created in report style", {
      
      expect_silent(ft <- getListing(data = data, style = "report"))
      
      expect_is(ft, "flextable")
      
      expect_setequal(ft$body$styles$text$font.size$data, 8)
      
      expect_setequal(ft$body$styles$text$font.family$data, "Times")
      
    })

test_that("table is created in presentation style", {
      
      expect_silent(ftPres <- getListing(data = data, style = "presentation"))
      expect_is(ftPres, "flextable")
      
      expect_setequal(ftPres$body$styles$text$font.size$data, 10)
      
      expect_setequal(ftPres$body$styles$text$font.family$data, "Tahoma")
      
    })

test_that("columns and rows are highlighted", {
      
      ### highlight columns	
      
      idxHighlight <- c(2, 3)
      
      # missing color
      expect_warning(
          ftHighlight <- getListing(
              data = data, 
              highlight = idxHighlight, 
              style = "presentation",
              colorTable = NULL
          )
      )
      
      # correct spec
      expect_silent(
          ftHighlight <- getListing(
              data = data, 
              highlight = idxHighlight, 
              style = "presentation",
              includeRownames = FALSE
          )
      )
      
      # check if background colors in highlighted columns are different in non highlighted columns
      
      for(el in c("header", "body")){		
        
        bgColors <- ftHighlight[[el]]$styles$cells$background.color$data
        tmp <- apply(bgColors, 1, function(row){
              expect_false(unique(row[idxHighlight]) == unique(row[-idxHighlight]))
            })
        
      }
      
      ### highlight rows
      
      # no rownames -> warning
      dataNoRn <- data
      rownames(dataNoRn) <- NULL
      expect_warning(
          ftHighlight <- getListing(
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
          ftHighlight <- getListing(
              data = dataWithRn, 
              highlight = 0, 
              style = "presentation",
              includeRownames = TRUE
          )
      )
      
      for(el in c("header", "body")){		
        
        bgColors <- ftHighlight[[el]]$styles$cells$background.color$data
        
        # are rows included?
        expect_equal(ncol(bgColors), ncol(dataWithRn) + 1)
        tmp <- apply(bgColors, 1, function(row){
              expect_false(unique(row[1]) == unique(row[-1]))
            })
        
      }
      
    })

test_that("table is created in landscape format", {
      
      ftPortrait <- getListing(data = data, landscape = FALSE)
      ftLandscape <- getListing(data = data, landscape = TRUE)
      
      expect_gte(sum(ftLandscape$body$colwidths), sum(ftPortrait$body$colwidths))
      expect_gte(sum(ftLandscape$header$colwidths), sum(ftPortrait$header$colwidths))
      
    })

test_that("table width is properly adjusted", {
      
      widths <- c(2, 50)
      tableWidths <- numeric()
      for(wid in widths){
        
        ft <- getListing(
            data = data, 
            pageDim = c(wid, 50), margin = 0,
            adjustWidth = TRUE
        )
        expect_equal(sum(ft$body$colwidths), wid)
        
      }
      
    })

test_that("a title is specified", {
      
      title <- "Subset of the cars dataset"
      ft <- getListing(data = data, title = title)
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

test_that("Alternate background color", {
      
      ft <- getListing(
          data = data, bgVar = "cyl",
          style = "presentation"
      )
      expect_identical(
          ft$body$content$content$default[[1]]$`shading.color`,
          NA_character_
      )
      
    })
