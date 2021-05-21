context("Test miscellaneous functions")

test_that("Conversion of a flag variable", {
      
      x <- c("Y", "N", '')	
      xFL <- convertVarFlag(x)
      expect_is(xFL, "factor")
      
      # correct levels
      expect_equivalent(levels(xFL), c("", "N"))
      
      # correct conversions
      expect_equal(as.character(xFL), c("", "N", NA_character_))
      
      # wrong input
      expect_error(convertVarFlag(c(NA_character_, x)), pattern = "*should only contain*")
      expect_error(convertVarFlag(c("blabla", x)), , pattern = "*should only contain*")
      
    })

test_that("Error in extraction of page dimensions", {
      
      expect_error(
          getDimPage(type = "width", pageDim = "ciao"),
          "'pageDim' should be a numeric vector."
      )
      
      expect_error(
          getDimPage(type = "width", pageDim = c(10, 2, 3)),
          "'pageDim' should be of length 2."
      )
      
    })

test_that("Extraction of page dimensions for report is correct", {
      
      expect_equal(
          object = getDimPage(type = "width", style = "report", margin = 0),
          expected = 21/2.54
      )
      expect_equal(
          object = getDimPage(type = "height", style = "report", margin = 0),
          expected = 29.7/2.54
      )
      
    })

test_that("Extraction of page dimensions for multiple dimensions is successful (and in correct order)", {
      
      expect_equal(
          object = getDimPage(type = c("height", "width"), style = "report"),
          expected = rev(getDimPage(type = c("width", "height"), style = "report"))
      )
      
    })

test_that("Extraction of page dimensions for presentation is correct", {
      
      expect_equal(
          object = getDimPage(type = "width", style = "presentation", margin = 0),
          expected = 10
      )
      expect_equal(
          object = getDimPage(type = "height", style = "presentation", margin = 0),
          expected = 7.5
      )
      
    })

test_that("Dimensions in portrait and landscape match", {
      
      expect_equal(
          object = getDimPage(type = "width", style = "report", margin = 0, landscape = FALSE),
          expected = getDimPage(type = "height", style = "report", margin = 0, landscape = TRUE)
      )
      expect_equal(
          object = getDimPage(type = "height", style = "report", margin = 0, landscape = FALSE),
          expected = getDimPage(type = "width", style = "report", margin = 0, landscape = TRUE)
      )
      
    })

test_that("Dimensions are correctly extracted with margins", {
      
      widthPageNoMargin <- getDimPage(type = "width", style = "report", margin = 0)
      expect_equal(
          getDimPage(type = "width", style = "report", margin = 3),
          expected = widthPageNoMargin-3*2
      )
      heightPageNoMargin <- getDimPage(type = "height", style = "report", margin = 0)
      expect_equal(
          getDimPage(type = "height", style = "report", margin = 3),
          expected = heightPageNoMargin-3*2
      )
      
    })

test_that("Specified page dimensions are correctly used", {
      
      pageDimSpecified <- c(10, 20)
      expect_equal(
          getDimPage(type = "width", style = "report", margin = 0, pageDim = pageDimSpecified),
          expected = pageDimSpecified[1]
      )
      expect_equal(
          getDimPage(type = "height", style = "report", margin = 0, pageDim = pageDimSpecified),
          expected = pageDimSpecified[2]
      )	
      
    })