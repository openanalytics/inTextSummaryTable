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

