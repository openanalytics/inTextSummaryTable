context("Test miscellaneous functions")

test_that("'roundHalfUp' function returns correct results", {
      
      ## positive value
      expect_equal(roundHalfUp(x = 0.001, digits = 1), 0)		
      
      expect_equal(roundHalfUp(x = 4.0, digits = 2), 4.00)
      expect_equal(roundHalfUp(x = 0.55, digits = 1), 0.6)
      expect_equal(roundHalfUp(x = 0.55, digits = 1), round(0.55, 1))
      
      # differ than R default: 'round to even'
      expect_equal(roundHalfUp(x = 0.45, digits = 1), 0.5)
      expect_false(roundHalfUp(x = 0.45, digits = 1) == round(0.45, 1))
      
      # test case leading to issue with implementation in version < 0.21.0 (was returning 18.27)
      expect_equal(roundHalfUp(x = 18.275, digits = 2), 18.28)
      
      ## negative numbers
      expect_equal(roundHalfUp(x = -0.55, digits = 1), -0.6)
      expect_equal(roundHalfUp(x = -0.45, digits = 1), -0.5)
      
      ## very small number
      expect_equal(roundHalfUp(x = 1e-300, digits = 300), 1e-300)
      
    })

test_that("'roundHalfUpTextFormat' function returns correct rounding", {
      
      expect_equal(roundHalfUpTextFormat(x = 4.0, digits = 2), "4.00")
      expect_equal(roundHalfUpTextFormat(x = 0.55, digits = 1), "0.6")
      expect_equal(
          roundHalfUpTextFormat(x = 0.55, digits = 1),
          as.character(round(0.55, 1))
      )
      
    })

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

