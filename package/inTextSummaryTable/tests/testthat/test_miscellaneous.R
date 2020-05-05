context("Test 'roundCustomText'")

test_that("'roundCustomText' function returns correct rounding", {
			
			expect_equal(roundCustomText(x = 4.0, digits = 2), "4.00")
			expect_equal(roundCustomText(x = 0.55, digits = 1), "0.6")
			expect_equal(roundCustomText(x = 0.55, digits = 1), as.character(round(0.55, 1)))
			
			expect_warning(roundCustomText(x = 4.0, digits = 2, format = "text"))
			
		})