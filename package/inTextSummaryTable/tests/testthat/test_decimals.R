context("Test 'formatPercentage'")

test_that("'formatPercentage' function returns correct formatting", {
			
			expect_is(class(formatPercentage(x = 45.6)), "character")

			expect_equal(formatPercentage(x = 0.55, nDec = 2), "0.55")
			expect_equal(formatPercentage(x = 48.43, nDec = 1), "48.4")
			expect_equal(formatPercentage(x = 48.0, nDec = 1), "48.0")
			expect_equal(formatPercentage(x = 100, nDec = 2), "100")
			expect_equal(formatPercentage(x = NA), "-")
			expect_equal(formatPercentage(x = 0.01), "<0.1")
			expect_equal(formatPercentage(x = 99.9998), ">99.9")
			
		})