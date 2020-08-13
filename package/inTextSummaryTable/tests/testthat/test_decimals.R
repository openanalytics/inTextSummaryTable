context("Test 'formatPercentage'")

test_that("Format percentage", {
			
	expect_is(formatPercentage(x = 45.6), "character")

	expect_equal(formatPercentage(x = 0.55, nDec = 2), "0.55")
	expect_equal(formatPercentage(x = 48.43, nDec = 1), "48.4")
	expect_equal(formatPercentage(x = 48.0, nDec = 1), "48.0")
	expect_equal(formatPercentage(x = 100, nDec = 2), "100")
	expect_equal(formatPercentage(x = NA), "-")
	expect_equal(formatPercentage(x = 0.01), "<0.1")
	expect_equal(formatPercentage(x = 99.9998), ">99.9")
	
})

test_that("Get number of decimals from data", {
			
	# set of values above and below 0
	xOrdMag <- seq(-3, 3)
	set.seed(123)
	x <- sample(100, size = length(xOrdMag))*10^(xOrdMag)
	expect_equal(getNDecimalsData(x), abs(ifelse(xOrdMag > 0, 0, xOrdMag)))
	
	# missing value
	expect_equal(getNDecimalsData(NA_real_), NA_real_)
	
	# character
	expect_error(getNDecimalsData("0.890"), "should be numeric")
				
})

test_that("Get maximum number of decimals from data", {

	x <- c(NA, 1e-2, 1e-3)
	expect_equal(getMaxNDecimals(x), 3)
			
})

test_that("Get number of decimals from specified rule", {
		
	expect_equal(
		object = getNDecimalsRule(c(0.999, 1, 10, 1000), rule = "1"), 
		expected = c(3, 2, 1, 0)
	)
	
	expect_error(getNDecimalsRule(0.999, rule = "test"))
	
	expect_error(getNDecimalsRule("0.890"), "should be numeric")
			
})

test_that("Get number of decimals", {

	# test rule and/or data for edge case:	
	expect_equal(
		object = getNDecimals(1e-2, useRule = TRUE, rule = "1", useData = FALSE),
		expected = 3,
		label = "decimals from rule only"
	)
	expect_equal(
		object = getNDecimals(1e-2, useRule = FALSE, useData = TRUE),
		expected = 2,
		label = "decimals from data only"
	)
	expect_equal(
		object = getNDecimals(1e-2, useRule = TRUE, rule = "1", useData = TRUE),
		expected = 2,
		label = "decimals from rule and data"
	)
	
	expect_error(
		getNDecimals(1e-2, useRule = FALSE, useData = FALSE),
		"'useRule' and/or 'useData'"
	)
			
})