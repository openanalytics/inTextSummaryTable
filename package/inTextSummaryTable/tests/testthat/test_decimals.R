context("Format numbers")

test_that("Percentages are correctly formatted", {

	expect_equal(formatPercentage(x = 0.55, nDec = 2), "0.55")
	expect_equal(formatPercentage(x = 48.43, nDec = 1), "48.4")
	expect_equal(formatPercentage(x = 48.0, nDec = 1), "48.0")
	expect_equal(formatPercentage(x = 100, nDec = 2), "100")
	expect_equal(formatPercentage(x = NA), "-")
	expect_equal(formatPercentage(x = 0.01), "<0.1")
	expect_equal(formatPercentage(x = 99.9998), ">99.9")
	
})

test_that("The number of decimals is correctly extracted from the data", {
			
	# set of values above and below 0
	xOrdMag <- seq(-3, 3)
	x <- 10^(xOrdMag)
	expect_equal(
		object = getNDecimalsData(x), 
		expected = abs(ifelse(xOrdMag > 0, 0, xOrdMag))
	)

})

test_that("The number of decimals is set to missing is the value is missing", {
			
	expect_equal(getNDecimalsData(NA_real_), NA_real_)
	
})

test_that("An error is generated if the input data is not a numeric", {

	expect_error(getNDecimalsData("0.890"), "should be numeric")
				
})

test_that("The maximum number of decimals of a set of numbers is correctly extracted from the data", {

	x <- c(NA, 1e-2, 1e-3)
	expect_equal(getMaxNDecimalsData(x), 3)
			
})

test_that("The number of decimals is correctly extracted according to rule", {
		
	expect_equal(
		object = getNDecimalsRule(c(0.999, 1, 10, 1000), rule = "1"), 
		expected = c(3, 2, 1, 0)
	)
	
})

test_that("An error is generated if the specified rule for the number of decimals is not implemented", {
			
	expect_error(getNDecimalsRule(0.999, rule = "test"))
	
})

test_that("An error is generated if the input value for the extraction of the number of decimals according to rule is not a numeric", {
	
	expect_error(getNDecimalsRule("0.890"), "should be numeric")
			
})

test_that("The number of decimals is correctly extracted according to rule when specified", {
			
	nDec <- getNDecimals(
		x = 5.1, 
		useRule = TRUE, rule = "1", 
		useData = FALSE
	)
	expect_equal(
		object = nDec,
		expected = 2,
		label = "decimals from rule only"
	)
	
})

test_that("The number of decimals is correctly extracted from the data when specified", {
		
	nDec <- getNDecimals(
		x = 5.1, 
		useRule = FALSE, 
		useData = TRUE
	)
	expect_equal(
		object = nDec,
		expected = 1,
		label = "decimals from data only"
	)
	
})

test_that("The number of decimals is correctly extracted from the minimum of the data and rule when both are specified", {
			
	nDec <- getNDecimals(
		x = 5.1, 
		useRule = TRUE, rule = "1", 
		useData = TRUE
	)
	expect_equal(
		object = nDec,
		expected = 1,
		label = "decimals from rule and data"
	)
	
})

test_that("An error is generated if neither a rule or data are specified for the extraction of the number of decimals", {
			
	expect_error(
		getNDecimals(5.1, useRule = FALSE, useData = FALSE),
		"'useRule' and/or 'useData'"
	)
			
})

test_that("The maximum number of decimals of a set of numbers is correctly extracted from the data and rule", {
	
	x <- c(5.1, 6.8, 10.2, 80.0, NA_real_)
			
	# nDec based on rule is taken
	expect_equal(getMaxNDecimals(x), 1)
			
})
