context("Test 'roundCustomText'")

test_that("'roundCustomText' function returns correct rounding", {
			
	expect_equal(roundCustomText(x = 4.0, digits = 2), "4.00")
	expect_equal(roundCustomText(x = 0.55, digits = 1), "0.6")
	expect_equal(roundCustomText(x = 0.55, digits = 1), as.character(round(0.55, 1)))
	
	expect_warning(roundCustomText(x = 4.0, digits = 2, format = "text"))
	
})

test_that("Conversion of a flag variable", {
			
	x <- c("Y", "N", '')	
	xFL <- inTextSummaryTable:::convertVarFlag(x)
	expect_is(xFL, "factor")
	
	# correct levels
	expect_equivalent(levels(xFL), c("", "N"))
	
	# correct conversions
	expect_equal(as.character(xFL), c("", "N", NA_character_))
	
	# wrong input
	expect_error(inTextSummaryTable:::convertVarFlag(c(NA_character_, x)), pattern = "*should only contain*")
	expect_error(inTextSummaryTable:::convertVarFlag(c("blabla", x)), , pattern = "*should only contain*")
			
})

