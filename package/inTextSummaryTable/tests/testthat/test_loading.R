context("Load the package")

test_that("there is a reference to the documentation", {
			
	detach('package:inTextSummaryTable', unload = TRUE)
	expect_message(
		library(inTextSummaryTable),
		regexp = "see ? inTextSummaryTable to get started",
		fixed = TRUE
	)
			
})