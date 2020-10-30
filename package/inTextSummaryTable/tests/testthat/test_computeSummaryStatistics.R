context("Computation of summary statistics")

test_that("stats for continuous variable (including missing) is correctly computed", {
	
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = sample(10, 5))
	statsCont <- computeSummaryStatistics(data = dataCont, var = "x")
	
	expect_s3_class(statsCont, "data.frame")
	
	expect_named(
		object = statsCont, 
		expected = c("statN", "statm", "statMean", "statSD", "statSE", "statMedian", "statMin", "statMax"),
		ignore.order = TRUE
	)
	
	expect_equal(statsCont[, "statN"], 4)
	expect_equal(statsCont[, "statm"], 4)	
	expect_equal(statsCont[, "statMean"], 5)
	expect_equal(statsCont[, "statMedian"], 4.5)
	expect_equal(statsCont[, "statSD"], sd(dataCont$x, na.rm = TRUE))
	expect_equal(statsCont[, "statSE"], statsCont[, "statSD"]/sqrt(statsCont[, "statm"]))
	expect_equal(statsCont[, "statMin"], 1)
	expect_equal(statsCont[, "statMax"], 10)
	
})

test_that("stats for categorical variable (including missing) is correctly computed", {
			
	dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = sample(10, 5))
	statsCat <- computeSummaryStatistics(data = dataCat, var = "x")
			
	expect_s3_class(statsCat, "data.frame")
			
	expect_named(
		object = statsCat, 
		expected = c("variableGroup", "statN", "statm"),
		ignore.order = TRUE
	)
			
	expect_s3_class(statsCat[, "variableGroup"], "factor")
	expect_identical(statsCat[, "variableGroup"], factor(c("A", "B")))
	
	expect_equal(statsCat[, "statm"], c(1, 3))
	expect_equal(statsCat[, "statN"], c(1, 3))

})

test_that("stats for empty variable returns results only if 'filterData' is specified", {
			
	emptyData <- data.frame(x = numeric(), USUBJID = character())
	
	# without filter
	expect_null(computeSummaryStatistics(data = emptyData, var = "x"))
	
	expect_silent(
		statsEmptyData <- computeSummaryStatistics(
			data = data.frame(x = numeric(), USUBJID = character()), 
			var = "x",
			filterEmptyVar = FALSE
		)
	)
	expect_s3_class(statsEmptyData, "data.frame")
	expect_named(
		object = statsEmptyData, 
		expected = c("statN", "statm", "statMean", "statSD", "statSE", "statMedian", "statMin", "statMax"),
		ignore.order = TRUE
	)
	
})

test_that("failure if variable is not available", {
	expect_error(
		computeSummaryStatistics(
			data = data.frame(x = 1:10), 
			var = "y"
		),
		pattern = "Variable to summarize .* not available"
	)		
})
			

test_that("failure if subjectVar is not available", {
	expect_error(
		computeSummaryStatistics(
			data = data.frame(x = 1:10), 
			var = "x", 
			subjectVar = "blabla"
		),
		pattern = "Subject variable .* not available"
	)		
})

test_that("failure if multiple records available per subject", {
	dataCont <- data.frame(x = seq.int(5), USUBJID = c(1, 1, 2, 2, 2))
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x"),
		pattern = "Extraction of statistics failed .* because multiple records are available"
	)		
})
