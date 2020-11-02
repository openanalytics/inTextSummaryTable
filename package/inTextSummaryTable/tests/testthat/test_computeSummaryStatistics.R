context("Computation of summary statistics")

set.seed(123)

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


test_that("variable is properly checked", {
			
	dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = sample(10, 5))
	
	# empty var
	expect_error(statsVarNULL <- computeSummaryStatistics(data = dataCat, var = NULL), NA)
	expect_error(statsVarAll <- computeSummaryStatistics(data = dataCat, var = "all"), NA)
	expect_identical(statsVarNULL, statsVarAll)
	expect_equal(statsVarNULL, data.frame(statN = 5, statm = 5))
	
	# var not in data
	expect_error(
		computeSummaryStatistics(data = dataCat, var = "y"),
		"Variable to summarize.*not available in data."
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

test_that("total included if requested", {
		
	dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = sample(10, 5))
	statsCatWithTotal <- computeSummaryStatistics(data = dataCat, var = "x", varTotalInclude = TRUE)
	
	statsCat <- computeSummaryStatistics(data = dataCat, var = "x", varTotalInclude = FALSE)
	statsCatIncludeTotal <- rbind(statsCatWthtTotal, data.frame(variableGroup = "Total", statN = 4, statm = 4))
	
	expect_equal(object = statsCatWithTotal, expected = statsCatIncludeTotal)
			
})

test_that("extra statistics correctly included", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = sample(10, 5))
	
	# create example of custom function (coefficient of variation)
	statsFctX <- function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100
	statsFctData <- function(data) sd(data$x, na.rm = TRUE)/mean(data$x, na.rm = TRUE)*100
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = statsFctX),
		"should be a list",
	)
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(statsFctX)),
		"should be named"
	)
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(A = statsFctX, 2)),
		"should be named"
	)
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(statN = statsFctX)),
		"choose a different name"
	)
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(A = function(y) length(y))),
		"'statsExtra' should contain a parameter named 'x' or 'data'"
	)
	
	expect_silent(statsExtraFctX <- computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(A = statsFctX)))
	expect_true("A" %in% names(statsExtraFctX))
	expect_equal(statsExtraFctX$A, statsFctX(dataCont$x))
	
	expect_silent(statsExtraFctX <- computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(B = statsFctData)))
	expect_true("B" %in% names(statsExtraFctX))
	expect_equal(statsExtraFctX$B, statsFctData(dataCont))
	
})

test_that("statistic type is correct for categorical variable", {
		
	dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = sample(10, 5))
	expect_error(
		stats <- computeSummaryStatistics(data = dataCat, var = "x", type = "summaryTable"),
		"should be numeric"
	)
	expect_identical(
		computeSummaryStatistics(data = dataCat, var = "x", type = "auto"),
		computeSummaryStatistics(data = dataCat, var = "x", type = "countTable")
	)
	
})

test_that("statistic type is correct for continuous variable", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = sample(10, 5))
	expect_identical(
		computeSummaryStatistics(data = dataCont, var = "x", type = "auto"),
		computeSummaryStatistics(data = dataCont, var = "x", type = "summaryTable")
	)
	
	# count table can be requested for a continuous variable
	expect_error(computeSummaryStatistics(data = dataCont, var = "x", type = "countTable"), NA)
			
})

test_that("filtering of empty continuous variable correctly done", {
	
	# variable is considered empty if all missing:
	emptyDataNA <- data.frame(x = rep(NA_real_, 5), USUBJID = sample(10, 5))	
	expect_null(computeSummaryStatistics(data = emptyDataNA, var = "x"))
	expect_silent(
		statsEmptyDataNA <- computeSummaryStatistics(data = emptyDataNA, var = "x", filterEmptyVar = FALSE)
	)
	expect_equal(nrow(statsEmptyDataNA), 1)
	expect_equal(
		statsEmptyDataNA, 
		data.frame(statN = 0, statm = 0, statMean = NA_real_, 
			statSD = NA_real_, statSE = NA_real_, statMedian = NA_real_, 
			statMin = NA_real_, statMax = NA_real_
		)
	)
	
	# variable is empty is data.frame is itself empty
	emptyData <- data.frame(x = numeric(), USUBJID = character())
	expect_silent(
		statsEmptyData <- computeSummaryStatistics(data = emptyData, var = "x", filterEmptyVar = FALSE)
	)
	expect_equal(statsEmptyData, statsEmptyDataNA)
	
})	

test_that("filtering of empty categorical variable correctly done", {
			
	dataCat <- data.frame(
		x = factor(c(NA_character_, "B", "B", "B", "A"), levels = c("A", "B", "C")),
		USUBJID = sample(10, 5)
	)
			
	# filter
	expect_equal(nrow(computeSummaryStatistics(data = dataCat, var = "x", filterEmptyVar = TRUE)), 2)
	
	# no filter
	statsCatNoFilter <- computeSummaryStatistics(data = dataCat, var = "x", filterEmptyVar = FALSE)
	expect_equal(nrow(statsCatNoFilter), 3)
	expect_true("C" %in% statsCatNoFilter$variableGroup)
	expect_equal(subset(statsCatNoFilter, variableGroup == "C")$statN, 0)
	expect_equal(subset(statsCatNoFilter, variableGroup == "C")$statm, 0)
	expect_identical(computeSummaryStatistics(data = dataCat, var = "x"), statsCatNoFilter)
	
})

test_that("check for duplicates in continuous variable correctly done", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		y = rep(2, 5),
		USUBJID = c("A", "B", "B", "C", "D")
	)
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x"),
		"multiple records are available for the same USUBJID"
	)
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", msgVars = "y"),
		"multiple records are available for the same USUBJID.* USUBJID y x"
	)
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", msgLabel = "test dataset"),
		"for the test dataset .* multiple records are available for the same USUBJID"
	)
			
})

test_that("check for multiple values in continuous variable correctly done", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 1, 6, 10), 
		USUBJID = c("A", "B", "B", "C", "D")
	)
			
	expect_message(
		computeSummaryStatistics(data = dataCont, var = "x"),
		"duplicated values for x"
	)
	
})

	
