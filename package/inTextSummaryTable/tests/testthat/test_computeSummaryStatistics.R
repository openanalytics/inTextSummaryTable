context("Compute summary statistics")

test_that("The statistics are correctly computed for a continuous variable (including missing)", {
	
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	statsCont <- computeSummaryStatistics(data = dataCont, var = "x")
	
	expect_s3_class(statsCont, "data.frame")
	
	statsContNameInt <- c("statN", "statm")
	statsContNameNum <- c("statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	expect_named(
		object = statsCont, 
		expected = c(statsContNameInt, statsContNameNum),
		ignore.order = TRUE
	)
	
	# check variable type
	for(col in statsContNameInt){
		expect_type(statsCont[, !!col], type = "integer")
	}
	for(col in statsContNameNum){
		expect_type(statsCont[, !!col], type = "double")
	}
	
	# check if computations are correct
	expect_equal(statsCont[, "statN"], 4)
	expect_equal(statsCont[, "statm"], 4)	
	expect_equal(statsCont[, "statMean"], 5)
	expect_equal(statsCont[, "statMedian"], 4.5)
	expect_equal(statsCont[, "statSD"], sd(dataCont$x, na.rm = TRUE))
	expect_equal(statsCont[, "statSE"], statsCont[, "statSD"]/sqrt(statsCont[, "statm"]))
	expect_equal(statsCont[, "statMin"], 1)
	expect_equal(statsCont[, "statMax"], 10)
	
})

test_that("The statistics are correctly computed for a categorical variable (including missing)", {
			
	dataCat <- data.frame(
		x = c(NA_character_, "B", "B", "B", "A"), 
		USUBJID = seq.int(5),
		stringsAsFactors = TRUE
	)
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


test_that("Counts are computed on the entire dataset if the variable is not specified", {
			
	data <- data.frame(
		x = c("A", "B"), 
		USUBJID = c("1", "2")
	)
	expect_equal(
		computeSummaryStatistics(data = data, var = NULL), 
		data.frame(statN = 2, statm = 2)
	)
	
})

test_that("Counts are computed on the entire dataset if the variable is set to 'all'", {
			
	data <- data.frame(
		x = c("A", "B"), 
		USUBJID = c("1", "2")
	)
	expect_equal(
		computeSummaryStatistics(data = data, var = "all"), 
		data.frame(statN = 2, statm = 2)
	)
	
})

test_that("Counts are computed similarly for empty variable or variable set to 'all'", {
			
	data <- data.frame(
		x = c("A", "B"), 
		USUBJID = c("1", "2")
	)
	expect_equal(
		computeSummaryStatistics(data = data, var = "all"), 
		computeSummaryStatistics(data = data, var = NULL)
	)
	
})

test_that("An error is generated if a specified variable is not available in the data", {
			
	data <- data.frame(
		x = c("A", "B"), 
		USUBJID = c("1", "2")
	)
	expect_error(
		computeSummaryStatistics(data = data, var = "y"),
		"Variable to summarize.*not available in data."
	)
	
})


test_that("An error is generated if the subject variable is not available in the data", {
	expect_error(
		computeSummaryStatistics(
			data = data.frame(x = 1:10), 
			var = "x", 
			subjectVar = "blabla"
		),
		pattern = "Subject variable .* not available"
	)		
})

test_that("An error is generated if multiple records are available per subject", {
	dataCont <- data.frame(x = seq.int(5), USUBJID = c(1, 1, 2, 2, 2))
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x"),
		pattern = "Extraction of statistics failed .* because multiple different records are available"
	)		
})

test_that("A warning is generated if multiple records are available per subject when requested", {
	dataCont <- data.frame(x = seq.int(5), USUBJID = c(1, 1, 2, 2, 2))
	expect_warning(
		computeSummaryStatistics(data = dataCont, var = "x", checkVarDiffBySubj = "warning"),
		pattern = "multiple different records are available"
	)		
})

test_that("No check is run for multiple records available per subject if requested", {
	dataCont <- data.frame(x = seq.int(5), USUBJID = c(1, 1, 2, 2, 2))
	expect_silent(
		computeSummaryStatistics(data = dataCont, var = "x", checkVarDiffBySubj = "none")
	)		
})

test_that("The variable total is correctly included when requested", {
		
	dataCat <- data.frame(
		x = c(NA_character_, "B", "B", "B", "A"), 
		USUBJID = seq.int(5)
	)
	statsCatWithTotal <- computeSummaryStatistics(
		data = dataCat, var = "x", varTotalInclude = TRUE
	)
	statsCatWthtTotal <- computeSummaryStatistics(
		data = dataCat, var = "x", varTotalInclude = FALSE
	)
	statsCatIncludeTotal <- rbind(
		statsCatWthtTotal, 
		data.frame(variableGroup = "Total", statN = 4, statm = 4)
	)
	
	expect_equal(
		object = statsCatWithTotal, 
		expected = statsCatIncludeTotal
	)
			
})

test_that("An error is generated if the type of the extra statistic is not correct", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "x", 
			statsExtra = function(x) x
		),
		"should be a list",
	)
	
})

test_that("An error is generated if the parameter of the extra statistic function is not correct", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)	
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "x", 
			statsExtra = list(A = function(y) length(y))
		),
		"'statsExtra' should contain a parameter named 'x' or 'data'"
	)
			
})

test_that("An error is generated if no names are specified for the extra statistics", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	
	# create example of custom function (coefficient of variation)
	statsFctX <- function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100	
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(statsFctX)),
		"should be named"
	)
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", statsExtra = list(A = statsFctX, 2)),
		"should be named"
	)
	
})
	
test_that("An error is generated if the name of the extra statistic is set to a default statistic", {
				
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	
	# create example of custom function (coefficient of variation)
	statsFctX <- function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100	
	
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "x", 
			statsExtra = list(statN = statsFctX)
		),
		"choose a different name"
	)
	
})
	
test_that("Extra statistic is correctly included when specified as a function of a vector", {
	
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	
	# create example of custom function (coefficient of variation)
	statsFctX <- function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100	
	
	statsExtraFctX <- computeSummaryStatistics(
		data = dataCont, 
		var = "x", 
		statsExtra = list(A = statsFctX)
	)
	expect_true("A" %in% names(statsExtraFctX))
	expect_equal(statsExtraFctX$A, statsFctX(dataCont$x))
	
})
	
test_that("Extra statistic is correctly included when specified as a function of the data", {
				
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	
	# create example of custom function (coefficient of variation)
	statsFctData <- function(data) sd(data$x, na.rm = TRUE)/mean(data$x, na.rm = TRUE)*100
	
	statsExtraFctData <- computeSummaryStatistics(
		data = dataCont, 
		var = "x", 
		statsExtra = list(B = statsFctData)
	)
	expect_true("B" %in% names(statsExtraFctData))
	expect_equal(statsExtraFctData$B, statsFctData(dataCont))
	
})

test_that("An error is generated if the type of table is continuous whereas the variable is categorical", {
		
	dataCat <- data.frame(
		x = c(NA_character_, "B", "B", "B", "A"), 
		USUBJID = seq.int(5)
	)
	expect_error(
		computeSummaryStatistics(
			data = dataCat, 
			var = "x", 
			type = "summaryTable"
		),
		"should be numeric"
	)
	
})

test_that("The type of table is correct for a categorical variable by default (or auto)", {
			
	dataCat <- data.frame(
		x = c(NA_character_, "B", "B", "B", "A"), 
		USUBJID = seq.int(5)
	)
	expect_identical(
		computeSummaryStatistics(data = dataCat, var = "x"),
		computeSummaryStatistics(data = dataCat, var = "x", type = "countTable")
	)
	expect_identical(
		computeSummaryStatistics(data = dataCat, var = "x", type = "auto"),
		computeSummaryStatistics(data = dataCat, var = "x", type = "countTable")
	)
	
})

test_that("The type of table is correct for a continuous variable by default (or auto)", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	expect_identical(
		computeSummaryStatistics(data = dataCont, var = "x"),
		computeSummaryStatistics(data = dataCont, var = "x", type = "summaryTable")
	)
	expect_identical(
		computeSummaryStatistics(data = dataCont, var = "x", type = "auto"),
		computeSummaryStatistics(data = dataCont, var = "x", type = "summaryTable")
	)
	
})

test_that("A count table is successfully extracted for a continuous variable", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "x", 
			type = "countTable"
		), 
		NA
	)
			
})

test_that("An error is generated if the variable is not specified for a continuous table", {
			
	dataCont <- data.frame(
		x = c(1, 3), 
		USUBJID = c("a", "b")
	)
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = NULL, 
			type = "summaryTable"
		),
		"Variable.*should be specified.*for a summary table"
	)
			
})

test_that("An error is generated if the table type is not count in case the variable is set to 'all'", {
			
	dataCont <- data.frame(
		x = c(1, 3), 
		USUBJID = c("a", "b")
	)
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "all", 
			type = "summaryTable"
		),
		"'type' should be set to 'countTable'"
	)
			
})

test_that("A continuous variable with only missing values is correctly filtered by default", {
	
	# variable is considered empty if all missing:
	emptyDataNA <- data.frame(
		x = rep(NA_real_, 5), 
		USUBJID = seq.int(5)
	)	
	expect_null(
		computeSummaryStatistics(data = emptyDataNA, var = "x")
	)
	
})

test_that("A continuous variable with only missing values is correctly included when requested", {
			
	# variable is considered empty if all missing:
	emptyDataNA <- data.frame(
		x = rep(NA_real_, 5), 
		USUBJID = seq.int(5)
	)	
	statsEmptyDataNA <- computeSummaryStatistics(
		data = emptyDataNA, 
		var = "x", 
		filterEmptyVar = FALSE
	)
	expect_equal(
		statsEmptyDataNA, 
		data.frame(
			statN = 0, statm = 0, statMean = NA_real_, 
			statSD = NA_real_, statSE = NA_real_, statMedian = NA_real_, 
			statMin = NA_real_, statMax = NA_real_
		)
	)

})	
	
test_that("An empty continuous variable is correctly included when requested", {

	emptyData <- data.frame(x = numeric(), USUBJID = character())
	statsEmptyData <- computeSummaryStatistics(
		data = emptyData, 
		var = "x", 
		filterEmptyVar = FALSE
	)
	expect_equal(
		statsEmptyData, 
		data.frame(
			statN = 0, statm = 0, statMean = NA_real_, 
			statSD = NA_real_, statSE = NA_real_, statMedian = NA_real_, 
			statMin = NA_real_, statMax = NA_real_
		)
	)
	
})	

test_that("Missing values are correctly filtered from a categorical variable with only missing values by default", {
			
	dataCat <- data.frame(
		x = factor(
			c(NA_character_, "B", "B", "B", "A"), 
			levels = c("A", "B", "C")
		),
		USUBJID = seq.int(5)
	)
	
	sumTable <- computeSummaryStatistics(
		data = dataCat, 
		var = "x", 
		filterEmptyVar = TRUE
	)
	expect_equal(sumTable$variableGroup, factor(c("A", "B"), levels = c("A", "B", "C")))
	
})

test_that("Missing values are correctly included from a categorical variable when requested", {

	dataCat <- data.frame(
		x = factor(
			c(NA_character_, "B", "B", "B", "A"), 
			levels = c("A", "B", "C")
		),
		USUBJID = seq.int(5)
	)
	statsCatNoFilter <- computeSummaryStatistics(data = dataCat, var = "x", filterEmptyVar = FALSE)
	expect_equal(nrow(statsCatNoFilter), 3)
	expect_true("C" %in% statsCatNoFilter$variableGroup)
	expect_equal(subset(statsCatNoFilter, variableGroup == "C")$statN, 0)
	expect_equal(subset(statsCatNoFilter, variableGroup == "C")$statm, 0)
	
})

test_that("An empty summary table is correctly returned by default if the input data is empty", {
		
	emptyData <- do.call(
		data.frame,
		list(
			x = factor(character(), levels = c("a", "b")),
			USUBJID = character()
		)
	)
	
	# by default, nothing is returned
	expect_equal(nrow(computeSummaryStatistics(data = emptyData, var = "x")), 0)
	
})	

test_that("A count table is correctly returned if the input data is empty when empty variable should be included", {
	
	# if variable should not be filtered, 0 counts are returned
	emptyData <- do.call(
		data.frame,
		list(
			x = factor(character(), levels = c("a", "b")),
			USUBJID = character()
		)
	)
	expect_equal(
		computeSummaryStatistics(data = emptyData, var = "x", filterEmptyVar = FALSE), 
		data.frame(variableGroup = c("a", "b"), statN = c(0, 0), statm = c(0, 0), stringsAsFactors = TRUE)
	)
	
})

test_that("A count table is correctly returned if the input data is empty when the empty variable and total of the variable are requested", {
			
	emptyData <- do.call(
		data.frame,
		list(
			x = factor(character(), levels = c("a", "b")),
			USUBJID = character()
		)
	)	
	expect_equal(
		computeSummaryStatistics(
			data = emptyData, 
			var = "x", 
			filterEmptyVar = FALSE, 
			varTotalInclude = TRUE
		), 
		data.frame(
			variableGroup = factor(
				c("a", "b", "Total"), 
				levels = c(c("a", "b", "Total"))
			),
			statN = c(0, 0, 0), 
			statm = c(0, 0, 0)
		)
	)
	
})
	
test_that("A count table is correctly returned if the input data is empty when total of the variable is requested", {
				
	emptyData <- do.call(
		data.frame,
		list(
			x = factor(character(), levels = c("a", "b")),
			USUBJID = character()
		)
	)	
	expect_equal(
		computeSummaryStatistics(
			data = emptyData, 
			var = "x", 
			varTotalInclude = TRUE
		), 
		data.frame(variableGroup = factor("Total", c("a", "b", "Total")),statN = 0, statm = 0)
	)
			
})

test_that("An error is generated if a continuous variable contains different values for the same subject", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		y = rep(2, 5),
		USUBJID = c("A", "B", "B", "C", "D")
	)
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x"),
		"multiple different records of x are available for the same USUBJID"
	)
	
})

test_that("An error is generated containing a specified variable if a continuous variable contains different values for the same subject", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		y = rep(2, 5),
		USUBJID = c("A", "B", "B", "C", "D")
	)
	
	expect_error(
		computeSummaryStatistics(data = dataCont, var = "x", msgVars = "y"),
		"multiple different records of x are available for the same USUBJID.* USUBJID y x"
	)
	
})

test_that("An error is generated containing a specified label if a continuous variable contains different values for the same subject", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = c("A", "B", "B", "C", "D")
	)
	
	expect_error(
		computeSummaryStatistics(
			data = dataCont, 
			var = "x", 
			msgLabel = "test dataset"
		),
		"multiple different records of x for the test dataset are available for the same USUBJID"
	)
			
})

test_that("A message is generated if a continuous variable contains duplicated values for the same subject", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 1, 6, 10), 
		USUBJID = c("A", "B", "B", "C", "D")
	)
			
	expect_message(
		computeSummaryStatistics(data = dataCont, var = "x"),
		"duplicated values for x"
	)
	
})

test_that("A warning is generated if some subject IDs are missing and a variable is specified", {

	data <- data.frame(USUBJID = c(NA_character_, 1), x = 1:2)
	expect_warning(
		summaryTable <- computeSummaryStatistics(data = data, var = "x"), 
		regexp = "Missing records.*are not considered for subject counts"
	)
			
})