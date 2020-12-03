context("Compute summary statistics table: total specification")

test_that("general total is extracted by default from data", {
			
	# data with different number of subjects/records per treatment
	data <- data.frame(
		USUBJID = c(1, 1, 2, 3, 4, 4, 5),
		TRT = rep(c("A", "B"), times = c(4, 3)),
		ADECOD = c("a", "b", "a", "c", "d", "a", "b"),
		stringsAsFactors = FALSE
	)
	
	sumTable <- computeSummaryStatisticsTable(
		data = data,
		var = "ADECOD",
		colVar = "TRT"
	)
	# by default data total extracted from the dataset directly
	sumTableTotal <- subset(sumTable, isTotal)
	sumTableTotal <- sumTableTotal[match(c("A", "B"), sumTableTotal$TRT), ]
	
	# # subjects/records for total is correct
	expect_equal(sumTableTotal$statN, c(3, 2))
	expect_equal(sumTableTotal$statm, c(4, 3))
	
	# number of subjects in 'total' same as total used for percentage by default
	expect_equal(
		sumTable$statPercTotalN, 
		sumTableTotal[match(sumTable$TRT, sumTableTotal$TRT), "statN"]
	)
			
})

test_that("general total is is extracted from specified dataset", {
			
	# data with different number of subjects/records per treatment
	data <- data.frame(
		USUBJID = c(1, 1, 2, 3, 4, 4, 5),
		TRT = rep(c("A", "B"), times = c(4, 3)),
		ADECOD = c("a", "b", "a", "c", "d", "a", "b"),
		stringsAsFactors = FALSE
	)
	
	## correct spec
	
	# data with different number of subjects/records
	dataTotal <- data.frame(
		TRT = rep(c("A", "B"), c(6, 4)),
		USUBJID = c(c(1, seq.int(5)), c(1, seq.int(3)))
	)
			
	sumTable <- computeSummaryStatisticsTable(
		data = data,
		var = "ADECOD",
		colVar = "TRT",
		dataTotal = dataTotal
	)
			
	# data total extracted from the specified dataset directly
	sumTableTotal <- subset(sumTable, isTotal)
	sumTableTotal <- sumTableTotal[match(c("A", "B"), sumTableTotal$TRT), ]
			
	# # subjects/records for total is correct
	expect_equal(sumTableTotal$statN, c(5, 3))
	expect_equal(sumTableTotal$statm, c(6, 4))
			
	# number of subjects in 'total' same as total used for percentage by default
	expect_equal(
		sumTable$statPercTotalN, 
		sumTableTotal[match(sumTable$TRT, sumTableTotal$TRT), "statN"]
	)
	
	## wrong spec
	dataTotal <- data.frame(
		USUBJID = c(c(1, seq.int(5)), c(1, seq.int(3)))
	)
	w <- capture_warnings(
		sumTable <- computeSummaryStatisticsTable(
			data = data,
			var = "ADECOD",
			colVar = "TRT",
			dataTotal = dataTotal
		)
	)
	expect_match(w, ".*colVarTotal are ignored because they are not available in: total dataset.", all = FALSE)
	expect_match(w, ".*colVarTotalPerc are ignored because they are not available in: total dataset for percentage.", all = FALSE)
	expect_equal(sum(sumTable$isTotal), 1) # total computed across columns
	
})

test_that("percentage is extracted from specified dataset", {

	# data with different number of subjects/records per treatment
	data <- data.frame(
		USUBJID = c(1, 1, 2, 3, 4, 4, 5),
		TRT = rep(c("A", "B"), times = c(4, 3)),
		ADECOD = c("a", "b", "a", "c", "d", "a", "b"),
		stringsAsFactors = FALSE
	)	
	
	dataTotalPerc <- data.frame(
		TRT = rep(c("A", "B"), c(6, 4)),
		USUBJID = c(c(1, seq.int(5)), c(1, seq.int(3)))
	)
	
	## correct spec
	
	sumTable <- computeSummaryStatisticsTable(
		data = data,
		var = "ADECOD",
		colVar = "TRT",
		dataTotalPerc = dataTotalPerc
	)
	
	# percentage is computed from the specified dataset
	expect_true(all(subset(sumTable, TRT == "A")$statPercTotalN == 5))
	expect_true(all(subset(sumTable, TRT == "B")$statPercTotalN == 3))
	
	expect_equal(sumTable$statPercN, with(sumTable, statN/statPercTotalN)*100)
	
	# data total is still extracted from the dataset directly
	sumTableTotal <- subset(sumTable, isTotal)
	sumTableTotal <- sumTableTotal[match(c("A", "B"), sumTableTotal$TRT), ]
	
	# # subjects/records for total is correct
	expect_equal(sumTableTotal$statN, c(3, 2))
	expect_equal(sumTableTotal$statm, c(4, 3))
	
	## wrong spec
	dataTotalPerc <- data.frame(
		USUBJID = c(c(1, seq.int(5)), c(1, seq.int(3)))
	)
	expect_warning(
		sumTable <- computeSummaryStatisticsTable(
			data = data,
			var = "ADECOD",
			colVar = "TRT",
			dataTotalPerc = dataTotalPerc
		),
		".*colVarTotalPerc are ignored because they are not available in: total dataset for percentage."
	)
})

test_that("a different dataset for the row total is specified", {
			
	# data with different number of subjects/records per treatment
	data <- data.frame(
		USUBJID = c(1, 1, 2, 3, 4, 4, 5),
		TRT = rep(c("A", "B"), times = c(4, 3)),
		ABODSYS = c("AB", "AB", "AB", "CD", "CD", "AB", "AB"),
		ADECOD = c("a", "b", "a", "c", "d", "a", "b"),
		stringsAsFactors = FALSE
	)
	
	## TODO: correct spec
	
	## incorrect spec
	
	# variables (columns, rows) missing in dataTotalRow
	expect_error(
		computeSummaryStatisticsTable(
			data = data,
			colVar = "TRT",
			rowVar = c("ABODSYS", "ADECOD"),
			rowVarTotalInclude = c("ABODSYS", "ADECOD"),
			dataTotalRow = data[, "USUBJID", drop = FALSE]
		),
		"*not available.*dataset for row total"
	)
	
	# dataset specified as a list but not for all variables
	expect_error(
		computeSummaryStatisticsTable(
			data = data,
			colVar = "TRT",
			rowVar = c("ABODSYS", "ADECOD"),
			rowVarTotalInclude = c("ABODSYS", "ADECOD"),
			dataTotalRow = list(ABODSYS = data)
		),
		"Dataset for row total missing for:.*ADECOD.*"
	)
	
})