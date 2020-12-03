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

test_that("a different dataset for the general total is specified", {
			
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