context("Compute summary statistics table")

test_that("data is not a data.frame", {
			
	# e.g. if we forget to create the 'data' object, 'data()' function is used instead:
	expect_error(
		computeSummaryStatisticsTable(data),
		"A data.frame should be specified in.*data.*."
	)
	
})

test_that("no data to report", {
			
	data <- data.frame()
	expect_message(
		res <- computeSummaryStatisticsTable(data),
		"No data to report."
	)
	expect_null(res)
	
})

test_that("only data specified", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
	
	# no variable specified: a count table is created
	sumNoVar <- computeSummaryStatisticsTable(dataCont)
	expect_s3_class(sumNoVar, "data.frame")
	expect_named(sumNoVar, c("isTotal", "statN", "statm", "statPercTotalN", "statPercN"), ignore.order = TRUE)
	expect_equal(
		unname(unlist(subset(sumNoVar, isTotal)[1, c("statN", "statm", "statPercTotalN", "statPercN")])),
		c(5, 5, 5, 100)
	)
	expect_equal(
		subset(sumNoVar, isTotal, select = -isTotal), 
		subset(sumNoVar, !isTotal, select = -isTotal), 
		check.attributes = FALSE
	)
	
})

test_that("subject ID variable is specified", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10),
		`subject identifier` = seq.int(5), 
		check.names = FALSE
	)
	
	expect_error(
		computeSummaryStatisticsTable(dataCont),
		"Subject variable.*not available for the computation of the number of subjects."
	)
	
	expect_silent(sumTable <- 
		computeSummaryStatisticsTable(dataCont, subjectVar = "subject identifier")			
	)
	expect_equal(sumTable$statN, c(5, 5))
	expect_equal(sumTable$statPercTotalN, c(5, 5))
	expect_equal(sumTable$statPercN, c(100, 100))
			
})

test_that("the summary table is filtered", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AEDECOD = rep(c("A", "B"), length.out = 5)
	)

	summaryTableFiltered <- computeSummaryStatisticsTable(
		data = data, var = "AEDECOD",
		filterFct = function(x) subset(x, variableGroup == "A")
	)
	expect_equal(
		summaryTableFiltered,
		subset(computeSummaryStatisticsTable(data = data, var = "AEDECOD"), variableGroup == "A"),
		check.attributes = FALSE
	)			
	
	# wrong filtering fct:
	expect_error(
		computeSummaryStatisticsTable(
			data = data, var = "AEDECOD",
			filterFct = function(x) subset(x, variableGroup2 == "A")
		)
	)
			
})

test_that("the summary table is filtered and a flag variable is specified", {
	
	# internally, a filterFct is set when varFlag is specified
	# this test checks that if the user specifies additionally 
	# a filterFct, this one is also used.
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AEDECOD = c("A", "A", "B", "B", "B"),
		ANFL = c("Y", "N", "Y", "N", "Y")
	)
	
	summaryTableAll <- computeSummaryStatisticsTable(
		data = data, 
		var = "ANFL", varFlag = "ANFL",
		rowVar = "AEDECOD",
		stats = "n"
	)
			
	summaryTableFiltered <- computeSummaryStatisticsTable(
		data = data, 
		var = "ANFL", varFlag = "ANFL",
		rowVar = "AEDECOD",
		stats = "n",
		filterFct = function(x) subset(x, AEDECOD == "A")
	)
	
	expect_equal(
		summaryTableFiltered,
		subset(summaryTableAll, AEDECOD == "A"),
		check.attributes = FALSE
	)
			
})