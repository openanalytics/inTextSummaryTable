context("Compute summary statistics table")

library(plyr)

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