context("Compute summary statistics table: statistics specification")

test_that("unique set of stats are computed by variable", {
		
	set.seed(123)
	data <- data.frame(AVAL = rnorm(10), CHG = c(NA_real_, rnorm(9)), USUBJID = seq.int(10))	
	
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			var = c("AVAL", "CHG"),
			data = data,
			stats = list(AVAL = expression(statMean), CHG = expression(statMedian))
		)
	)
	expect_identical(
		attr(summaryTable, "summaryTable")$statsVar,
		"Statistic"
	)

	expect_equal(subset(summaryTable, variable == "AVAL")$Statistic, mean(data$AVAL))
	expect_equal(subset(summaryTable, variable == "CHG")$Statistic, median(data$CHG, na.rm = TRUE))	
			
})

test_that("multiple sets of stats are computed by variable", {
		
	set.seed(123)
	data <- data.frame(AVAL = rnorm(10), CHG = c(NA_real_, rnorm(9)), USUBJID = seq.int(10))		
		
	# variable to summarize
	stats <- list(
		AVAL = list(n = expression(statN), `Mean` = expression(statMean)), 
		CHG = list(Median = expression(round(statMedian, 3)))
	)
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			var = c("AVAL", "CHG"),
			data = data,
			stats = stats
		)
	)
	expect_identical(
		attr(summaryTable, "summaryTable")$statsVar,
		c("n", "Median", "Mean"),
		label = "Order of statistics is as specified by variable"
	)
	sumTableAVAL <- subset(summaryTable, variable == "AVAL")[, c("n", "Median", "Mean")]
	expect_equal(unlist(sumTableAVAL), c(n = 10, Median = NA_real_, Mean = mean(data$AVAL)))
	
	sumTableCHG <- subset(summaryTable, variable == "CHG")[, c("n", "Median", "Mean")]
	expect_equal(unlist(sumTableCHG), c(n = NA_integer_, Median = round(median(data$CHG, na.rm = TRUE), 3), Mean = NA_real_))
	
})

test_that("incorrect specification of stats by variable is correctly flagged", {
		
	set.seed(123)
	data <- data.frame(AVAL = rnorm(10), CHG = c(NA_real_, rnorm(9)), USUBJID = seq.int(10))	
				
	# in this case, stats should be specified for all variables
	expect_error(
		computeSummaryStatisticsTable(
			var = c("AVAL", "CHG"),
			data = data,
			stats = list(AVAL = list(Mean = expression(statMean)))
		),
		"'stats'.*should be specified for all variables specified in 'var'"
	)
	
	# wrong variable is specified
	expect_error(
		computeSummaryStatisticsTable(
			var = c("AVAL", "CHG"),
			data = data,
			stats = list(AVAL1 = list(Mean = expression(statMean)))
		),
		"Statistics specified in 'stats' should be.*list named with variable.*"
	)
	
})

