context("Compute summary statistics table: statistics specification")

test_that("an unique statistic is specified as expression/name", {
		
	set.seed(123)
	data <- data.frame(AVAL = rnorm(5), USUBJID = seq.int(5))
		
	# as an expression
	expect_silent(
		summaryTableExpr <- computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = expression(statMean)
		)
	)
	expect_identical(attr(summaryTableExpr, "summaryTable")$statsVar, "Statistic")
	expect_equal(subset(summaryTableExpr, !isTotal)$Statistic, mean(data$AVAL))
	
	# as a 'name' object
	expect_silent(
		summaryTableName <- computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = bquote(statMean)
		)
	)
	expect_identical(summaryTableExpr, summaryTableName)
		
})

test_that("statistic is specified as a string among default set", {
			
	set.seed(123)
	data <- data.frame(AVAL = rnorm(5), USUBJID = seq.int(5))
		
	expect_silent(
		summaryTableString <- computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = "default"
		)
	)
	
	# specification equivalent to:
	stats <- getStatsData("summary-default", data = data, var = "AVAL")$AVAL
	summaryTableGetStats <- computeSummaryStatisticsTable(
		var = "AVAL",
		data = data,
		stats = stats
	)
	expect_identical(summaryTableString, summaryTableGetStats)
	
	
	# statistic not available in the default set:
	expect_error(
		computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = "myStat"
		),
		"should be one of .*summary.*, .*count.*"
	)
			
})

test_that("stats is a copy of default statistic", {
			
	set.seed(123)
	data <- data.frame(AVAL = rnorm(5),	USUBJID = seq.int(5))	
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			data = data, var = "AVAL", 
			stats = list(statMean = expression(statMean))
		)
	)
	expect_true("statMean" %in% colnames(summaryTable))
	expect_equal(
		subset(summaryTable, !isTotal)$statMean,
		mean(data$AVAL)
	)
			
})

test_that("stats has same name than default statistic", {
			
	data <- data.frame(AVAL = rnorm(5),	USUBJID = seq.int(5))	
	expect_error(
		computeSummaryStatisticsTable(
			data = data, var = "AVAL", 
			stats = list(statMean = expression(statMean+statSD))
		),
		"statistic name.*is a default name used"
	)
			
})

test_that("unique set of stats are computed by variable", {
		
	set.seed(123)
	data <- data.frame(
		AVAL = rnorm(10), 
		CHG = c(NA_real_, rnorm(9)), 
		USUBJID = seq.int(10)
	)	
	
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
	data <- data.frame(
		AVAL = rnorm(10), 
		CHG = c(NA_real_, rnorm(9)), 
		USUBJID = seq.int(10)
	)		
		
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
	data <- data.frame(
		AVAL = rnorm(10), 
		CHG = c(NA_real_, rnorm(9)), 
		USUBJID = seq.int(10)
	)	
				
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

test_that("stats are computed by a specified row/column variable", {
			
	set.seed(123)
	data <- data.frame(
		PARAM = rep(c("ALB", "ALT"), length.out = 10),
		AVAL = rnorm(10), 
		USUBJID = seq.int(10)
	)
	
	# variable to summarize
	stats <- list(
		ALB = list(Mean = expression(statMean)), 
		ALT = list(Median = expression(statMedian))
	)
	
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = stats,
			rowVar = "PARAM", statsVarBy = "PARAM"
		)
	)
	
	sumTableALT <- subset(summaryTable, PARAM == "ALT")[, c("Median", "Mean")]
	dataALT <- subset(data, PARAM == "ALT")
	expect_equal(
		unlist(sumTableALT), 
		c(Median = median(dataALT$AVAL), Mean = NA_real_)
	)
	
	sumTableALB <- subset(summaryTable, PARAM == "ALB")[, c("Median", "Mean")]
	dataALB <- subset(data, PARAM == "ALB")
	expect_equal(
		unlist(sumTableALB), 
		c(Median = NA_real_, Mean = mean(dataALB$AVAL))
	)
	
	# error if 'statsVarBy' is not specified in row/column variable
	expect_error(
		computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = stats,
			statsVarBy = "PARAM"
		),
		".*not available in.*row or column variables.*"
	)
	
	# error is variable to compute stats by is not specified
	expect_error(
		computeSummaryStatisticsTable(
			var = "AVAL",
			data = data,
			stats = stats
		),
		"Statistics.*should be.*list named with.*statsVarBy elements.*"
	)
	
})

test_that("stats are computed by a specified row/column variable and by variable", {
	
	set.seed(123)
	data <- data.frame(
		PARAM = rep(c("ALB", "ALT"), length.out = 10),
		AVAL = rnorm(10), 
		CHG = rnorm(10),
		USUBJID = seq.int(10)
	)
	
	# variable to summarize
	stats <- list(
		AVAL = list(ALB = list(Mean = expression(statMean))), 
		CHG = list(ALT = list(Median = expression(statMedian)))
	)
	
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			var = c("AVAL", "CHG"),
			data = data,
			stats = stats,
			rowVar = "PARAM", statsVarBy = "PARAM"
		)
	)
	
	expect_equal(
		unlist(subset(summaryTable, PARAM == "ALB" & variable == "AVAL", c("Mean", "Median"))), 
		c(Mean = mean(subset(data, PARAM == "ALB")$AVAL), Median = NA_real_)
	)
	expect_identical(
		unlist(subset(summaryTable, PARAM == "ALT" & variable == "AVAL", c("Mean", "Median"))), 
		c(Mean = NA_real_, Median = NA_real_)
	)
	expect_identical(
		unlist(subset(summaryTable, PARAM == "ALB" & variable == "CHG", c("Mean", "Median"))), 
		c(Mean = NA_real_, Median = NA_real_)
	)
	expect_identical(
		unlist(subset(summaryTable, PARAM == "ALT" & variable == "CHG", c("Mean", "Median"))), 
		c(Mean = NA_real_, Median = median(subset(data, PARAM == "ALT")$CHG))
	)
				
})
			

test_that("custom statistics are specified", {
			
	set.seed(123)
	data <- data.frame(
		AVAL = rnorm(10), 
		USUBJID = seq.int(10)
	)
	
	CV <- function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100
	
	summaryTable <- computeSummaryStatisticsTable(
		var = "AVAL",
		data = data,
		stats = list(CV = bquote(statCV)),
		statsExtra = list(statCV = CV)
	)
	expect_equal(subset(summaryTable, !isTotal)$CV, CV(data$AVAL))
	
	# format of statsExtra already checked in unit tests
	# for the 'computeSummaryStatistics' function
			
})

test_that("general label for statistics is specified", {

	set.seed(123)
	data <- data.frame(USUBJID = seq.int(10))	
	
	statLab <- "My custom statistic"
	# general label is stored in table attribute
	# (and set when the table is formatted)
	expect_identical(
		attr(
			computeSummaryStatisticsTable(data = data, statsGeneralLab = statLab), 
			"summaryTable"
		)$rowVarLab,
		c(Statistic = statLab)
	)
			
})

test_that("percentage is computed on a different statistic", {
			
	set.seed(123)
	data <- data.frame(
		USUBJID = c("1", "1", "2", "3", "4"),
		AEDECOD = rep("A", 5)
	)
	
	# compute percentage based on the number of subjects:
	expect_silent(
		summaryTableStatN <- computeSummaryStatisticsTable(
			var = "AEDECOD",
			data = data,
			statsPerc = "statN"
		)
	)
	summaryTableStatNGroup <- subset(summaryTableStatN, variableGroup == "A")
	expect_equal(summaryTableStatNGroup$statPercTotalN, 4)
	
	# compute percentage based on the number of records:
	expect_silent(
		summaryTableStatm <- computeSummaryStatisticsTable(
			var = "AEDECOD",
			data = data,
			statsPerc = "statm"
		)
	)
	summaryTableStatmGroup <- subset(summaryTableStatm , variableGroup == "A")
	expect_equal(summaryTableStatmGroup$statPercTotalm, 5)
	
	# statistic not available:
	expect_error(
		computeSummaryStatisticsTable(
			var = "AEDECOD",
			data = data,
			statsPerc = "statTest"
		)
	)
			
})