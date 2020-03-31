context("Test 'computeSummaryStatisticsTable' error/warning tracking")

dataLB <- ADaMDataPelican$ADLB

## tests different error/warning tracking mechanisms

# in case of a summaryTable, 'var' should be specified ...
expect_error(
	computeSummaryStatistics(
		var = NULL,
		data = subset(dataLB, PARAMCD == "ALB"),
		type = "summaryTable"
	)	
)
# ... and numeric
expect_error(
	computeSummaryStatistics(
		var = "PARAM",
		data = subset(dataLB, PARAMCD == "ALB"),
		type = "summaryTable"
	)
)
