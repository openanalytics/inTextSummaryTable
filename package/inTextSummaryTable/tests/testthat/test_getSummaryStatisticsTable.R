context("Get summary statistics table")

test_that("Get summary table is identical to computation+export step", {
		
	data <- data.frame(
		PARAM = c("a", "b", "a", "b"),
		TRT = c("A", "A", "B", "B"),
		AVAL = c(1, 2, 4, 5),
		USUBJID = seq.int(4)
	)		
	
	summaryTableOutput <- getSummaryStatisticsTable(
		data = data,	
		rowVar = "PARAM", colVar = "TRT",
		var = "AVAL"
	)
	
	summaryTable <- computeSummaryStatisticsTable(
		data = data,	
		rowVar = "PARAM", 
		colVar = "TRT",
		var = "AVAL"
	)
	summaryTableOutputSteps <- exportSummaryStatisticsTable(summaryTable)
			
	expect_identical(summaryTableOutput, summaryTableOutputSteps)
			
})

test_that("no output if data is empty", {

	expect_message(
		res <- getSummaryStatisticsTable(data = data.frame())
	)
	expect_null(res)
	
})
			
