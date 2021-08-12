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
	summaryTableOutputSteps <- export(summaryTable)
			
	expect_identical(summaryTableOutput, summaryTableOutputSteps)
			
})

test_that("no output if data is empty", {

	expect_message(
		res <- getSummaryStatisticsTable(data = data.frame())
	)
	expect_null(res)
	
})

test_that("a variable with subgroups and a flag variable are correctly combined when multiple statistics are set", {
			
	data <- data.frame(
		TRTEMFL = c("Y", "Y", "N", "N"),
		WORST = factor(
			c("Mild", "Moderate", "Mild", "Mild"),
			levels = c("Mild", "Moderate"),
		),
		TRT = c("A", "A", "A", "A"),
		USUBJID = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	ft <- getSummaryStatisticsTable(
		data = data,
		var = c("TRTEMFL", "WORST"),
		varFlag = "TRTEMFL",
		colVar = "TRT",
		stats = getStats(c("n", "%"))
	)
			
	dataRef <- 	data.frame(
		col1 = c(
			"TRTEMFL", "n", "%",
			"WORST", 
			"Mild", "n", "%",
			"Moderate", "n", "%"
		),
		col2 = c(
			NA_character_, "2", "50.0", 
			NA_character_, 
			NA_character_, "3", "75.0", 
			NA_character_, "1", "25.0"
		),
		stringsAsFactors = FALSE
	)
			
	expect_equal(
		object = ft$body$data, 
		expected = dataRef, 
		# rownames might differ
		check.attributes = FALSE
	)
			
})
			
