context("Get a summary statistics table")

test_that("Getting a summary table is identical to computing and exporting a summary table", {
		
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

test_that("No output and a message is generated if the data is empty", {

	expect_message(
		res <- getSummaryStatisticsTable(data = data.frame())
	)
	expect_null(res)
	
})

test_that("A variable with subgroups and a flag variable are correctly combined when multiple statistics are set", {
			
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

test_that("The width of the columns is correctly set", {
			
	data <- data.frame(
		TRT = c("A", "B"),
		USUBJID = c("1", "2"),
		stringsAsFactors = FALSE
	)
	columnsWidth <- c(0.5, 2, 8)
	ft <- getSummaryStatisticsTable(data = data, colVar = "TRT", columnsWidth = columnsWidth)
			
	expect_equal(object = ft$body$colwidths, expected = columnsWidth)
			
})

test_that("A warning is generated if the width of the columns is not correctly set", {
			
	data <- data.frame(
		TRT = c("A", "B"),
		USUBJID = c("1", "2"),
		stringsAsFactors = FALSE
	)
	columnsWidth <- c(2, 8)
	expect_warning(
		object = getSummaryStatisticsTable(
			data = data, 
			colVar = "TRT", 
			columnsWidth = columnsWidth
		)
	)

})
			
