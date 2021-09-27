context("Export summary statistics table as data frame")

test_that("The summary table is correctly exported to a text file in base format by default", {
			
	summaryTable <- data.frame(PARAM = c("A", "B"), n = c(9, 10))	
	
	# by default, if a text file is specified,
	# format is set to 'data.frame-base'
	fileTable <- tempfile(pattern = "table", fileext = ".txt")
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = fileTable
	)
	
	expect_true(file.exists(fileTable))
	fileCnt <- read.table(fileTable, header = TRUE, sep = "\t")
	expect_equal(fileCnt, summaryTable)
	
})

test_that("The summary table is correctly exported to a text file in base format as specified", {
			
	summaryTable <- data.frame(PARAM = c("A", "B"), n = c(9, 10))			
			
	fileTable <- tempfile(pattern = "table", fileext = ".txt")
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = c(`data.frame-base` = fileTable)
	)
	expect_true(file.exists(fileTable))
	fileCnt <- read.table(fileTable, header = TRUE, sep = "\t")
	expect_equal(fileCnt, summaryTable)
	
})

test_that("The summary table is correctly exported to a text file in in-text format as specified", {
			
	summaryTable <- data.frame(PARAM = c("A", "B"),n = c(9, 10))	
	
	fileTable <- tempfile(pattern = "table", fileext = ".txt")
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = c('data.frame' = fileTable)
	)
	
	expect_true(file.exists(fileTable))
	fileCnt <- readLines(fileTable)
	expect_equal(
		fileCnt, 
		c("PARAM\tn", "(N=NA)", "A\t9", "B\t10")
	)
	
})