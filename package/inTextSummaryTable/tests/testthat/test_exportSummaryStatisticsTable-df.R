context("Export summary statistics table as data frame")

test_that("summary table is exported to multiple format", {
			
	summaryTable <- data.frame(
		PARAM = c("A", "B"),
		n = c(9, 10)
	)	
	
	fileTable <- "table.txt"
	if(file.exists(fileTable))
		tmp <- file.remove(fileTable)
	
	# by default, if a text file is specified,
	# format is set to 'data.frame-base'
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = fileTable
	)
	
	expect_true(file.exists(fileTable))
	
	fileCnt <- read.table(fileTable, header = TRUE, sep = "\t")
	expect_equal(fileCnt, summaryTable)
	
	# format specified explicitly
	tmp <- file.remove(fileTable)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = c(`data.frame-base` = fileTable)
	)
	expect_true(file.exists(fileTable))
	
	fileCntExplicit <- read.table(fileTable, header = TRUE, sep = "\t")
	expect_equal(fileCntExplicit, fileCnt)
	
})

test_that("summary table in long format is exported to a text file", {
			
	summaryTable <- data.frame(
		PARAM = c("A", "B"),
		n = c(9, 10)
	)	
	
	fileTable <- "table.txt"
	if(file.exists(fileTable))
		tmp <- file.remove(fileTable)
	
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