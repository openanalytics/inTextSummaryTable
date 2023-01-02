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

test_that("A list of summary tables is successfully exported to text files in base format", {
  
  summaryTables <- list(
    A = data.frame(PARAM = "A", n = 10),
    B = data.frame(PARAM = "B", n = 9)
  )
  
  file <- tempfile(pattern = "table", fileext = ".txt")
  ft <- exportSummaryStatisticsTable(
    summaryTable = summaryTables,
    rowVar = "PARAM", statsVar = "n",
    file = c(`data.frame-base` = file)
  )
  for(i in seq_along(summaryTables)){
    expect_equal(
      object = read.table(
        file = paste0(
          tools::file_path_sans_ext(file), 
          "_", !!i, ".txt"
        ),
        header = TRUE, sep = "\t"
      ),
      expected = summaryTables[[!!i]]
    )
  }
  
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

test_that("A list of summary tables is successfully exported to text files in in-text format", {
  
  summaryTables <- list(
    A = data.frame(PARAM = "A", n = 10),
    B = data.frame(PARAM = "B", n = 9)
  )
  
  file <- tempfile(pattern = "table", fileext = ".txt")
  ft <- exportSummaryStatisticsTable(
    summaryTable = summaryTables,
    rowVar = "PARAM", statsVar = "n",
    file = c('data.frame' = file)
  )
  
  for(i in seq_along(summaryTables)){
    expect_equal(
      object = readLines(
        con = paste0(
          tools::file_path_sans_ext(file), 
          "_", !!i, ".txt"
        )
      ),
      expected = c(
        "PARAM\tn", 
        "(N=NA)", 
        paste(summaryTables[[!!i]], collapse = "\t")
      )
    )
  }
  
})
