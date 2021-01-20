context("Export summary statistics table")

library(tools)
library(officer)

test_that("summary table is exported to multiple format", {
			
	summaryTable <- data.frame(
		PARAM = c("A", "B"),
		n = c(9, 10)
	)	
	
	files <- c(
		`data.frame-base` = "table-base.txt",
		`data.frame` = "table.txt",
		`flextable` = "table.docx",
		`DT` = "table.html"
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM", statsVar = "n",
		file = files
	)
	
	importTableFromFile <- function(file){
		switch(tools::file_ext(file),
			txt = readLines(file),
			docx = officer::docx_summary(officer::read_docx(file)),
			html = readLines(file)
		)
	}
	
	# check that output is similar as output for single export
	for(outputType in names(files)){
		
		expect_equal(
			object = {
				importTableFromFile(files[[!!outputType]])
			}, 	
			expected = {
				fileTest <- paste0("test", ".", tools::file_ext(files[[!!outputType]]))
				res <- exportSummaryStatisticsTable(
					summaryTable = summaryTable,
					rowVar = "PARAM", statsVar = "n",
					outputType = !!outputType,
					file = fileTest
				)
				importTableFromFile(fileTest)
			},
			check.attributes = FALSE
		)
		
	}
	
})
			
