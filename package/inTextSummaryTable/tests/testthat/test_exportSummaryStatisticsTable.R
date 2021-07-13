context("Export summary statistics table")

library(tools)
library(officer)
library(rmarkdown)

test_that("summary table is exported to multiple formats", {
			
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
	)
			
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
			html = gsub("htmlwidget-\\w+", "\\1", readLines(file))
		)
	}
	
	# check that output is similar as output for single export
	for(outputType in names(files)){
		
		expect_equal(
			object = {
				importTableFromFile(file = files[[!!outputType]])
			}, 	
			expected = {
				fileTest <- sapply(files[!!outputType], function(x)
					sub("(.+)\\.(.+)", "\\1-test.\\2", x)
				)
				res <- exportSummaryStatisticsTable(
					summaryTable = summaryTable,
					rowVar = "PARAM", statsVar = "n",
					file = fileTest
				)
				importTableFromFile(fileTest)
			},
			check.attributes = FALSE
		)
		
	}
	
})
			
