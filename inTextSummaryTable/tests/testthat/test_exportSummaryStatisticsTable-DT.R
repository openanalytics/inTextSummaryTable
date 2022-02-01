context("Export summary statistics table to a DataTables")

library(htmltools)
library(rmarkdown)
library(tools)

test_that("A summary table with a row variable is correctly exported to DataTables", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)	
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM",
		outputType = "DT"
	)
	expect_s3_class(dt, "datatables")
	expect_identical(
		as.character(dt$x$data[, 1]),
		c("B", "A")
	)
			
})

test_that("A warning is generated if a summary table is exported to DataTables with multiple nested row variables", {
			
	summaryTable <- data.frame(
		TRT = rep(c("A", "A", "B", "B"), times = 2),
		PARAM = rep(c("a", "b", "a", "b"), times = 2),
		metric = rep(c("Actual Value", "Change from Baseline"), each = 4),
		n = seq_len(8)
	)
	
	expect_warning(
		exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = c("TRT", "PARAM", "metric"),
			outputType = "DT"
		),
		"multi-level row grouping row variable is not available"
	)
	
})


test_that("The specified labels of the row variables are correctly set in a DataTables summary table", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM",
		rowVarLab = c(PARAM = "Parameter"),
		outputType = "DT"
	)
	expect_identical(
		object = attr(dt$x, "colnames")[1], 
		expected = "Parameter"
	)
			
})

test_that("The labels of the row variables, extracted from the labels of all variables, are correctly set in a DataTables summary table", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM",
		labelVars = c(PARAM = "Parameter"),
		outputType = "DT"
	)
	expect_identical(
		object = attr(dt$x, "colnames")[1], 
		expected = "Parameter"
	)
	
})


test_that("A summary table with a row variable in a separated column is correctly exported to DataTables", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("TRT", "PARAM"),
		rowVarInSepCol = "PARAM",
		outputType = "DT"
	)
	expect_identical(
		object = attr(dt$x, "colnames")[1:2], 
		expected = c("TRT", "PARAM")
	)
	
})

test_that("A summary table with a column variable is correctly exported to DataTables", {
			
	summaryTable <- data.frame(
		TRT = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		outputType = "DT"
	)
	expect_s3_class(dt, "datatables")
	expect_identical(
		colnames(dt$x$data),
		c("B", "A")
	)
	
})

test_that("A summary table with totals in the header is correctly exported to DataTables", {
	
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		outputType = "DT",
		colHeaderTotalInclude = TRUE
	)
	expect_identical(
		object = colnames(dt$x$data),
		expected = c("B\n(N=5)", "A\n(N=4)")
	)
	
})
	
test_that("A summary table without totals in the header is correctly exported to DataTables", {
		
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		outputType = "DT",
		colHeaderTotalInclude = FALSE
	)
	expect_identical(
		object = colnames(dt$x$data),
		expected = c("B", "A")
	)

})

test_that("A summary table without column total is correctly exported to DataTables without totals in header", {
			
	# check that no records are filtered
	summaryTable <- data.frame(
		TRT = factor(c("A", "B"), levels = c("B", "A")),
		statN = c(1, 4),
		isTotal = c(FALSE, FALSE)
	)	

	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		outputType = "DT",
		colHeaderTotalInclude = FALSE
	)
	expect_equal(
		object = dt$x$data, 
		expected = data.frame(B = 4, A = 1), 
		check.attributes = FALSE
	)
			
})

test_that("A summary table with statistics in a separated column is correctly exported to DataTables", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
	
	dtStatRowInSepCol <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "rowInSepCol",
		outputType = "DT"
	)

	dataRefStatRowInSepCol <- data.frame(
		variable = c("A", "A", "B", "B"),
		Statistic = c("n", "Mean", "n", "Mean"),
		`StatisticValue\n(N=NA)` = c("1", "0.34", "2", "0.56"),
		check.names = FALSE, stringsAsFactors = FALSE
	)
	# class of single column differ
	dataDTStatRowInSepCol <- as.data.frame(sapply(dtStatRowInSepCol$x$data, as.character), stringsAsFactors = FALSE)
	expect_identical(dataDTStatRowInSepCol, dataRefStatRowInSepCol)
	
})

test_that("A summary table with statistics in rows is correctly exported to DataTables", {
	
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
			
	dtStatInRow <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "row",
		outputType = "DT"
	)

	# class of single column differ
	dataDTStatInRow <- as.data.frame(sapply(dtStatInRow$x$data, as.character), stringsAsFactors = FALSE)
	# if columns are nested, the internal dataset represents them as separated column:
	dataRefStatRowInSepCol <- data.frame(
		variable = c("A", "A", "B", "B"),
		Statistic = c("n", "Mean", "n", "Mean"),
		`StatisticValue\n(N=NA)` = c("1", "0.34", "2", "0.56"),
		check.names = FALSE, stringsAsFactors = FALSE
	)
	expect_identical(dataDTStatInRow, dataRefStatRowInSepCol)
	# but these are specified via the 'rowGroup' option
	# (in JS column index which starts at 0)
	expect_identical(dtStatInRow$x$options$rowGroup$dataSrc, 0)
	
})

test_that("A summary table with statistics in columns is correctly exported to DataTables", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)

	dtStatInCol <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "col",
		outputType = "DT"
	)		
	dataRefStatInCol <-data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
	# class of single column differ
	dataDTStatInCol <- as.data.frame(sapply(dtStatInCol$x$data, as.character), stringsAsFactors = FALSE)
	expect_identical(object = dataDTStatInCol, expected = dataRefStatInCol)
	
})

test_that("The label for the statistic value is correctly set in a DataTables summary table", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable, rowVar = "variable", 
		statsVar = "Statistic", statsValueLab = "Number of subjects",
		outputType = "DT"
	)
	expect_match(
		object = colnames(dt$x$data)[2],
		regexp = "Number of subjects.*" 
	)
	
})

test_that("An error is generated if the label for the statistic value is set to the default name in a DataTables summary table", {
	
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)		
	
	# error is label is set to 'Statistic' (used as default naming)
	expect_error(
		exportSummaryStatisticsTable(
			summaryTable, rowVar = "variable", 
			statsVar = "Statistic", statsValueLab = "Statistic",
			outputType = "DT"
		),
		"'statsValueLab' should be different than 'Statistic'."
	)
			
})

test_that("A summary table with one statistic is correctly exported to DataTables with the statistic name", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "variable", colVar = "TRT",
		statsVar = "n", 
		statsLabInclude = TRUE,
		outputType = "DT"
	)
	expect_identical(
		object = attr(dt$x, "colnames")[2:3],
		expected = c("A_n", "B_n")
	)
	
})

test_that("A summary table with one statistic is correctly exported to DataTables without the statistic name", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "variable", colVar = "TRT",
		statsVar = "n", 
		statsLabInclude = FALSE,
		outputType = "DT"
	)
	expect_identical(
		object = attr(dt$x, "colnames")[2:3],
		expected = c("A", "B")
	)
	
})

test_that("A warning is generated if a DataTables summary table contain multiple statistics but the names are specified to be not included", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		Mean = c("0.1", "1.3", "4.5", "6.7"),
		stringsAsFactors = FALSE
	)
	expect_warning(
		exportSummaryStatisticsTable(
			summaryTable, 
			rowVar = "variable", colVar = "TRT",
			statsVar = c("n", "Mean"),
			statsLabInclude = FALSE,
			outputType = "DT"
		),
		"Statistic label is included.*because more than one statistic variable.*"
	)
	
})

test_that("A placeholder for empty value is correctly included by default in a DataTables summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = c("1", "2", "3", NA_character_),
		stringsAsFactors = FALSE
	)
	
	# by default, 'empty' value are set to '-'
	# Note that 'numeric-like' column to numeric 
	dt <- exportSummaryStatisticsTable(
		summaryTable,
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "n",
		outputType = "DT"
	)
	expect_identical(
		object = dt$x$data,
		expected = data.frame(
			PARAM = c("a", "b"),
			A = c(1, 2),
			B = c("3", "-"),
			stringsAsFactors = FALSE
		)
	)
	
})

test_that("A specified placeholder for empty value is correctly included in a DataTables summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = c("1", "2", "3", NA_character_),
		stringsAsFactors = FALSE
	)
		
	dt <- exportSummaryStatisticsTable(
		summaryTable,
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "n",
		emptyValue = "0",
		outputType = "DT"
	)
	expect_identical(
		object = dt$x$data,
		expected = data.frame(
			PARAM = c("a", "b"),
			A = c(1, 2),
			B = c(3, 0),
			stringsAsFactors = FALSE
		)
	)
			
})

test_that("Multiple titles are correctly included in a DataTables summary table", {
		
	summaryTable <- data.frame(n = 10)
	
	titles <- c("Title A", "Title B")
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		title = titles,
		outputType = "DT"
	)
	expect_match(
		object = dt$x$caption, 
		regexp = paste(titles, collapse = " ")
	)
			
})

test_that("Multiple titles are correctly included in HTML format in a DataTables summary table", {
			
	summaryTable <- data.frame(n = 10)
			
	# multiple titles
	titles <- htmltools::tags$caption(
		htmltools::a("Formatted .docx table", target="_blank", href = "./myFile.docx"),
		htmltools::br(),
		"This is a test caption."
	)
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		title = titles,
		outputType = "DT"
	)
	expect_match(
		object = dt$x$caption, 
		regexp = "<caption>.*href=.*\\./myFile\\.docx.*This is a test caption.*</caption>"
	)
			
})

test_that("A warning is generated if no data remain after filtering of the column totals in a DataTables summary table", {
			
	data <- data.frame(
		isTotal = rep(TRUE, 2),
		n = c(1, 2)
	)
	expect_warning(
		exportSummaryStatisticsTable(data, outputType = "DT"),
		regexp = "No data remain after filtering of total rows."
	)
			
})

test_that("An error is generated if a DataTables summary table contains multiple values for the column total but no column variables are specified", {
			
	data <- data.frame(
		isTotal = c(FALSE, TRUE, TRUE),
		statN = c(1, 2, 3)
	)
	expect_error(
		exportSummaryStatisticsTable(data, outputType = "DT"),
		regexp = "Multiple values for the header total .*"
	)
			
})


test_that("A summary table is correctly exported to an html file", {
	
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
	)
			
	summaryTable <- data.frame(n = 10)
	
	file <- tempfile(pattern = "table", fileext = ".html")
	expect_silent(
		exportSummaryStatisticsTable(
			summaryTable, 
			file = file
		)
	)
	expect_true(file.exists(file))

})

test_that("Variables are correctly expanded in a DataTables summary table", {
			
	summaryTable <- data.frame(
		patientProfileLink = "/path/to/patientProfile1.pdf",
		stringsAsFactors = FALSE
	)
			
	## stat in the row direction, but in a separated column
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		statsVar = "patientProfileLink",
		expandVar = "patientProfileLink",
		outputType = "DT",
		colHeaderTotalInclude = FALSE
	)

	
	# there is a button available:
	cDefs <- dt$x$options$columnDefs
	isControlPresent <- sapply(cDefs, function(x) 
		hasName(x, "className") && 
		x[["className"]] == "details-control"
	)
	expect_length(which(isControlPresent), 1)
	expect_equal(cDefs[[which(isControlPresent)]]$targets, 0)
	
	# there is a JS callback defined for this variable:
	expect_match(dt$x$callback, regexp = "patientProfileLink")
	
})

test_that("A subset of the variables are correctly expanded in a DataTables summary table", {
			
	summaryTable <- data.frame(
		patientProfileLink = "/path/to/patientProfile1.pdf",
		n = 1,
		stringsAsFactors = FALSE
	)
			
	# creation of expand for only one statistic works without error:
	expect_silent(
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			statsVar = c("n", "patientProfileLink"),
			expandVar = "patientProfileLink",
			statsLayout = "row",
			outputType = "DT"
		)
	)
	
	# there is a button available:
	cDefs <- dt$x$options$columnDefs
	isControlPresent <- sapply(cDefs, function(x) 
		hasName(x, "className") && 
		x[["className"]] == "details-control"
	)
	expect_length(which(isControlPresent), 1)
	expect_equal(cDefs[[which(isControlPresent)]]$targets, 1)
	
	# there is a JS callback defined for this variable:
	expect_length(dt$x$callback, 1)
			
})

test_that("Page length is correctly set in a DataTables summary table", {
			
	summaryTable <- data.frame(n = 10)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		pageDim = c(NA, 3),
		outputType = "DT"
	)
	expect_equal(
		object = dt$x$options$pageLength,
		expected = 3
	)
	
})

test_that("All variables of a summary table are correctly not (HTML) escaped when exported to DataTables", {
			
	summaryTable <- data.frame(
		patientProfileLink = '<a href="www.google.com">blabla</a>',
		n = 1,
		stringsAsFactors = FALSE
	)
	
	# entire table is escaped:
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		noEscapeVar = c("patientProfileLink", "n"),
		statsVar = c("patientProfileLink", "n"),
		outputType = "DT"
	)	
	expect_false(grepl("\\d", attr(dt$x$options, "escapeIdx")))
	
})

test_that("One variable of a summary table is correctly not (HTML) escaped when exported to DataTables", {
			
	summaryTable <- data.frame(
		patientProfileLink = '<a href="www.google.com">blabla</a>',
		n = 1,
		stringsAsFactors = FALSE
	)
	
	# escape only one column
	# This is checked with 'statsLayout' is 'row'
	# as in this case: the entire stat value column
	# should not be escaped
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		noEscapeVar = "patientProfileLink",
		statsVar = c("patientProfileLink", "n"),
		statsLayout = "row",
		outputType = "DT"
	)
	expect_match(
		object = attr(dt$x$options, "escapeIdx"), 
		regexp = "1"
	)
			
})

test_that("A specified parameter is correctly passed to datatable", {
	
	summaryTable <- data.frame(
		patientProfileLink = "www.google.com",
		n = 1,
		stringsAsFactors = FALSE
	)
	
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		outputType = "DT",
		width = 200
	)
	expect_equal(
		object = dt$width,
		expected = 200
	)
	
})

test_that("A warning is generated if a datatable parameter is already set via in-text specific parameters in a DataTables summary table", {
	
	summaryTable <- data.frame(
		patientProfileLink = "www.google.com",
		n = 1,
		stringsAsFactors = FALSE
	)
			
	# warning in case a parameter is already set
	# via the in-text table wrapper
	# e.g. 'noEscapeVar' is passed to datatable 'escape' parameter
	expect_warning(
		exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			noEscapeVar = "patientProfileLink",
			escape = 1,
			outputType = "DT"
		),
		"Parameter.+escape.+ are already specified internally"
	)
	
})

test_that("An error is generated if a specified parameter is not a datatable parameter in a DataTables summary table", {
			
	summaryTable <- data.frame(
		patientProfileLink = "www.google.com",
		n = 1,
		stringsAsFactors = FALSE
	)
	
	# error if parameter not available
	expect_error(
		exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			blabla = "test",
			outputType = "DT"
		),
		"unused argument"
	)
			
})

test_that("A list of summary tables is correctly exported to DataTables", {
			
	summaryTables <- list(
		`PARAM 2` = data.frame(n = 10),
		`PARAM 1` = data.frame(n = 2)
	)
			
	dts <- exportSummaryStatisticsTable(
		summaryTables, 
		outputType = "DT"
	)
	expect_type(dts, "list")
	expect_named(dts, names(summaryTables))
			
	for(group in names(summaryTables)){
				
		# table content is the same as if the table would have
		# been created directly
		expect_identical({
			dt <- exportSummaryStatisticsTable(summaryTables[[!!group]], outputType = "DT")
			dt$x$data
			},
			expected = dts[[!!group]]$x$data
		)
		
		# and group is specified in the title
		expect_true(grepl(!!group, dts[[!!group]]$x$caption,))
			
	}
	
})

test_that("A list of summary tables with different titles is correctly exported to DataTables", {
			
	summaryTables <- list(
		`PAR2` = data.frame(n = 10),
		`PAR1` = data.frame(n = 2)
	)
			
	titles <- c("PARAMETER 2", "PARAMETER 1")
	dts <- exportSummaryStatisticsTable(
		summaryTables, 
		outputType = "DT",
		title = titles
	)
			
	for(i in seq_along(summaryTables)){
		expect_true(grepl(titles[[!!i]], dts[[!!i]]$x$caption))
	}
			
})

test_that("A list of summary tables is successfully exported to html files", {
			
	skip_if_not(
		condition = rmarkdown::pandoc_available(), 
		message = "pandoc is not available"
	)
			
	summaryTables <- list(
		`PARAM 2` = data.frame(n = 10),
		`PARAM 1` = data.frame(n = 2)
	)
			
	file <- tempfile(pattern = "table", fileext = ".html")	
	dts <- exportSummaryStatisticsTable(
		summaryTables, 
		file = file
	)
	fileTableOutput <- paste0(
		tools::file_path_sans_ext(file),
		"_", c("1", "2"), ".html"
	)
	expect_true(all(file.exists(fileTableOutput)))

})


test_that("A variable is successfully visualized as a colored bar in a DataTables summary table", {
			
	summaryTable <- data.frame(
		n = c(1, 2, 4),
		PARAM = paste("PARAM", seq_len(3)),
		stringsAsFactors = FALSE
	)
			
	dt <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		barVar = "n",
		colHeaderTotalInclude = FALSE,
		rowVar = "PARAM", 
		outputType = "DT"
	)
	
	# check if a color gradient is specified in JS in the object:
	expect_match(dt$x$options$rowCallback, "linear-gradient")
			
})