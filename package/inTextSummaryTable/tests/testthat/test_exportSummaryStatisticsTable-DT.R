context("Export summary statistics table to DT")

library(htmltools)

test_that("table is exported with row variable", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)	
	expect_silent(
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "PARAM",
			outputType = "DT"
		)
	)
	expect_s3_class(dt, "datatables")
	expect_identical(
		as.character(dt$x$data[, 1]),
		c("B", "A")
	)
			
})

test_that("warning is table is exported with multiple nested row variables", {
			
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


test_that("table is exported with label for row variables", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "PARAM",
				rowVarLab = c(PARAM = "Parameter"),
				outputType = "DT"
			)
			attr(dt$x, "colnames")[1]
		}, 
		expected = "Parameter"
	)
			
})

test_that("table is exported with label for row variables extracted from label vars", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "PARAM",
				labelVars = c(PARAM = "Parameter"),
				outputType = "DT"
			)
			attr(dt$x, "colnames")[1]
		}, 
		expected = "Parameter"
	)
	
})


test_that("table is exported with row variables in a separated column", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("TRT", "PARAM"),
				rowVarInSepCol = "PARAM",
				outputType = "DT"
			)
			attr(dt$x, "colnames")[1:2]
		}, 
		expected = c("TRT", "PARAM")
	)
	
})

test_that("table is exported with column variables", {
			
	summaryTable <- data.frame(
		TRT = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_silent(
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			colVar = "TRT",
			outputType = "DT"
		)
	)
	expect_s3_class(dt, "datatables")
	expect_identical(
		colnames(dt$x$data),
		c("B", "A")
	)
	
})

test_that("table is exported with total in header", {
	
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				colVar = "TRT",
				outputType = "DT",
				colHeaderTotalInclude = TRUE
			)
			colnames(dt$x$data)
		},
		expected = c("B\n(N=5)", "A\n(N=4)")
	)
	
})
	
test_that("table is exported without total in header", {
		
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
			
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				colVar = "TRT",
				outputType = "DT",
				colHeaderTotalInclude = FALSE
			)
			colnames(dt$x$data)
		},
		expected = c("B", "A")
	)

})

test_that("table without rows with col total is exported without total in header", {
			
	# check that no records are filtered
	summaryTable <- data.frame(
		TRT = factor(c("A", "B"), levels = c("B", "A")),
		statN = c(1, 4),
		isTotal = c(FALSE, FALSE)
	)	

	expect_silent({
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			colVar = "TRT",
			outputType = "DT",
			colHeaderTotalInclude = FALSE
		)
	})
	expect_equal(
		object = dt$x$data, 
		expected = data.frame(B = 4, A = 1), 
		check.attributes = FALSE
	)
			
})

test_that("stat layout is specified", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
	
	## stat in the row direction, but in a separated column
	expect_silent(
		dtStatRowInSepCol <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "variable",
			statsVar = c("n", "Mean"),
			statsLayout = "rowInSepCol",
			outputType = "DT"
		)
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
	
	## stat in row
	expect_silent(
		dtStatInRow <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "variable",
			statsVar = c("n", "Mean"),
			statsLayout = "row",
			outputType = "DT"
		)
	)
	# class of single column differ
	dataDTStatInRow <- as.data.frame(sapply(dtStatInRow$x$data, as.character), stringsAsFactors = FALSE)
	# if columns are nested, the internal dataset represents them as separated column:
	expect_identical(dataDTStatInRow, dataRefStatRowInSepCol)
	# but these are specified via the 'rowGroup' option
	# (in JS column index which starts at 0)
	expect_identical(dtStatInRow$x$options$rowGroup$dataSrc, 0)
	
	## stat in column
	expect_silent(
		dtStatInCol <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "variable",
			statsVar = c("n", "Mean"),
			statsLayout = "col",
			outputType = "DT"
		)		
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

test_that("stat value label is specified", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)
	expect_match(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable, rowVar = "variable", 
				statsVar = "Statistic", statsValueLab = "Number of subjects",
				outputType = "DT"
			)
			colnames(dt$x$data)[2]
		},
		regexp = "Number of subjects.*" # + (N = )
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

test_that("stat value label is specified", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		Mean = c("0.1", "1.3", "4.5", "6.7"),
		stringsAsFactors = FALSE
	)
	
	## only one statistic
	
	# include statistical variable name
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "variable", colVar = "TRT",
				statsVar = "n", 
				statsLabInclude = TRUE,
				outputType = "DT"
			)
			attr(dt$x, "colnames")[2:3]
		},
		expected = c("A_n", "B_n")
	)
	
	# don't include include statistical variable name
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "variable", colVar = "TRT",
				statsVar = "n", 
				statsLabInclude = FALSE,
				outputType = "DT"
			)
			attr(dt$x, "colnames")[2:3]
		},
		expected = c("A", "B")
	)
	
	## multiple statistics
	
	# the statistical variable name should be included
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

test_that("a placeholder is specified for empty value", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = c("1", "2", "3", NA_character_),
		stringsAsFactors = FALSE
	)
	
	# by default, 'empty' value are set to '-'
	# Note that 'numeric-like' column to numeric 
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable,
				rowVar = "PARAM", colVar = "TRT",
				statsVar = "n",
				outputType = "DT"
			)
			dt$x$data
		},
		expected = data.frame(
			PARAM = c("a", "b"),
			A = c(1, 2),
			B = c("3", "-"),
			stringsAsFactors = FALSE
		)
	)
	
	# custom placeholder for empty value
	expect_identical(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable,
				rowVar = "PARAM", colVar = "TRT",
				statsVar = "n",
				emptyValue = "0",
				outputType = "DT"
			)
			dt$x$data
		},
		expected = data.frame(
			PARAM = c("a", "b"),
			A = c(1, 2),
			B = c(3, 0),
			stringsAsFactors = FALSE
		)
	)
			
})

test_that("title is specified", {
		
	summaryTable <- data.frame(n = 10)
	
	# multiple titles
	titles <- c("Title A", "Title B")
	expect_match(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				title = titles,
				outputType = "DT"
			)
			dt$x$caption
		}, 
		regexp = paste(titles, collapse = " ")
	)
			
})

test_that("title is specified as an HTML string", {
			
	summaryTable <- data.frame(n = 10)
			
	# multiple titles
	titles <- htmltools::tags$caption(
		htmltools::a("Formatted .docx table", target="_blank", href = "./myFile.docx"),
		htmltools::br(),
		"This is a test caption."
	)
	expect_silent(
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			title = titles,
			outputType = "DT"
		)
	)
	expect_match(
		object = dt$x$caption, 
		regexp = "<caption>.*href=.*\\./myFile\\.docx.*This is a test caption.*</caption>"
	)
			
})

test_that("no data remains besides total row", {
			
	data <- data.frame(
		isTotal = rep(TRUE, 2),
		n = c(1, 2)
	)
	expect_warning(
		exportSummaryStatisticsTable(data, outputType = "DT"),
		regexp = "No data remain after filtering of total rows."
	)
			
})

test_that("total header should unique in DT export", {
			
	data <- data.frame(
		isTotal = c(FALSE, TRUE, TRUE),
		statN = c(1, 2, 3)
	)
	expect_error(
		exportSummaryStatisticsTable(data, outputType = "DT"),
		regexp = "Multiple values for the header total .*"
	)
			
})


test_that("export summary table to a file", {
			
	summaryTable <- data.frame(n = 10)
	
	file <- "table.html" 
	if(file.exists(file))	tmp <- file.remove(file)
	
	expect_silent(
		exportSummaryStatisticsTable(
			summaryTable, outputType = "DT", file = file
		)
	)
	expect_true(file.exists(file))

})

test_that("table is exported to DT with row variables with expand variables", {
			
	summaryTable <- data.frame(
		patientProfileLink = "/path/to/patientProfile1.pdf",
		stringsAsFactors = FALSE
	)
			
	## stat in the row direction, but in a separated column
	expect_silent(
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			statsVar = "patientProfileLink",
			expandVar = "patientProfileLink",
			outputType = "DT",
			colHeaderTotalInclude = FALSE
		)
	)
	
	# there is a button available:
	cDefs <- dt$x$options$columnDefs
	isControlPresent <- which(sapply(cDefs, function(x) "className" %in% names(x)))
	expect_length(isControlPresent, 1)
	expect_equal(cDefs[[isControlPresent]]$className, "details-control")
	expect_equal(cDefs[[isControlPresent]]$targets, 0)
	
	# there is a JS callback defined for this variable:
	expect_match(dt$x$callback, regexp = "patientProfileLink")
	
})

test_that("one statistical variable is expanded", {
			
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
	isControlPresent <- which(sapply(cDefs, function(x) "className" %in% names(x)))
	expect_length(isControlPresent, 1)
	expect_equal(cDefs[[isControlPresent]]$className, "details-control")
	expect_equal(cDefs[[isControlPresent]]$targets, 1)
	
	# there is a JS callback defined for this variable:
	expect_length(dt$x$callback, 1)
			
})

test_that("page dimension is specified", {
			
	summaryTable <- data.frame(n = 10)
	
	expect_equal(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				pageDim = c(NA, 3),
				outputType = "DT"
			)
			dt$x$options$pageLength
		},
		expected = 3
	)
	
})

test_that("a stat variable is not escaped", {
			
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
	
	# escape only one column
	# This is checked with 'statsLayout' is 'row'
	# as in this case: the entire stat value column
	# should not be escape
	expect_match({
		dt <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			noEscapeVar = "patientProfileLink",
			statsVar = c("patientProfileLink", "n"),
			statsLayout = "row",
			outputType = "DT"
		)
		attr(dt$x$options, "escapeIdx")
		}, 
		regexp = "1"
	)
			
})

test_that("parameters are passed to datatable", {
	
	summaryTable <- data.frame(
		patientProfileLink = "www.google.com",
		n = 1,
		stringsAsFactors = FALSE
	)
	
	# pass parameter to data.table
	expect_equal(
		object = {
			dt <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				outputType = "DT",
				width = 200
			)
			dt$width
		},
		expected = 200
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

test_that("list of summary tables is specified", {
			
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

test_that("different titles are specified for a list of summary tables", {
			
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

test_that("list of summary tables is exported to file", {
			
	summaryTables <- list(
		`PARAM 2` = data.frame(n = 10),
		`PARAM 1` = data.frame(n = 2)
	)
			
	file <- "table.html" 
	
	fileTableOutput <- c("table_1.html", "table_2.html")
	if(any(file.exists(fileTableOutput)))
		tmp <- file.remove(fileTableOutput)
	
	dts <- exportSummaryStatisticsTable(
		summaryTables, 
		outputType = "DT",
		file = file
	)
	expect_true(all(file.exists(fileTableOutput)))

})


test_that("a variable is visualized as a bar", {
			
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