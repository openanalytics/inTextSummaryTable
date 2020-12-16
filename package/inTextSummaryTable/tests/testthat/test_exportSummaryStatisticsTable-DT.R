context("Export summary statistics table to DT")

test_that("table is exported to DT with row variables", {
			
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

test_that("table is exported to DT with label for row variables", {
			
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

test_that("table is exported to DT with label for row variables extracted from label vars", {
			
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


test_that("table is exported to DT with row variables in a separated column", {
			
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

test_that("table is exported to DT with column variables", {
			
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

test_that("table is exported to DT with total in header", {
	
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	# with header:
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
	
	# without header
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

test_that("different stat layout when table is exported to DT", {
			
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

test_that("stat value label is specified when table is exported to DT", {
			
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

test_that("stat value label is specified when table is exported to DT", {
			
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

test_that("a placeholder is specified for empty value when table is exported to DT", {
			
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

test_that("title is specified for DT export", {
		
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

test_that("no data remains besides total row in export DT", {
			
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


test_that("export DT to a file", {
			
	summaryTable <- data.frame(n = 10)
	
	fileTable <- "table.html" 
	if(file.exists(fileTable))	tmp <- file.remove(fileTable)
	
	expect_silent(
		exportSummaryStatisticsTable(
			summaryTable, outputType = "DT", file = fileTable
		)
	)
	expect_true(file.exists(fileTable))

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

test_that("table is exported to DT with row variables with expand one statistic", {
			
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

test_that("page dimension is specified when the table is exported to DT", {
			
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

test_that("parameters are passed to datatable for DT export", {
	
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

			
