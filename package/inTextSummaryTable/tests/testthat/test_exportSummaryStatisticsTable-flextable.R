context("Export summary statistics table to flextable")

test_that("row variable is specified", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)	
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "PARAM"
		)
	)
	expect_s3_class(ft, "flextable")
	expect_identical(
		ft$body$dataset[, 1],
		c("B", "A")
	)
			
})

test_that("row(s) are nested correctly for multiple row variable(s)", {
			
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
			
	rowPadBase <- 50
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT"),
		rowPadBase = rowPadBase
	)
	
	# check that correct data is displayed
	dataRef <- data.frame(
		c(
			"Actual Value", "I", "B", "A", "II", "A",
			"Change from baseline", "I", "B", "A", "II", "A"
		),
		c(
			NA_character_, NA_character_, "2", "1", NA_character_, "3", 
			NA_character_, NA_character_, "5", "4", NA_character_, "6"
		),
		stringsAsFactors = FALSE
	)
	expect_equal(
		unname(ft$body$dataset),
		unname(dataRef),
		check.attributes = FALSE
	)
	
	# check that correct padding is set for the nested column
	# (Note: remove row headers because flextable default padding is used)
	expect_equal(
		ft$body$styles$pars$padding.left$data[-c(1, 7), 1],
		rowPadBase * rep(c(1, 2, 2, 1, 2), 2)
	)
			
})		

test_that("label is specified for row variable", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "PARAM",
				rowVarLab = c(PARAM = "Parameter")
			)
			ft$header$dataset[, 1]
		}, 
		expected = "Parameter"
	)
			
})

test_that("label for row variable is extracted from label vars", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "PARAM",
				labelVars = c(PARAM = "Parameter")
			)
			ft$header$dataset[, 1]
		}, 
		expected = "Parameter"
	)
	
})


test_that("row variable(s) are in a separated column", {
			
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT"),
		rowVarInSepCol = c("COHORT", "TRT")
	)
	
	expect_identical(
		object = as.character(as.vector(ft$header$dataset[1, 1:3])),
		expected = c("PARAM", "COHORT", "TRT")
	)
	
	# check if data is correct
	# Note: factor should have been re-ordered based on levels (e.g. TRT)
	dataRef <- data.frame(
		rep(c("Actual Value", "Change from baseline"), each = 3),
		rep(c("I", "I", "II"), times = 2),
		rep(c("B", "A", "A"), times = 2),
		c("2", "1", "3", "5", "4", "6"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		object = unname(ft$body$dataset),
		expected = unname(dataRef),
		check.attributes = FALSE
	)
	
})

test_that("horizontal lines are correctly set for multiple row variable(s) in separated column", {
	
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT"),
		rowVarInSepCol = c("COHORT", "TRT"),
		colorTable = c(line = "red")
	)
	
	# check that horizontal lines are properly set
	ftDataBd <- ft$body$styles$cells$border.color.top$data
	# separator between groups is set
	expect_setequal(
		object = ftDataBd[c(3, 4, 6), 2:4], 
		expected = "red"
	)
	# other lines are not set
	expect_false(unique(c(ftDataBd[-c(3, 4, 6), 2:4])) == "red") 
			
})

test_that("row(s) are merged correctly for multiple row variable(s) in separated column", {
	
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT"),
		rowVarInSepCol = c("COHORT", "TRT")
	)
			
	# flextable specify 'merging' as the 'spans' in 'columns'
			
	# correct lines are merged
	expect_identical(
		ft$body$spans$columns[, 1:2],
		cbind(
			c(3, 0, 0, 3, 0, 0),
			c(2, 0, 1, 2, 0, 1)
		)
	)
	
	# other lines are not merged
	expect_setequal(
		c(ft$body$spans$columns[, -(1:2)]),
		1
	)
	
})

test_that("row formatting is specified", {
			
	summaryTable <- data.frame(
		TRT = c("A", "B"),
		n = 1:2
	)
			
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "TRT",
				rowVarFormat = list(TRT = "bold")
			)
			unname(ft$body$styles$text$bold$data)
		},
		expected = cbind(
			rep(TRUE, nrow(summaryTable)),
			rep(FALSE, nrow(summaryTable))
		)
	)	
		
})


test_that("row formatting is specified for nested row var", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("TRT", "PARAM"),
				rowVarFormat = list(PARAM = "bold")
			)
			unname(ft$body$styles$text$bold$data)
		},
		expected = cbind(
			c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
			rep(FALSE, 6)
		)
	)
			
})

test_that("row formatting is specified for row in different column", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
		
	# only entire second column is in bold
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("TRT", "PARAM"),
				rowVarInSepCol = "PARAM",
				rowVarFormat = list(PARAM = "bold")
			)
			unname(ft$body$styles$text$bold$data)
		},
		expected = cbind(
			rep(FALSE, nrow(summaryTable)),
			rep(TRUE, nrow(summaryTable)),
			rep(FALSE, nrow(summaryTable))
		)
	)
			
})

test_that("row total is included in the header row", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = factor(c("a", "Total", "b", "Total"), levels = c("Total", "a", "b")),
		n = c("2", "1", "4", "3")
	)
			
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("TRT", "PARAM"),
				rowVarTotalInclude = "PARAM"
			)
			ft$body$dataset
		},
		expected = data.frame(
			c("A", "a", "B", "b"),
			c("1", "2", "3", "4"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})

test_that("row total is included in a separated row", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = factor(c("a", "Total", "b", "Total"), levels = c("Total", "a", "b")),
		n = c("2", "1", "4", "3")
	)
			
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("TRT", "PARAM"),
				rowVarTotalInclude = "PARAM",
				rowVarTotalInSepRow = "PARAM"
			)
			ft$body$dataset
		},
		expected = data.frame(
			c("A", "Total", "a", "B", "Total", "b"),
			c(NA_character_, "1", "2", NA_character_, "3", "4"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})

test_that("rows are merged in case of unique variable group", {
			
	summaryTable <- data.frame(
		variable = c("A", "B", "B"),
		variableGroup = c("a", "b1", "b2"),
		n = c("1", "2", "3")
	)
		
	# (by default) variable and variableGroup 
	# are automatically merged in one row if only one category
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("variable", "variableGroup"),
				rowAutoMerge = TRUE
			)
			ft$body$dataset[, 1]
		},
		expected = c("A a", "B", "b1", "b2"),
		check.attributes = FALSE
	)
	
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = c("variable", "variableGroup"),
				rowAutoMerge = FALSE
			)
			ft$body$dataset[, 1]
		},
		expected = c("A", "a", "B", "b1", "b2"),
		check.attributes = FALSE
	)
	
})

test_that("rows are merged in case of unique statistic", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c(NA_character_, "0.56"),
		stringsAsFactors = FALSE
	)
			
	# (by default) row var and statistic label
	# are automatically merged in the same row if only one stat
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "variable",
				statsVar = c("n", "Mean"),
				rowAutoMerge = TRUE
			)
			ft$body$dataset[, 1]
		},
		expected = c("A n", "B", "n", "Mean"),
		check.attributes = FALSE
	)
	
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "variable",
				statsVar = c("n", "Mean"),
				rowAutoMerge = FALSE
			)
			ft$body$dataset[, 1]
		},
		expected = c("A", "n", "B", "n", "Mean"),
		check.attributes = FALSE
	)
	
})

test_that("column variable is specified", {
			
	summaryTable <- data.frame(
		TRT = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			colVar = "TRT"
		)
	)
	expect_s3_class(ft, "flextable")
	expect_identical(
		unname(unlist(ft$header$dataset[1, ])),
		c("B", "A")
	)
	
})

test_that("total is included in header", {
	
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	# with header:
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				colVar = "TRT",
				colHeaderTotalInclude = TRUE
			)
			unname(unlist(ft$header$dataset))
		},
		expected = c("B\n(N=5)", "A\n(N=4)")
	)
	
	# without header
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				colVar = "TRT",
				colHeaderTotalInclude = FALSE
			)
			unname(unlist(ft$header$dataset))
		},
		expected = c("B", "A")
	)

})

test_that("stat layout is specified", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
	
	# stat in row
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				rowVar = "variable",
				statsVar = c("n", "Mean"),
				statsLayout = "row"
			)
			ft$body$dataset
		},
		expected = data.frame(
			c("A", "n", "Mean", "B", "n", 'Mean'),
			c(NA_character_, "1", "0.34", NA_character_, "2", "0.56"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
	# stat in column
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "variable",
			statsVar = c("n", "Mean"),
			statsLayout = "col"
		)		
	)
	expect_identical(
		object = unname(unlist(ft$header$dataset[1, ])),
		expected = c("variable", "n", "Mean")
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("A", "B"),
			c("1", "2"),
			c("0.34", "0.56"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE	
	)
	
	# stat in the row direction, but in a separated column
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = "variable",
			statsVar = c("n", "Mean"),
			statsLayout = "rowInSepCol"
		)
	)
	expect_identical(unname(unlist(ft$header$dataset[1:2])), c("variable", "Statistic"))
	expect_equal(
		object = as.data.frame(sapply(ft$body$dataset, as.character)),
		expected = data.frame(
			c("A", "A", "B", "B"),
			c("n", "Mean", "n", "Mean"),
			c("1", "0.34", "2", "0.56")
		),
		check.attributes = FALSE	
	)
	
})

test_that("stat value label is specified", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)
	expect_match(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, rowVar = "variable", 
				statsVar = "Statistic", 
				statsValueLab = "Number of subjects"
			)
			unname(unlist(ft$header$dataset[, 2]))
		},
		regexp = "Number of subjects.*" # + (N = )
	)
	
	# error is label is set to 'Statistic' (used as default naming)
	expect_error(
		exportSummaryStatisticsTable(summaryTable, rowVar = "variable", 
			statsVar = "Statistic", statsValueLab = "Statistic"
		),
		"'statsValueLab' should be different than 'Statistic'."
	)
			
})

test_that("label for stat is included", {
			
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
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "variable", colVar = "TRT",
				statsVar = "n", 
				statsLabInclude = TRUE
			)
			ft$body$dataset[, 1]
		},
		expected = c("a", "n", "b", "n")
	)
	
	# don't include include statistical variable name
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "variable", colVar = "TRT",
				statsVar = "n", 
				statsLabInclude = FALSE
			)
			ft$body$dataset[, 1]
		},
		expected = c("a", "b")
	)
	
	## multiple statistics
	
	# the statistical variable name should be included
	expect_warning(
		exportSummaryStatisticsTable(
			summaryTable, 
			rowVar = "variable", colVar = "TRT",
			statsVar = c("n", "Mean"),
			statsLabInclude = FALSE
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
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable,
				rowVar = "PARAM", colVar = "TRT",
				statsVar = "n"
			)
			ft$body$dataset
		},
		expected = data.frame(
			c("a", "b"),
			c("1", "2"),
			c("3", "-"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
	# custom placeholder for empty value
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable,
				rowVar = "PARAM", colVar = "TRT",
				statsVar = "n",
				emptyValue = "0"
			)
			ft$body$dataset
		},
		expected = data.frame(
			c("a", "b"),
			c("1", "2"),
			c("3", "0"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
			
})

test_that("list of summary tables is specified", {
			
	summaryTables <- list(
		`PARAM 2` = data.frame(n = 10),
		`PARAM 1` = data.frame(n = 2)
	)
			
	fts <- exportSummaryStatisticsTable(
		summaryTables, 
		outputType = "flextable"
	)
	expect_type(fts, "list")
	expect_named(fts, names(summaryTables))
			
	for(group in names(summaryTables)){
			
		# table content is the same as if the table would have
		# been created directly
		expect_identical({
				ft <- exportSummaryStatisticsTable(summaryTables[[!!group]])
				ft$body$dataset
			},
			expected = fts[[!!group]]$body$dataset
		)
		
		# correct group is specified in the title
		expect_equal(fts[[!!group]]$header$dataset[1, ], !!group)

	}
			
})

test_that("formatting row total variable is correct", {
			
	summaryTable <- data.frame(
		variable = factor(c("RACE", "SEX", "SEX", "SEX")),
		variableGroup = factor(
			c("White", "Female", "Male", "Total"), 
			levels = c("White", "Female", "Male", "Total")
		),
		n = c("9", "3", "7", "10")
	)
			
	expect_warning(
		exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = c("variable", "variableGroup"),
			statsVar = "n",
			rowVarTotalInclude = "variableGroup"
		),
		"variable.*with total.*should be formatted.*with 'Total' as the first level"
	)
	
})

test_that("only a subset of the variables have a total", {
			
	summaryTable <- data.frame(
		variable = factor(c("RACE", "SEX", "SEX", "SEX")),
		variableGroup = factor(
			c("White", "Female", "Male", "Total"), 
			levels = c("Total", "White", "Female", "Male")
		),
		n = c("9", "3", "7", "10")
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = "n",
		rowVarTotalInclude = "variableGroup"
	)
	
	refData <- data.frame(
		c("RACE", "White", "SEX", "Female", "Male"),
		c(NA_character_, "9", "10", "3", "7"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		unname(ft$body$dataset),
		unname(refData),
		check.attributes = FALSE
	)
	
})

test_that("only a subset of the variables have a total in a separated row", {
			
	summaryTable <- data.frame(
		variable = factor(c("RACE", "SEX", "SEX", "SEX")),
		variableGroup = factor(
			c("White", "Female", "Male", "Total"), 
			levels = c("Total", "White", "Female", "Male")
		),
		n = c("9", "3", "7", "10")
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = "n",
		rowVarTotalInclude = "variableGroup",
		rowVarTotalInSepRow = "variableGroup"
	)
			
	refData <- data.frame(
		c("RACE White", "SEX", "Total", "Female", "Male"),
		c("9", NA_character_, "10", "3", "7"),
		stringsAsFactors = FALSE
	)
			
	expect_equal(
		unname(ft$body$dataset),
		unname(refData),
		check.attributes = FALSE
	)
			
})

test_that("missing values are present in nested row variables", {
			
	summaryTable <- data.frame(
		variable = factor(c("SEX", "SEX", "SEX")),
		variableGroup = factor(
			c(NA_character_, "Male", "Female"), 
			levels = c("Male", "Female")
		),
		n = c("3", "7", "10")
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = "n"
	)
			
	refData <- data.frame(
		c("SEX", "Male", "Female", NA_character_),
		c(NA_character_, "7", "10", "3"),
		stringsAsFactors = FALSE
	)
			
	expect_equal(
		unname(ft$body$dataset),
		unname(refData),
		check.attributes = FALSE
	)
			
})

test_that("only one element in nested row variable", {
			
	summaryTable <- data.frame(
		variable = "SEX",
		variableGroup = factor("Total"),
		n = "10"
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = "n",
		rowVarTotalInclude = "variableGroup"
	)
	
	expect_equal(
		unname(ft$body$dataset),
		unname(data.frame("SEX", "10", stringsAsFactors = FALSE)),
		check.attributes = FALSE
	)

})

test_that("page dimension is specified", {
	
	summaryTable <- data.frame(n = 10)
	
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				pageDim = c(10, 3),
				margin = 0
			)
			ft$body$colwidths
		},
		expected = 10
	)
			
})

test_that("margins are specified", {
			
	summaryTable <- data.frame(n = 10)
	
	expect_equal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				pageDim = c(10, 3),
				margin = 1
			)
			ft$body$colwidths
		},
		expected = 8
	)
	
})

test_that("table is displayed in landscape mode", {
			
	summaryTable <- data.frame(n = 10)	
	
	# check that width of body in landscape mode > width in portrait mode
	expect_gt(
		object = {
			ftLandscape <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				landscape = TRUE
			)
			ftPortrait <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				landscape = FALSE
			)
			sum(ftLandscape$body$colwidths) - sum(ftPortrait$body$colwidths)
		}, 
		expected = 0
	)
	
})

test_that("title is specified", {
		
	summaryTable <- data.frame(n = 10)
	
	# multiple titles
	titles <- c("Title A", "Title B")
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				title = titles
			)
			ft$header$dataset[seq_along(titles), 1]
		}, titles
	)
			
})

test_that("footer is specified", {
			
	summaryTable <- data.frame(n = 10)
	
	# multiple footers
	footers <- c("Explanation 1", "Explanation 2")
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable = summaryTable,
				footer = footers
			)
			ft$footer$dataset[seq_along(footers), 1]
		}, footers
	)
	
})

test_that("color is specified", {
	
	summaryTable <- data.frame(n = 10)
	
	colorTable <- c(
		header = "#330000", headerBackground = "#CC3333",
		body = "#CCCC00", bodyBackground = "#FFFFCC", 
		footerBackground = "#CCFFFF", footer = "#000066",
		line = "#669900"
	)
	
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			summaryTable, 
			colorTable = colorTable,
			footer = "test"
		)
	)
	# check colors in flextable object
	expect_setequal(ft$body$styles$text$color$data, colorTable["body"])
	expect_setequal(ft$body$styles$cells$background.color$data, colorTable["bodyBackground"])
	expect_setequal(ft$header$styles$text$color$data, colorTable["header"])
	expect_setequal(ft$header$styles$cells$background.color$data, colorTable["headerBackground"])
	expect_setequal(ft$footer$styles$text$color$data, colorTable["footer"])
	expect_setequal(ft$footer$styles$cells$background.color$data, colorTable["footerBackground"])
	expect_setequal(ft$body$styles$cells$border.color.bottom$data, colorTable["line"])
	
})

test_that("fontsize is specified", {
			
	summaryTable <- data.frame(n = 10)
	
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				fontsize = 25
			)
			c(ft$body$styles$text$font.size$data)
		}, 
		expected = 25
	)
			
})

test_that("fontname is specified", {
			
	summaryTable <- data.frame(n = 10)
	
	expect_identical(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				fontname = "Arial"
			)
			c(ft$body$styles$text$font.family$data)
		}, 
		expected = "Arial"
	)
	
})

test_that("vertical lines are set", {
				
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		AVISIT = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	
	# no vertical lines included
	expect_setequal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				colVar = c("TRT", "AVISIT"),
				statsVar = "n",
				vline = "none"
			)
			ft$body$styles$cells$border.width.left$data
		}, 
		expected = 0
	)
	
	# vertical line included between subgroups
	expect_setequal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				colVar = c("TRT", "AVISIT"),
				statsVar = "n",
				vline = "auto"
			)
			ft$body$styles$cells$border.width.left$data[, c(1, 3)]
		}, 
		expected = 1
	)
			
})

test_that("horizontal lines are set", {
			
	summaryTable <- data.frame(
		PARAM = c("a", "b"),
		n = c("1", "2"),
		Mean = c("1", "2"),
		stringsAsFactors = FALSE
	)
			
	# no horizontal lines included
	expect_setequal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "PARAM",
				statsVar = c("n", "Mean"),
				hline = "none"
			)
			ft$body$styles$cells$border.width.top$data
		}, 
		expected = 0
	)
	
	# horizontal line included between subgroups
	expect_setequal(
		object = {
			ft <- exportSummaryStatisticsTable(
				summaryTable, 
				rowVar = "PARAM",
				statsVar = c("n", "Mean"),
				hline = "auto"
			)
			ft$body$styles$cells$border.width.top$data[4, ]
		}, 
		expected = 1
	)
	
})

test_that("no data remains besides total row", {
			
	data <- data.frame(
		isTotal = rep(TRUE, 2),
		n = c(1, 2)
	)
	expect_warning(
		exportSummaryStatisticsTable(data),
		regexp = "No data remain after filtering of total rows."
	)
			
})

test_that("total header is unique", {
			
	data <- data.frame(
		isTotal = c(FALSE, TRUE, TRUE),
		statN = c(1, 2, 3)
	)
	expect_error(
		exportSummaryStatisticsTable(data),
		regexp = "Multiple values for the header total .*"
	)
			
})
