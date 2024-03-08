context("Export summary statistics table to a flextable")

library(flextable)
library(officer)

test_that("A summary table with a row variable is correctly exported to flextable", {
			
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

test_that("Multiple row variables are nested correctly in a flextable summary table", {
			
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT")
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
			
})		

test_that("Padding is correctly set for nested row variables in a flextable summary table", {
			
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
			
	# check that correct padding is set for the nested column
	# (Note: remove row headers because flextable default padding is used)
	expect_equal(
		ft$body$styles$pars$padding.left$data[-c(1, 7), 1],
		rowPadBase * rep(c(1, 2, 2, 1, 2), 2)
	)
	
})

test_that("Horizontal lines are correctly set for nested row variables in a flextable summary table", {
			
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)

	# specify all colors as requested
	colorTable <- getColorPaletteTable()
	colorTable["line"] <- "red"
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("PARAM", "COHORT", "TRT"),
		colorTable = colorTable
	)
	
	# check that horizontal lines are properly set
	ftDataBd <- ft$body$styles$cells$border.color.top$data
	# separator between groups is set
	expect_setequal(
		object = ftDataBd[7, ], 
		expected = "red"
	)
	# other lines are not set
	expect_false(unique(c(ftDataBd[-7, ])) == "red") 
	
})
			
test_that("The specified labels of the row variables are correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(
		PARAM = factor(c("A", "B"), levels = c("B", "A")),
		n = c(9, 10)
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "PARAM",
		rowVarLab = c(PARAM = "Parameter")
	)
	expect_identical(
		object = ft$header$dataset[, 1], 
		expected = "Parameter"
	)
			
})

test_that("The labels of the row variables, extracted from the labels of all variables, are correctly set in a flextable summary table", {
			
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

test_that("Row variables in a separated column are merged correctly in a flextable summary table", {
			
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
	
	# each variable is exported as separated column
	expect_identical(
		object = as.character(as.vector(ft$header$dataset[1, 1:3])),
		expected = c("PARAM", "COHORT", "TRT")
	)
	
	# the data is correct
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

test_that("Nested and merged row variables are displayed correctly in a flextable summary table", {
			
	summaryTable <- data.frame(
		AESOC = c("A", "A", "A"),
		AEDECOD = factor(c("a", "b", "b"), levels = c("b", "a")),
		WORSTINT = factor(
			c("Moderate", "Severe", "Moderate"), 
			levels = c("Moderate", "Severe")
		),
		n = c(2, 4, 7)
	)
	
	rowPadBase <- 50
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("AESOC", "AEDECOD", "WORSTINT"),
		rowVarInSepCol = "WORSTINT",
		rowPadBase = rowPadBase
	)
	
	# check if data is correct
	dataRef <- data.frame(
		c("A", "b", "b", "a"),
		c(NA_character_, "Moderate", "Severe", "Moderate"),
		c(NA_character_, "7", "4", "2"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		object = unname(ft$body$dataset),
		expected = unname(dataRef),
		check.attributes = FALSE
	)
	
	# check that correct padding is set for the nested column
	# (Note: remove row headers because flextable default padding is used)
	expect_setequal(
		ft$body$styles$pars$padding.left$data[-1, 1],
		rowPadBase
	)
	
})

test_that("Horizontal lines are correctly set for multiple row variables in a separated column in a flextable summary table", {
	
	summaryTable <- data.frame(
		PARAM = rep(c("Actual Value", "Change from baseline"), each = 3),
		COHORT = rep(c("I", "I", "II"), times = 2),
		TRT = factor(rep(c("A", "B", "A"), times = 2), levels = c("B", "A")),
		n = seq_len(6)
	)
	
	# warning because not all table colors are set
	expect_warning(
		ft <- exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			rowVar = c("PARAM", "COHORT", "TRT"),
			rowVarInSepCol = c("COHORT", "TRT"),
			colorTable = c(line = "red")
		)
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

test_that("Custom formatting is correctly set to a row variable in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "B"),
		n = 1:2
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "TRT",
		rowVarFormat = list(TRT = "bold")
	)
	expect_identical(
		object = unname(ft$body$styles$text$bold$data),
		expected = cbind(
			rep(TRUE, nrow(summaryTable)),
			rep(FALSE, nrow(summaryTable))
		)
	)	
		
})

test_that("Custom formatting is correctly set to a nested row variable in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("TRT", "PARAM"),
		rowVarFormat = list(PARAM = "bold")
	)
	expect_identical(
		object = unname(ft$body$styles$text$bold$data),
		expected = cbind(
			c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
			rep(FALSE, 6)
		)
	)
			
})

test_that("Custom formatting is correctly set to a row variable in a different column in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = 1:4
	)
		
	# only entire second column is in bold
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("TRT", "PARAM"),
		rowVarInSepCol = "PARAM",
		rowVarFormat = list(PARAM = "bold")
	)
	expect_identical(
		object = unname(ft$body$styles$text$bold$data),
		expected = cbind(
			rep(FALSE, nrow(summaryTable)),
			rep(TRUE, nrow(summaryTable)),
			rep(FALSE, nrow(summaryTable))
		)
	)
			
})

test_that("Row totals are correctly included in the header row in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = factor(c("a", "Total", "b", "Total"), levels = c("Total", "a", "b")),
		n = c("2", "1", "4", "3")
	)
		
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("TRT", "PARAM"),
		rowVarTotalInclude = "PARAM"
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("A", "a", "B", "b"),
			c("1", "2", "3", "4"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})

test_that("Row totals are correctly included in separated rows in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = factor(c("a", "Total", "b", "Total"), levels = c("Total", "a", "b")),
		n = c("2", "1", "4", "3")
	)
		
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("TRT", "PARAM"),
		rowVarTotalInclude = "PARAM",
		rowVarTotalInSepRow = "PARAM"
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("A", "Total", "a", "B", "Total", "b"),
			c(NA_character_, "1", "2", NA_character_, "3", "4"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})

test_that("The variable group and name are correctly merged in a flextable summary table in case of unique group", {
			
	summaryTable <- data.frame(
		variable = c("A", "B", "B"),
		variableGroup = c("a", "b1", "b2"),
		n = c("1", "2", "3")
	)
		
	# (by default) variable and variableGroup 
	# are automatically merged in one row if only one category
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		rowAutoMerge = TRUE
	)
	expect_equal(
		object = ft$body$dataset[, 1],
		expected = c("A a", "B", "b1", "b2"),
		check.attributes = FALSE
	)
	
})

test_that("The variable group and name are correctly included in different rows when requested in a flextable summary table in case of unique group", {
	
	summaryTable <- data.frame(
		variable = c("A", "B", "B"),
		variableGroup = c("a", "b1", "b2"),
		n = c("1", "2", "3")
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		rowAutoMerge = FALSE
	)
	expect_equal(
		object = ft$body$dataset[, 1],
		expected = c("A", "a", "B", "b1", "b2"),
		check.attributes = FALSE
	)
	
})

test_that("The variable name and the statistic are correctly merged in a flextable summary table in case of unique statistic", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c(NA_character_, "0.56"),
		stringsAsFactors = FALSE
	)
			
	# (by default) row var and statistic label
	# are automatically merged in the same row if only one stat
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		rowAutoMerge = TRUE
	)
	expect_equal(
		object = ft$body$dataset[, 1],
		expected = c("A n", "B", "n", "Mean"),
		check.attributes = FALSE
	)
	
})

test_that("The variable name and the statistic are correctly included in different rows when requested in a flextable summary table in case of unique statistic", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c(NA_character_, "0.56"),
		stringsAsFactors = FALSE
	)		
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		rowAutoMerge = FALSE
	)
	expect_equal(
		object = ft$body$dataset[, 1],
		expected = c("A", "n", "B", "n", "Mean"),
		check.attributes = FALSE
	)
	
})

test_that("A summary table with a continuous and a categorical variables is correctly exported to flextable", {
			
	summaryTable <- data.frame(
		variable = factor(
			c("SEX", "SEX", "AGE"), 
			levels = c("SEX", "AGE")
		),
		variableGroup = factor(
			c("Female", "Male", NA_character_),
			levels = c("Male", "Female")
		),
		n = c(3, 4, NA_real_),
		mean = c(NA_real_, NA_real_, 3.33)
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = c("n", "mean")
	)
	
	dataRef <- data.frame(
		c("SEX", "Male n", "Female n", "AGE mean"),
		c(NA_character_, "4", "3", "3.33"),
		stringsAsFactors = FALSE
	)
	expect_equal(
		object = unname(ft$body$dataset),
		expected = unname(dataRef),
		check.attributes = FALSE	
	)
			
})	

test_that("A summary table with a continuous and a categorical variables is correctly exported without row merging to flextable", {
		
	# Edge-case: different levels of nestings by row variables
	# because variables are of different types (categorical/numeric)
	# Padding is extracted for each nested level
	# and smoothed afterwards to avoid double padding for cont var 
	# (check 'smoothPadding')
			
	summaryTable <- data.frame(
		variable = factor(
			c("SEX", "SEX", "AGE"), 
			levels = c("SEX", "AGE")
		),
		variableGroup = factor(
			c("Female", "Male", NA_character_),
			levels = c("Male", "Female")
		),
		n = c(3, 4, NA_real_),
		mean = c(NA_real_, NA_real_, 3.33)
	)
			
	rowPadBase <- 50
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		statsVar = c("n", "mean"),
		rowPadBase = rowPadBase,
		rowAutoMerge = FALSE
	)
		
	# check that data is correctly set
	dataRef <- data.frame(
		c("SEX", "Male", "n", "Female", "n", 
			"AGE", "mean"),
		c(NA_character_, NA_character_, "4", NA_character_, "3", 
			NA_character_, "3.33"),
		stringsAsFactors = FALSE
	)
	
	expect_equal(
		object = unname(ft$body$dataset),
		expected = unname(dataRef),
		check.attributes = FALSE	
	)
	
	# check that correct padding is set for the nested column
	# (Note: remove row headers because flextable default padding is used)
	expect_equal(
		ft$body$styles$pars$padding.left$data[-c(1, 6), 1],
		rowPadBase * c(1, 2, 1, 2, 1)
	)
	
})	

test_that("A summary table with a continuous and a categorical variable, with and without named statistic, is correctly exported to flextable", {
			
	# edge case to check if merging rows variableGroup/stats is correct
	summaryTable <- data.frame(
		variable = factor(
			c("SEX", "SEX", "AGE"), 
			levels = c("SEX", "AGE")
		),
		variableGroup = factor(
			c("Female", "Male", NA_character_),
			levels = c("Male", "Female")
		),
		Statistic = c(3, 4, NA_real_),
		Mean = c(NA_real_, NA_real_, 3.33)
	)
			
	rowPadBase <- 50
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = c("variable", "variableGroup"),
		rowPadBase = rowPadBase
	)	
	
	# check that data is correctly set
	dataRef <- data.frame(
		c("SEX", "Male", "Female", "AGE Mean"),
		c(NA_character_, "4", "3", "3.33"),
		stringsAsFactors = FALSE
	)
	
	expect_equal(
		object = unname(ft$body$dataset),
		expected = unname(dataRef),
		check.attributes = FALSE	
	)
	
	# check that correct padding is set for the nested column
	# (Note: remove row headers because flextable default padding is used)
	expect_setequal(
		ft$body$styles$pars$padding.left$data[-c(1, 4), 1],
		rowPadBase
	)
			
})

test_that("A summary table with a column variable is correctly exported to flextable", {
			
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

test_that("An error is generated if a flextable summary table contains multiple records for the same variable", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B"), 
		n = c(9, 8, 10)
	)
	expect_error(
		exportSummaryStatisticsTable(
			summaryTable = summaryTable,
			colVar = "TRT"
		),
		"Table formatting to multiple columns failed because of duplicated records for each row/col"
	)
			
})

test_that("A summary table without totals in the header is correctly exported to flextable", {
	
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		colHeaderTotalInclude = TRUE
	)
	expect_identical(
		object = unname(unlist(ft$header$dataset)),,
		expected = c("B\n(N=5)", "A\n(N=4)")
	)
	
})
	
test_that("A summary table without totals in the header is correctly exported to flextable", {
			
	summaryTable <- data.frame(
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statN = c(1, 4, 2, 5),
		isTotal = c(FALSE, TRUE, FALSE, TRUE)
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		colVar = "TRT",
		colHeaderTotalInclude = FALSE
	)
	expect_identical(
		object = unname(unlist(ft$header$dataset)),
		expected = c("B", "A")
	)

})

test_that("A summary table with statistics in rows is correctly exported to flextable", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "row"
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("A", "n", "Mean", "B", "n", 'Mean'),
			c(NA_character_, "1", "0.34", NA_character_, "2", "0.56"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})	
	
test_that("A summary table with statistics in columns is correctly exported to flextable", {
		
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)
				
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "col"
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
	
})

test_that("A summary table with statistics in a separated column is correctly exported to flextable", {
	
	summaryTable <- data.frame(
		variable = c("A", "B"),
		n = c("1", "2"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)		
	
	# stat in the row direction, but in a separated column
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = "variable",
		statsVar = c("n", "Mean"),
		statsLayout = "rowInSepCol"
	)
	# header
	expect_identical(
		object = unname(unlist(ft$header$dataset[1:2])), 
		expected = c("variable", "Statistic")
	)
	# data
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

test_that("The label for the statistic value is correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)
	ft <- exportSummaryStatisticsTable(
		summaryTable, rowVar = "variable", 
		statsVar = "Statistic", 
		statsValueLab = "Number of subjects"
	)
	expect_match(
		object = unname(unlist(ft$header$dataset[, 2])),
		regexp = "Number of subjects.*"
	)
	
})

test_that("An error is generated if the label for the statistic value is set to the default name in a flextable summary table", {
			
	summaryTable <- data.frame(
		variable = c("A", "B"),
		Statistic = c("1", "2"),
		stringsAsFactors = FALSE
	)
	expect_error(
		exportSummaryStatisticsTable(summaryTable, rowVar = "variable", 
			statsVar = "Statistic", statsValueLab = "Statistic"
		),
		"'statsValueLab' should be different than 'Statistic'."
	)
			
})

test_that("A summary table with one statistic is correctly exported to flextable with the statistic name", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "variable", colVar = "TRT",
		statsVar = "n", 
		statsLabInclude = TRUE
	)
	expect_identical(
		object = ft$body$dataset[, 1],
		expected = c("a", "n", "b", "n")
	)
	
})

test_that("A summary table with one statistic is correctly exported to flextable without the statistic name", {
		
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)		
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "variable", colVar = "TRT",
		statsVar = "n", 
		statsLabInclude = FALSE
	)
	
	expect_identical(
		object = ft$body$dataset[, 1],
		expected = c("a", "b")
	)
	
})
	
test_that("A warning is generated if a flextable summary table contain multiple statistics but the names are specified to be not included", {
	
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		variable = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		Mean = c("0.34", "0.56"),
		stringsAsFactors = FALSE
	)		
			
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

test_that("A placeholder for empty value is correctly included by default in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = c("1", "2", "3", NA_character_),
		stringsAsFactors = FALSE
	)
	
	# by default, 'empty' value are set to '-'
	ft <- exportSummaryStatisticsTable(
		summaryTable,
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "n"
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("a", "b"),
			c("1", "2"),
			c("3", "-"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
	
})

test_that("A specified placeholder for empty value is correctly included in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		PARAM = c("a", "b", "a", "b"),
		n = c("1", "2", "3", NA_character_),
		stringsAsFactors = FALSE
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable,
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "n",
		emptyValue = "0"
	)
	expect_equal(
		object = ft$body$dataset,
		expected = data.frame(
			c("a", "b"),
			c("1", "2"),
			c("3", "0"),
			stringsAsFactors = FALSE
		),
		check.attributes = FALSE
	)
			
})

test_that("A list of summary tables is correctly exported to flextable", {
			
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

test_that("A list of summary tables with different titles is correctly exported to flextable", {
			
	summaryTables <- list(
		`PAR2` = data.frame(n = 10),
		`PAR1` = data.frame(n = 2)
	)
			
	titles <- c("PARAMETER 2", "PARAMETER 1")
	fts <- exportSummaryStatisticsTable(
		summaryTables, 
		outputType = "flextable",
		title = titles
	)
	
	for(i in seq_along(summaryTables)){
		
		expect_equal(
			fts[[!!i]]$header$dataset[1, 1],
			titles[!!i]
		)
		
	}
		
})

test_that("A warning is generated if the variable group with totals is not formatted correctly in a flextable summary table", {
			
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

test_that("A flextable summary table is correctly formatted if totals are included only for a subset of the categorical variables", {
			
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

test_that("A flextable summary table is correctly formatted if totals are included only for a subset of the categorical variables in separated rows", {
			
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

test_that("Missing values in nested row variables are correctly represented in a flextable summary table", {
			
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

test_that("A summary table with only one element in nested row variable in a row variable is correctly exported to flextable", {
			
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

test_that("Page dimension are correctly set in a flextable summary table", {
	
	summaryTable <- data.frame(n = 10)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		pageDim = c(10, 3),
		margin = 0
	)
	
	expect_equal(
		object = ft$body$colwidths,
		expected = 10
	)
			
})

test_that("Margins are correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(n = 10)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		pageDim = c(10, 3),
		margin = 1
	)
	expect_equal(
		object = ft$body$colwidths,
		expected = 8
	)
	
})

test_that("A summary table is correctly exported to flextable in landscape mode", {
			
	summaryTable <- data.frame(n = 10)	
	
	# check that width of body in landscape mode > width in portrait mode
	ftLandscape <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		landscape = TRUE
	)
	ftPortrait <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		landscape = FALSE
	)
	
	# check that width in landscape mode is > than in portrait mode
	for(part in c("body", "header", "footer"))
		expect_gt(
			object = ftLandscape[[!!part]]$colwidths, 
			expected = ftPortrait[[!!part]]$colwidths
		)
	
})

test_that("Multiple titles are correctly included in a flextable summary table", {
		
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

test_that("Footers are correctly included in a flextable summary table", {
			
	summaryTable <- data.frame(n = 10)
	
	footers <- c("Explanation 1", "Explanation 2")
	ft <- exportSummaryStatisticsTable(
		summaryTable = summaryTable,
		footer = footers
	)
	expect_identical(
		object = ft$footer$dataset[seq_along(footers), 1],
		expected = footers
	)
	
})

test_that("Colors are correctly set in a flextable summary table", {
	
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

test_that("Fontsize is correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(n = 10)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		fontsize = 25
	)
	
	expect_identical(
		object = c(ft$body$styles$text$font.size$data), 
		expected = 25
	)
			
})

test_that("Fontname is correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(n = 10)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		fontname = "Arial"
	)
	
	expect_identical(
		object = c(ft$body$styles$text$font.family$data), 
		expected = "Arial"
	)
	
})

test_that("Vertical lines are not included in a flextable summary table when requested", {
				
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		AVISIT = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		colVar = c("TRT", "AVISIT"),
		statsVar = "n",
		vline = "none"
	)
	
	expect_setequal(
		object = ft$body$styles$cells$border.width.left$data, 
		expected = 0
	)
	
})

test_that("Vertical lines are correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(
		TRT = c("A", "A", "B", "B"),
		AVISIT = c("a", "b", "a", "b"),
		n = c("1", "2", "3", "4"),
		stringsAsFactors = FALSE
	)
		
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		colVar = c("TRT", "AVISIT"),
		statsVar = "n",
		vline = "auto"
	)

	expect_setequal(
		object = ft$body$styles$cells$border.width.left$data[, c(1, 3)], 
		expected = 1
	)
			
})

test_that("Horizontal lines are not included in a flextable summary table when requested", {
			
	summaryTable <- data.frame(
		PARAM = c("a", "b"),
		n = c("1", "2"),
		Mean = c("1", "2"),
		stringsAsFactors = FALSE
	)
			
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "PARAM",
		statsVar = c("n", "Mean"),
		hline = "none"
	)
	expect_setequal(
		object = ft$body$styles$cells$border.width.top$data, 
		expected = 0
	)
	
})


test_that("Horizontal lines are correctly set in a flextable summary table", {
			
	summaryTable <- data.frame(
		PARAM = c("a", "b"),
		n = c("1", "2"),
		Mean = c("1", "2"),
		stringsAsFactors = FALSE
	)
	
	ft <- exportSummaryStatisticsTable(
		summaryTable, 
		rowVar = "PARAM",
		statsVar = c("n", "Mean"),
		hline = "auto"
	)
	expect_setequal(
		object = ft$body$styles$cells$border.width.top$data[4, ], 
		expected = 1
	)
	
})

test_that("A warning is generated if no data remain after filtering of the column totals in a flextable summary table", {
			
	data <- data.frame(
		isTotal = rep(TRUE, 2),
		n = c(1, 2)
	)
	expect_warning(
		exportSummaryStatisticsTable(data),
		regexp = "No data remain after filtering of total rows."
	)
			
})

test_that("An error is generated if a flextable summary table contains multiple values for the column total but no column variables are specified", {
			
	data <- data.frame(
		isTotal = c(FALSE, TRUE, TRUE),
		statN = c(1, 2, 3)
	)
	expect_error(
		exportSummaryStatisticsTable(data),
		regexp = "Multiple values for the header total .*"
	)
			
})

test_that("A superscript is correctly formatted in a flextable summary table", {
			
	# Example with row/col vars specification in specific cell
	xSps <- "<0.001^{*}"
	data <- data.frame(
		pValue = c("0.05", xSps, "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value", "Change from Baseline"), times = 2)
	)
	ft <- exportSummaryStatisticsTable(
		data, 
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "pValue"
	)
	
	if(packageVersion("flextable") >= "0.9.5"){
		
		ftDataChunk <- flextable::information_data_chunk(ft)
		ftDataTxtSuperscript <- subset(ftDataChunk, txt == "*")
		expect_equal(
			object = ftDataTxtSuperscript[, "vertical.align"], 
			expected = "superscript"
		)
		
	}else{
	
		idxSps <- which(ft$body$dataset == xSps)
		ftBodyCnt <- ft$body$content$content$data
		cntDataSps <- ftBodyCnt[idxSps][[1]]
		expect_equal(nrow(cntDataSps), 2)
		expect_equal(cntDataSps[, "txt"], c("<0.001", "*"))
		expect_equal(cntDataSps[, "vertical.align"], c(NA_character_, "superscript"))
		
		alignDataOther <- unlist(lapply(ftBodyCnt[-idxSps], "[", "vertical.align"))
		expect_setequal(alignDataOther, NA_character_)
		
	}
			
})

test_that("A subscript is correctly formatted in a flextable summary table", {
			
	# Example with row/col vars specification in specific cell
	xSbs <- "<0.001_{(significative)}"
	data <- data.frame(
		pValue = c("0.05", xSbs, "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value", "Change from Baseline"), times = 2)
	)
	ft <- exportSummaryStatisticsTable(
		data, 
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "pValue"
	)
	
	if(packageVersion("flextable") >= "0.9.5"){
		
		ftDataChunk <- flextable::information_data_chunk(ft)
		ftDataSubscript <- subset(ftDataChunk, txt == "(significative)")
		expect_equal(
			object = ftDataSubscript[, "vertical.align"], 
			expected = "subscript"
		)
		
	}else{
			
		idxSbs <- which(ft$body$dataset == xSbs)
		ftBodyCnt <- ft$body$content$content$data
		cntDataSbs <- ftBodyCnt[idxSbs][[1]]
		expect_equal(nrow(cntDataSbs), 2)
		expect_equal(cntDataSbs[, "txt"], c("<0.001", "(significative)"))
		expect_equal(cntDataSbs[, "vertical.align"], c(NA_character_, "subscript"))
				
		alignDataOther <- unlist(lapply(ftBodyCnt[-idxSbs], "[", "vertical.align"))
		expect_setequal(alignDataOther, NA_character_)
		
	}
			
})

test_that("A cell is correctly formatted in bold in a flextable summary table", {
			
	# Example with row/col vars specification in specific cell
	xBold <- "bold{<0.001}"
	data <- data.frame(
		pValue = c("0.05", xBold, "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value", "Change from Baseline"), times = 2)
	)
	ft <- exportSummaryStatisticsTable(
		data, 
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "pValue"
	)
	
	if(packageVersion("flextable") >= "0.9.5"){
		
		ftDataChunk <- flextable::information_data_chunk(ft)
		ftDataBold <- subset(ftDataChunk, txt == "<0.001")
		expect_true(object = ftDataBold[, "bold"])
		
	}else{
	
		isBold <- apply(
			ft$body$content$content$data, 
			2, function(x) sapply(x, `[[`, "bold")	
		)
		idxBold <- which(ft$body$dataset == xBold)
		expect_setequal(isBold[idxBold], TRUE)
		expect_setequal(isBold[-idxBold], NA)
		
	}
			
})

test_that("A cell is correctly formatted with multiple same text formatting in a flextable summary table", {
			
	# Example with row/col vars specification in specific cell
	data <- data.frame(
		pValue = c("0.05", "1", "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value^{test} of the measurement^{test2} in data", 
			"Change from Baseline"), times = 2)
	)
	ft <- exportSummaryStatisticsTable(
		data, 
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "pValue"
	)
	
	if(packageVersion("flextable") >= "0.9.5"){
		
		ftDataChunk <- flextable::information_data_chunk(ft)
		expect_equal(
			object = subset(ftDataChunk, txt == "test")[, "vertical.align"],
			expected = "superscript"
		)
		expect_equal(
			object = subset(ftDataChunk, txt == "test2")[, "vertical.align"],
			expected = "superscript"
		)
		
	}else{
	
		idxSps <- which(grepl("Actual Value", ft$body$dataset))
		ftBodyCnt <- ft$body$content$content$data
		cntDataSps <- ftBodyCnt[idxSps][[1]]
		
		expect_equal(
			object = cntDataSps[, "txt"], 
			expected = c("Actual Value", "test", " of the measurement", "test2",
				" in data")
		)
		expect_equal(
			object = cntDataSps[, "vertical.align"], 
			expected = c(NA_character_, "superscript", NA_character_, 
				"superscript", NA_character_)
		)
		
	}
	
})

test_that("A cell is correctly formatted with multiple and different text formatting in a flextable summary table", {
			
	# Example with row/col vars specification in specific cell
	x <- "^{test}bold{<0.001}"
	data <- data.frame(
		pValue = c("0.05", x, "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value", "Change from Baseline"), times = 2)
	)
	ft <- exportSummaryStatisticsTable(
		data, 
		rowVar = "PARAM", colVar = "TRT",
		statsVar = "pValue"
	)
	
	if(packageVersion("flextable") >= "0.9.5"){
		
		ftDataChunk <- flextable::information_data_chunk(ft)
		expect_equal(
			object = subset(ftDataChunk, txt == "test")[, "vertical.align"],
			expected = "superscript"
		)
		expect_true(object = subset(ftDataChunk, txt == "<0.001")[, "bold"])
		
	}else{
		
		cntData <- ft$body$content$content$data[2, 2][[1]]
		
		expect_equal(object = cntData[, "txt"], expected = c("test", "<0.001"))
		expect_equal(object = cntData[1, "vertical.align"], expected = "superscript")
		expect_true(object = cntData[2, "bold"])
		
	}
	
})

test_that("A summary table is correctly exported to a docx file", {

	data <- data.frame(
		pValue = c("0.05", "<0.001", "1", "0.89"),
		TRT = rep(c("A", "B"), each = 2),
		PARAM = rep(c("Actual Value", "Change from Baseline"), times = 2)
	)
	
	file <- tempfile(pattern = "table", fileext = ".docx")	
	expect_silent(
		ft <- exportSummaryStatisticsTable(
			data, 
			rowVar = "PARAM", colVar = "TRT",
			statsVar = "pValue",
			file = file
		)
	)
	
	# has file been created?
	expect_true(file.exists(file))
	
	# check if content is correct
	# (Note: this is based on the 'officerverse' user documentation of officer)
	doc <- officer::read_docx(file)
	docTable <- subset(docx_summary(doc), `content_type` == "table cell")
	
	docTableData <- with(
		subset(docTable, !`is_header`),
		tapply(text, list(`row_id`, `cell_id`), I)
	)
	docTableHeader <- with(
		subset(docTable, is_header), 
		tapply(text, `cell_id`, I)
	)
	colnames(docTableData) <- docTableHeader
	
	dataRef <- cbind(
		PARAM = c("Actual Value", "Change from Baseline"),
		A = c("0.05", "<0.001"),
		B = c("1", "0.89")
	)
	expect_equal(object = docTableData, expected = dataRef, check.attributes = FALSE)
	
})

test_that("A summary table is correctly exported to a docx file in landscape format", {
		
	file <- tempfile(pattern = "table", fileext = ".docx")	
	ft <- exportSummaryStatisticsTable(
		summaryTable = data.frame(n = 10),
		file = file,
		landscape = TRUE
	)
	doc <- officer::read_docx(file)
	docCnt <- officer::docx_body_xml(doc)
	expect_match(
		object = as.character(docCnt), 
		regexp = 'orient=\"landscape\"', 
		fixed = TRUE
	)
			
})

test_that("A summary table is correctly exported to a docx file in landscape format without additional empty pages", {
  
  file <- tempfile(pattern = "table", fileext = ".docx")
  ft <- exportSummaryStatisticsTable(
    summaryTable = data.frame(n = 10),
    file = file,
    landscape = TRUE
  )
  
  # check that document doesn't contain any extra paragraph (i.e. empty page)
  doc <- officer::read_docx(file)
  docCntType <- officer::docx_summary(doc)[, "content_type"]
  expect_false("paragraph" %in% docCntType)
  
})

test_that("A list of summary tables is correctly exported to docx files", {
			
	summaryTables <- list(
		`PARAM 2` = data.frame(n = 10),
		`PARAM 1` = data.frame(n = 2)
	)
			
	file <- tempfile(pattern = "table", fileext = ".docx")		
	dts <- exportSummaryStatisticsTable(
		summaryTables, 
		file = file,
		colHeaderTotalInclude = FALSE
	)
			
	fileTableOutput <- paste0(
		tools::file_path_sans_ext(file),
		"_", c("1", "2"), ".docx"
	)
	expect_true(all(file.exists(fileTableOutput)))
	
	for(iTable in seq_along(summaryTables)){

		expect_equal(
			object = {
				doc <- officer::read_docx(fileTableOutput[!!iTable])
				docTables <- subset(officer::docx_summary(doc), `content_type` == "table cell")
				docTables$text
			}, 
			expected = {
				table <- summaryTables[[!!iTable]]
				tableCnt <- c(names(summaryTables)[!!iTable], colnames(table), unlist(table))
				unname(tableCnt)
			}
		)
	}
			
})

test_that("A list of summary tables is correctly exported to a single docx file", {
			
	# Note: this functionality is currently not available 
	# in exportSummaryStatisticsTable
	# only in exportFlextableToDocx
	summaryTables <- list(
		`PARAM 2` = flextable(data.frame(n = "10")),
		`PARAM 1` = flextable(data.frame(n = "2"))
	)
	
	file <- tempfile(pattern = "table", fileext = ".docx")	
	exportFlextableToDocx(object = summaryTables, file = file)		
	
	doc <- officer::read_docx(file)
	docTables <- subset(officer::docx_summary(doc), `content_type` == "table cell")
	expect_equal(docTables$text, c("n", "10", "n", "2"))
	
})

test_that("A flextable summary table is correctly styled for a report", {
	
	summaryTable <- data.frame(n = 9)	
			
	expect_identical(
		object = exportSummaryStatisticsTable(
			summaryTable = summaryTable, 
			style = "report"
		),
		expected = exportSummaryStatisticsTable(
			summaryTable = summaryTable, 
			fontname = "Times", fontsize = 8,
			landscape = FALSE,
			pageDim = getDimPage(style = "report", margin = 0),
			colorTable = getColorPaletteTable(style = "report")
		)
	)
	
})

test_that("A flextable summary table is correctly styled for a presentation", {
			
	summaryTable <- data.frame(n = 9)	
	
	expect_identical(
		object = exportSummaryStatisticsTable(
			summaryTable = summaryTable, 
			style = "presentation"
		),
		expected = exportSummaryStatisticsTable(
			summaryTable = summaryTable, 
			fontname = "Tahoma", fontsize = 10,
			landscape = TRUE,
			pageDim = getDimPage(
				style = "presentation", margin = 0,
				landscape = FALSE
			),
			colorTable = getColorPaletteTable(style = "presentation")
		)
	)
			
})

test_that("Column headers of a summary table with identical elements across consecutive columns or rows are correctly merged", {
  
  # Example with:
  # - column that should not be merged based on previous column header:
  # [TRT1, TRT1, Placebo] and [TRT2, Placebo, Placebo]
  # - nesting of > 2 cells: Placebo (rows) and TRT1 (columns)
  summaryTable <- data.frame(
    TRT01P = factor(c("TRT1", "TRT1", "TRT1", "TRT2", "TRT2"), 
      levels = c("TRT1", "TRT2")),
    TRT02P = factor(c("TRT1", "TRT1", "Placebo", "TRT1", "TRT1"), 
        levels = c("TRT1", "Placebo")),
    TRT03P = factor(
      c("TRT1", "Placebo", "Placebo", "TRT1", "TRT2"),
      levels = c("TRT1", "TRT2", "Placebo")
    ),
    variable = "A",
    n = as.character(1:5),
    stringsAsFactors = FALSE
  )		
  
  ft <- exportSummaryStatisticsTable(
    summaryTable = summaryTable,
    statsVar = "n",
    colVar = c("TRT01P", "TRT02P", "TRT03P"),
    rowVar = "variable",
    style = "presentation"
  )
  # columns are correctly merged
  expect_equal(
    ft$header$spans$rows,
    rbind(
      c(1, 3, 0, 0, 2, 0), # TRT01P
      c(1, 2, 0, 1, 2, 0), # TRT02P
      c(1, 1, 1, 1, 1, 1) # TRT03P
    )
  )
  # rows are correctly merged:
  expect_equal(
    ft$header$spans$columns,
    cbind(c(3, 0, 0), c(1, 1, 1), c(1, 1, 1), c(1, 2, 0), c(1, 1, 1), c(1, 1, 1))
  )
  
})

test_that("Column headers of a summary table with identical elements across consecutive columns and rows are correctly merged", {
  
  summaryTable <- data.frame(
    TRT01P = c("TRT1", "TRT1"),
    TRT02P = c("TRT1", "TRT1"),
    AVISIT = factor(c("Baseline", "Week 10"), levels = c("Baseline", "Week 10")),
    n = as.character(1:2),
    stringsAsFactors = FALSE
  )		
  
  ft <- exportSummaryStatisticsTable(
    summaryTable = summaryTable,
    statsVar = "n",
    colVar = c("TRT01P", "TRT02P", "AVISIT"),
  )
  # columns are correctly merged
  expect_equal(
    ft$header$spans$rows,
    rbind(
      c(2, 0), # TRT01P
      c(2, 0), # TRT02P
      c(1, 1) # AVISIT
    )
  )
  # rows are correctly merged:
  expect_setequal(
    ft$header$spans$columns,
    c(c(2, 2), c(2, 0), c(1, 2))
  )
  
})

test_that("Column headers of a summary table are not merged if specified", {
  
  summaryTable <- data.frame(
    TRT01P = c("TRTX", "TRTX", "Placebo"),
    TRT02P = c("TRTX", "Placebo", "Placebo"),
    variable = "A",
    n = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )		
  
  ft <- exportSummaryStatisticsTable(
    summaryTable = summaryTable,
    rowVar = "variable",
    statsVar = "n",
    colVar = c("TRT01P", "TRT02P"),
    colHeaderMerge = FALSE
  )
  # columns are correctly merged
  expect_setequal(object = ft$header$spans$rows, expected = 1)
  
  # rows are correctly merged:
  expect_setequal(object = ft$header$spans$columns, expected = 1)
  
})
