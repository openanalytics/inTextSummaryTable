context("Export summary statistics table to flextable")

test_that("table is exported to flextable with row variables", {
			
	set.seed(123)
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

test_that("table is exported to flextable with label for row variables", {
			
	set.seed(123)
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

test_that("table is exported to flextable with row variables in a separated column", {
			
	set.seed(123)
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
				rowVarInSepCol = "PARAM"
			)
			unname(unlist(ft$header$dataset[1:2]))
		}, 
		expected = c("TRT", "PARAM")
	)
	
})

test_that("table is exported to flextable with row variables in a separated column", {
			
	set.seed(123)
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
			ft$body$styles$text$bold$data[, 1]
		},
		expected = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
	)
			
})

test_that("table is exported to flextable with total included in the header row", {
			
	set.seed(123)
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

test_that("table is exported to flextable with total included in a separated row", {
			
	set.seed(123)
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

test_that("row merging for variable group in flextable", {
			
	set.seed(123)
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

test_that("row merging for statistic in flextable", {
			
	set.seed(123)
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

