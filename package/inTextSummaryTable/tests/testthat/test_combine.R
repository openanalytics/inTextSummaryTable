context("Combine summary statistics tables")

library(plyr)

test_that("An error is generated if summary tables without variable names are combined", {
			
	# In case a single variable is included,
	# the variable name is not included in the table
	# so tables cannot be combined
	dataCont <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2)
	)
	summaryTableCont <- computeSummaryStatisticsTable(
		data = dataCont, 
		var = "AVAL"
	)
	dataCat <- data.frame(
		USUBJID = c("1", "2"),
		GROUP = c("A", "B")
	)
	summaryTableCat <- computeSummaryStatisticsTable(
		data = dataCat, 
		var = "GROUP"
	)
	expect_error(
		combine(summaryTableCont, summaryTableCat),
		"Summary tables cannot be combined"
	)
			
})

test_that("Summary tables with only one variable and with similar layout are correctly combined", {
			
	data <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		AVAL2 = c(2, 3)
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL2",
		varLabInclude = TRUE
	)
	summaryTableCombine <- combine(summaryTable1, summaryTable2)
	expect_s3_class(summaryTableCombine, "summaryTable")
			
	# check that content without total is correct
	expect_equal(
		subset(summaryTableCombine, !isTotal),
		rbind(
			subset(summaryTable1, !isTotal), 
			subset(summaryTable2, !isTotal)
		),
		check.attributes = FALSE
	)
			
	# only one row should be retained for the total
	expect_equal(
		subset(summaryTableCombine, isTotal),
		subset(summaryTable1, isTotal), 
		check.attributes = FALSE
	)	
	
	## check attributes
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["rowVar"]], 
		"variable"
	)
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["statsVar"]], 
		attr(summaryTable1, "summaryTable")[["statsVar"]]
	)	
	
})

test_that("Summary tables with a variable to summarize, row and column variables and with similar layout are correctly combined", {
			
	data <- data.frame(
		USUBJID = c("1", "2", "1", "2"),
		PARAM = c("a", "a", "b", "b"),
		TRT = c("A", "B", "A", "B"),
		AVAL = c(1, 2 ,3, 4),
		AVAL2 = c(4, 3 , 2, 1),
		stringsAsFactors = FALSE
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL", varLabInclude = TRUE,
		colVar = "TRT",
		rowVar = "PARAM"
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL2", varLabInclude = TRUE,
		colVar = "TRT",
		rowVar = "PARAM"
	)
	summaryTableCombine <- combine(summaryTable1, summaryTable2)
	expect_s3_class(summaryTableCombine, "summaryTable")
	
	# check that content without total is correct
	expect_equal(
		subset(summaryTableCombine, !isTotal),
		rbind.fill(subset(summaryTable1, !isTotal), 
			subset(summaryTable2, !isTotal)),
		check.attributes = FALSE
	)
	
	# only one row should be retained for the total
	expect_equal(
		subset(summaryTableCombine, isTotal),
		subset(summaryTable1, isTotal), 
		check.attributes = FALSE
	)
	
	## check attributes
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["rowVar"]], 
		c("PARAM", "variable")
	)
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["colVar"]], 
		"TRT"
	)
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["statsVar"]], 
		attr(summaryTable1, "summaryTable")[["statsVar"]]
	)	
			
})

test_that("Summary tables with a categorical and with a continuous variable are successfully combined", {
			
	dataCont <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2)
	)
	summaryTableCont <- computeSummaryStatisticsTable(
		data = dataCont, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	dataCat <- data.frame(
		USUBJID = c("1", "2"),
		GROUP = c("A", "B")
	)
	summaryTableCat <- computeSummaryStatisticsTable(
		data = dataCat, 
		var = "GROUP",
		varLabInclude = TRUE
	)
	summaryTableCombine <- combine(summaryTableCont, summaryTableCat)
	
	# check for the table with categorical variable
	# that initial content is retained
	summaryTableCCat <- subset(summaryTableCombine, !isTotal & variable == "GROUP")
	expect_equal(
		subset(summaryTableCCat, select = colnames(summaryTableCat)),
		subset(summaryTableCat, !isTotal),
		check.attributes = FALSE
	)
	# and extra columns for other table is filled with NA
	summaryTableCCatExtra <- subset(summaryTableCCat, 
		select = setdiff(colnames(summaryTableCCat), 
			colnames(summaryTableCat)
		))
	expect_true(all(is.na(summaryTableCCatExtra)))
	
	# check for the table with continuous variable
	# that initial content is retained
	summaryTableCCont <- subset(summaryTableCombine, !isTotal & variable == "AVAL")
	expect_equal(
		subset(summaryTableCCont, select = colnames(summaryTableCont)),
		subset(summaryTableCont, !isTotal),
		check.attributes = FALSE
	)
	# and extra columns for other table is filled with NA
	summaryTableCContExtra <- subset(summaryTableCCont, 
		select = setdiff(colnames(summaryTableCCont), 
			colnames(summaryTableCont)
	))
	expect_true(all(is.na(summaryTableCContExtra)))
	
	# only one row should be retained for the total
	expect_equal(
		subset(summaryTableCombine, isTotal)[, colnames(summaryTableCat)],
		subset(summaryTableCat, isTotal), 
		check.attributes = FALSE
	)	
	
	## check attributes
	expect_equal(
		attr(summaryTableCombine, "summaryTable")[["rowVar"]],
		c("variable", "variableGroup")
	)
	expect_setequal(
		attr(summaryTableCombine, "summaryTable")[["statsVar"]],
		c(
			attr(summaryTableCat, "summaryTable")[["statsVar"]],
			attr(summaryTableCont, "summaryTable")[["statsVar"]]
		)
	)
			
})

test_that("The origin of the table is correctly included in the combined table as row", {
			
	data <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		AVAL2 = c(2, 3)
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL2",
		varLabInclude = TRUE
	)
	summaryTableCombine <- combine(summaryTable1, summaryTable2,
		combineVar = "Version", combineDir = "row")
	
	expect_true("Version" %in% colnames(summaryTableCombine))
	
	# check that input table are correctly saved in correct combine var
	summaryTableCombineWthtTotal <- subset(summaryTableCombine, !isTotal)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == 1, select = -Version),
		subset(summaryTable1, !isTotal),
		check.attributes = FALSE
	)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == 2, select = -Version),
		subset(summaryTable2, !isTotal),
		check.attributes = FALSE
	)
	
	## check attributes
	expect_equal(
		attr(summaryTableCombine, "summaryTable")$rowVar,
		c("Version", "variable")
	)
			
})

test_that("The table label is correctly included in the combined table", {
			
	data <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		AVAL2 = c(2, 3)
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL2",
		varLabInclude = TRUE
	)
	summaryTableCombine <- combine(
		`old batch` = summaryTable1, 
		`new batch` = summaryTable2,
		combineVar = "Version", 
		combineDir = "row"
	)
	
	expect_true("Version" %in% colnames(summaryTableCombine))
			
	# check that input table are correctly saved in correct combine var
	summaryTableCombineWthtTotal <- subset(summaryTableCombine, !isTotal)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == "old batch", select = -Version),
		subset(summaryTable1, !isTotal),
		check.attributes = FALSE
	)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == "new batch", select = -Version),
		subset(summaryTable2, !isTotal),
		check.attributes = FALSE
	)
			
})


test_that("The origin of the table is correctly included in the combined table as column", {
			
	data <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		AVAL2 = c(2, 3)
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data, 
		var = "AVAL2",
		varLabInclude = TRUE
	)
	summaryTableCombine <- combine(summaryTable1, summaryTable2,
		combineVar = "Version", combineDir = "col")
			
	expect_true("Version" %in% colnames(summaryTableCombine))
			
	# check that input table are correctly saved in correct combine var
	summaryTableCombineWthtTotal <- subset(summaryTableCombine, !isTotal)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == 1, select = -Version),
		subset(summaryTable1, !isTotal),
		check.attributes = FALSE
	)
	expect_equal(
		subset(summaryTableCombineWthtTotal, Version == 2, select = -Version),
		subset(summaryTable2, !isTotal),
		check.attributes = FALSE
	)
			
	## check attributes
	expect_equal(
		attr(summaryTableCombine, "summaryTable")$colVar,
		"Version"
	)
			
})

test_that("The column total is discarded in the combined table if it differs between tables", {
			
	data1 <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2)
	)
	summaryTable1 <- computeSummaryStatisticsTable(
		data = data1, 
		var = "AVAL",
		varLabInclude = TRUE
	)
	data2 <- data.frame(
		USUBJID = c("1", "2", "3"),
		AVAL2 = c(1, 2, 3)
	)
	summaryTable2 <- computeSummaryStatisticsTable(
		data = data2, 
		var = "AVAL2",
		varLabInclude = TRUE
	)
	expect_warning(
		summaryTableCombine <- combine(summaryTable1, summaryTable2),
		"column totals differ"
	)
	expect_false(all(summaryTableCombine$isTotal))
	
})

test_that("The column total is discarded if the tables contain different column variables", {
			
	dataTRT <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		TRT = c("A", "B")
	)
	summaryTableTRT <- computeSummaryStatisticsTable(
		data = dataTRT, 
		var = "AVAL",
		colVar = "TRT",
		varLabInclude = TRUE
	)
	dataVISIT <- data.frame(
		USUBJID = c("1", "2"),
		AVAL = c(1, 2),
		VISIT = c("Baseline", "Week 4")
	)
	summaryTableVISIT <- computeSummaryStatisticsTable(
		data = dataVISIT, 
		var = "AVAL",
		colVar = "VISIT",
		varLabInclude = TRUE
	)
	expect_warning(
		summaryTableCombine <- combine(summaryTableTRT, summaryTableVISIT),
		"column totals are discarded"
	)
	expect_false(all(summaryTableCombine$isTotal))
			
})