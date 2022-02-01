context("Compute summary statistics table with row variable")

test_that("A summary table is correctly computed for a continuous variable by a row variable", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		x = rnorm(n = 6),
		USUBJID = seq.int(6)
	)
	
	sumTableRowVar <- computeSummaryStatisticsTable(
		data,
		rowVar = c("parent", "child"),
		var = "x"
	)    
	expect_s3_class(sumTableRowVar, "summaryTable")
	stats <- c(
		"statN", "statm",
		"statMean",
		"statSD", "statSE",
		"statMedian", "statMin",
		"statMax",
		"statPercTotalN", "statPercN"
	)
	expect_identical(
		names(sumTableRowVar),
		c("parent", "child", "isTotal", stats)
	)
	lastRowIdx <- nrow(sumTableRowVar)
	expect_true(sumTableRowVar$isTotal[lastRowIdx])
	expect_identical(
		sumTableRowVar$statPercN[lastRowIdx],
		100
	)
	
	# check if summary statistics table with rowVar specification
	# corresponds to summary statistics computed for each unique rowVar
	rowVarUnique <- unique(data[, c("parent", "child")])
	statsToCompare <- setdiff(stats , c("statPercTotalN", "statPercN"))
	for(iRow in seq_len(nrow(rowVarUnique))){
		expect_equal(
			object = {
				dataIRow <- merge(rowVarUnique[!!iRow, ], data)
				sumTableIRow <- computeSummaryStatisticsTable(dataIRow, var = "x")
				subset(sumTableIRow, !isTotal)[, statsToCompare]
			}, 
			expected = merge(rowVarUnique[!!iRow, ], sumTableRowVar)[, statsToCompare],
			check.attributes = FALSE
		)
	}
	
})

test_that("A warning is generated if the row variable is not available", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		USUBJID = seq.int(6)
	)
			
	expect_warning(
		computeSummaryStatisticsTable(data, rowVar = "Y"),
		"Y.* in rowVar.*ignored"
	)
	
})


test_that("Row variables are labelled with the variable name by default", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		USUBJID = seq.int(6)
	)
	rowVar <- c("parent", "child")

	summaryTable <- computeSummaryStatisticsTable(data, rowVar = rowVar)
	expect_identical(
		object = attr(summaryTable, "summaryTable")$rowVarLab[rowVar],
		expected = setNames(rowVar, rowVar)
	)
	
})

test_that("The labels of the row variables are correctly extracted from a specified labels", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		USUBJID = seq.int(6)
	)
	rowVar <- c("parent", "child")
	rowVarLab <- c(parent = "Parent variable", child = "Child variable")
			
	summaryTable <- computeSummaryStatisticsTable(
		data = data, 
		rowVar = rowVar, 
		rowVarLab = rowVarLab
	)
	expect_identical(
		object = attr(summaryTable, "summaryTable")$rowVarLab[rowVar], 
		expected = rowVarLab
	)
	
})

test_that("The labels of the row variables are correctly extracted from the labels of all variables", {
			
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		USUBJID = seq.int(6)
	)
	rowVar <- c("parent", "child")

	labelVars <- c(parent = "Parent variable", child = "Child variable")
	summaryTable <- computeSummaryStatisticsTable(
		data = data, 
		rowVar = rowVar, 
		labelVars = labelVars
	)
	expect_identical(
		attr(summaryTable, "summaryTable")$rowVarLab[rowVar], 
		labelVars[rowVar]
	)
	
})

test_that("The labels of the row variables are correctly extracted from the labels of all variables", {
	
	data <- data.frame(
		parent = c("A", "A", "A", "A", "B", "B"), 
		child = c("a", "a", "a", "b", "c", "c"),
		USUBJID = seq.int(6)
	)
	rowVar <- c("parent", "child")
	summaryTable <- computeSummaryStatisticsTable(
		data = data, rowVar = rowVar, 
		rowVarLab = c(child = "Child variable")
	)
	
	# no errors if labels are not specified for all row variables
	expect_identical(
		object = attr(summaryTable, "summaryTable")$rowVarLab[rowVar], 
		expected = c(parent = "parent", child = "Child variable")
	)

})

test_that("Row elements are ordered alphabetically if the row variable is a character", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
	
	expect_silent(
		resAuto <- computeSummaryStatisticsTable(
			data,
			rowVar = "SEX",
			rowOrder = "auto"
		)
	)
	expect_identical(
		levels(resAuto$SEX),
		c("F", "M")
	)
	
})

test_that("Row elements are ordered alphabetically when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = factor(rep(c("M", "F"), times = 3), levels = c("M", "F"))
	)
	
	expect_silent(
		resAlphabet <- computeSummaryStatisticsTable(
			data,
			rowVar = "SEX",
			rowOrder = "alphabetical"
		)
	)
	expect_identical(
		levels(resAlphabet$SEX),
		c("F", "M")
	)
	
})
	
test_that("An error is generated if the order for the rows is not correctly specified", {
		
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
			
	expect_error(
		resAuto <- computeSummaryStatisticsTable(
			data = data,
			rowVar = "SEX",
			rowOrder = "test"
		),
		'.*should be one of.*auto.*alphabetical.*total.*'
	)
	
})

test_that("Row elements are ordered as specified when the row variable is a factor", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = factor(rep(c("M", "F"), times = 3), levels = c("M", "F")),
		stringsAsFactors = FALSE
	)
	expect_silent(
		resAuto <- computeSummaryStatisticsTable(
			data,
			rowVar = "SEX",
			rowOrder = "auto"
		)
	)
	expect_identical(levels(resAuto$SEX), c("M", "F"))
	
})

test_that("Row elements are correctly ordered based on the total", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	)
	
	# Total
	expect_silent(
		resTotal <- computeSummaryStatisticsTable(
			data,
			rowVar = "TRT",
			rowOrder = "total"
		)
	)
	expect_identical(
		levels(resTotal$TRT),
		c("B", "A")
	)
	
})

test_that("Row elements are correctly ordered based on the column totals", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = c("F", "F", "M", "M", "M", "M"),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	)
			
	expect_silent(
		resTotal <- computeSummaryStatisticsTable(
			data,
			rowVar = "SEX", colVar = "TRT",
			rowOrder = "total", 
		)
	)
	expect_identical(
		levels(resTotal$SEX),
		c("M", "F")
	)
	# Total across columns included internally but not returned by the function
	expect_identical(
		levels(resTotal$TRT),
		c("A", "B")
	)
			
})

test_that("Row elements are correctly ordered when the order is specified for each row variable", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = c("F", "F", "M", "M", "M", "M"),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	)
	
	# Different for each row variable
	expect_silent(
		resDoubleOrder <- computeSummaryStatisticsTable(
			data,
			rowVar = c("SEX", "TRT"),
			rowOrder = c(SEX = "auto", TRT = "total")
		)
	)      
	expect_identical(
		levels(resDoubleOrder$TRT),
		c("B", "A")
	)
	expect_identical(
		levels(resDoubleOrder$SEX),
		c("F", "M")
	)
	
})

test_that("Row elements are correctly ordered based on a specified function", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	)
	resFct <- computeSummaryStatisticsTable(
		data,
		rowVar = "TRT",
		rowOrder = function(summaryTable){
			data <- subset(summaryTable, !isTotal)
			data[order(data$statN, decreasing = TRUE), "TRT"]
		}
	)
	expect_identical(levels(resFct$TRT), c("B", "A"))
	
})

test_that("Row elements are correctly ordered when not all elements are returned by a specified function", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	)
			
	# if function is wrong, e.g. doesn't return all values in summaryTable
	# the remaining values are added anyway
	expect_silent(
		resFct <- computeSummaryStatisticsTable(
			data,
			rowVar = "TRT",
			rowOrderTotalFilterFct = function(summaryTable){
				c("")
			}
		)
	)
	expect_identical(levels(resFct$TRT), c("A", "B"))
	
})

test_that("Row elements are correctly ordered based on the total of a subset of the data", {
			
	# example order based on total would be different than based on specified subset data total
	data <- data.frame(
		USUBJID = seq.int(5),
		TRT = c("B", "B", "B", "A", "A"),
		SEX = c("M", "M", "F", "F", "F"),
		stringsAsFactors = FALSE
	)
			
	expect_silent(
		resTotal <- computeSummaryStatisticsTable(
			data = data,
			rowVar = c("TRT", "SEX"),
			rowOrder = "total",
			rowOrderTotalFilterFct = function(x) subset(x, TRT == "B")
		)
	)
	expect_identical(levels(resTotal$SEX), c("M", "F"))
			
})

test_that("The last element of the rows is correctly set when specified", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		TRT = c("Z", "B", "Other", "Y", "M"),
		catLast = "Other",
		stringsAsFactors = FALSE
	)
			
	expect_silent(
		res <- computeSummaryStatisticsTable(
			data = data,
			rowVar = "TRT",
			rowOrder = "auto",
			rowOrderCatLast = "Other"
		)
	)
	expect_identical(levels(res$TRT), c("B", "M", "Y", "Z", "Other"))
			
})

test_that("Specific combinations of row elements are included when requested", {
			
	# for example: table of lab abnormalities
	data <- data.frame(
		USUBJID = seq.int(7),
		PARAM = factor(c("A", "A", "A", "B", "B", "B", "B")),
		AVALC = factor(c(">10", ">10", ">10", "<=2", "]2, 10]", "]2, 10]", "]2, 10]")),
		stringsAsFactors = FALSE
	)
	
	avalcFact <- c("<=10", ">10", "<=2", "]2, 10]", "> 10")
	rowVarDataLevels <- data.frame(
		PARAM = c(rep("A", 2), rep("B", 3)),
		AVALC = factor(avalcFact, levels = avalcFact)
	)
	expect_silent(
		resSpecLevels <- computeSummaryStatisticsTable(
			data,
			rowVar = c("PARAM", "AVALC"),
			rowVarDataLevels = rowVarDataLevels
		)
	)
	# all levels specified in rowVarDataLevels are present:
	expect_identical(levels(resSpecLevels$AVALC), levels(rowVarDataLevels$AVALC))
	
	# categories not in the data are also present with a count of 0:
	avalcNotInData <- setdiff(avalcFact, data$AVALC)
	expect_true(all(avalcNotInData %in% resSpecLevels$AVALC))
	expect_identical(
		subset(resSpecLevels, AVALC %in% avalcNotInData)$statN,
		as.integer(rep(0, length(avalcNotInData)))
	)
	
})
	
test_that("The row totals are correctly computed", {
			
	data <- data.frame(
		USUBJID = c(1, 2, 3, 4, 5, 6, 6),
		SEX = c("M", "M", "M", "F", "F", "F", "F"),
		stringsAsFactors = FALSE
	)
			
	sumTableRowTotal <- computeSummaryStatisticsTable(
		data = data,
		rowVar = "SEX",
		rowVarTotalInclude = "SEX"
	)
	expect_true("Total" %in% sumTableRowTotal$SEX)
	sumTableTotal <- subset(sumTableRowTotal, SEX == "Total")
	expect_equal(sumTableTotal$statN, 6)
	expect_equal(sumTableTotal$statm, 7)
			
})


test_that("A warning is generated when the variable for row totals is not available", {
			
	data <- data.frame(
		USUBJID = c(1, 2, 3, 4, 5, 6, 6),
		SEX = c("M", "M", "M", "F", "F", "F", "F"),
		stringsAsFactors = FALSE
	)
			
	sumTableBase <- computeSummaryStatisticsTable(data = data, rowVar = "SEX")
	expect_warning(
		sumTableRowTotalWrong <- computeSummaryStatisticsTable(
			data = data,
			rowVar = "SEX",
			rowVarTotalInclude = TRUE
		),
		"Variable.*TRUE.* are not available"
	)
	expect_identical(sumTableRowTotalWrong, sumTableBase)
		
})

test_that("The row totals are correctly extracted in separated rows when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "SEX",
			rowVarTotalInclude = "SEX",
			rowVarTotalInSepRow = "SEX"
		)
	)
	# inclusion of row var in separated row during export step
	# currently only stored in the output
	expect_true("SEX" %in% attr(summaryTable, "summaryTable")$rowVarTotalInSepRow)
		
})

test_that("A warning is generated if the variable for row totals to be included in separated rows is not among the row total variables", {
		
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		summaryTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "SEX",
			rowVarTotalInSepRow = "SEX"
		),
		"SEX.*in rowVarTotalInSepRow are ignored.*"
	)
			
})

test_that("The row totals are correctly computed by variable", {
			
	# example where a subject: 'a' has same severity for 2 adverse events
	# so should be counted only once for the 'Mild' category
	data <- data.frame(
		USUBJID = c("a", "a", "b", "c", "c", "a"),
		ABODSYS = rep("X", 6),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		AESEV = c("Mild", "Moderate", "Mild", "Severe", "Moderate", "Mild"),
		stringsAsFactors = FALSE
	)
	
	sumTableRVBV <- computeSummaryStatisticsTable(
		data = data, 
		rowVar = c("AEDECOD", "AESEV"),
		rowVarTotalInclude = "AEDECOD",
		rowVarTotalByVar = "AESEV"
	)
	expect_true("Total" %in% sumTableRVBV$AEDECOD)
	sumTableRVBVRowTotal <- subset(sumTableRVBV, AEDECOD == "Total")
	expect_equal(as.character(sumTableRVBVRowTotal$AESEV), c("Mild", "Moderate", "Severe"))
	# subject 'a' is only counted once in the 'Mild' category
	expect_equal(subset(sumTableRVBVRowTotal, AESEV == "Mild")$statN, 2)
	# but 3 records are reported
	expect_equal(subset(sumTableRVBVRowTotal, AESEV == "Mild")$statm, 3)
	
})

test_that("The row totals are correctly computed by variable for only a subset of the variable to summarize", {
			
	# example where a subject: 'a' has same severity for 2 adverse events
	# so should be counted only once for the 'Mild' category
	data <- data.frame(
		USUBJID = c("a", "a", "b", "c", "c", "a"),
		ABODSYS = rep("X", 6),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		AESEV = c("Mild", "Moderate", "Mild", "Severe", "Moderate", "Mild"),
		stringsAsFactors = FALSE
	)
	
	expect_silent(
		sumTableRVBVSpec <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = c("ABODSYS", "AEDECOD", "AESEV"),
			rowVarTotalInclude = c("ABODSYS", "AEDECOD"),
			rowVarTotalByVar = c(AEDECOD = "AESEV")
		)
	)
	# if rowVarTotalByVar is not specified, only one total row is included
	sumTableTotalRowVar1 <- subset(sumTableRVBVSpec, ABODSYS == "Total")
	expect_length(nrow(sumTableTotalRowVar1), 1)
	
	# otherwise, total row(s) for each element in rowVarTotalByVar
	sumTableTotalRowVar2 <- subset(sumTableRVBVSpec, ABODSYS == "X" & AEDECOD == "Total")
	
	# should match stats based on table on ADECOD
	sumTableRVBV <- computeSummaryStatisticsTable(
		data = data, 
		rowVar = c("AEDECOD", "AESEV"),
		rowVarTotalInclude = "AEDECOD",
		rowVarTotalByVar = "AESEV"
	)
	sumTableRVBVRowTotal <- subset(sumTableRVBV, AEDECOD == "Total")
	expect_equal(
		subset(sumTableTotalRowVar2, select = -ABODSYS), 
		sumTableRVBVRowTotal,
		check.attributes = FALSE # row names might differ
	)

})

test_that("A warning is generated if the variable to compute row totals by is not available", {
			
	# example where a subject: 'a' has same severity for 2 adverse events
	# so should be counted only once for the 'Mild' category
	data <- data.frame(
		USUBJID = c("a", "a", "b", "c", "c", "a"),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		summaryTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "AEDECOD",
			rowVarTotalInclude = "AEDECOD",
			rowVarTotalByVar = "a"
		),
		".*rowVarTotalByVar.*ignored"
	)
	
})

test_that("The row percentages are correctly computed by a row variable", {
	
	data <- data.frame(
		USUBJID = c("a", "b", "c", "d", "a", "b"),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	sumTableRVTPerc <- computeSummaryStatisticsTable(
		data = data, 
		rowVar = "AEDECOD", 
		rowVarTotalPerc = "AEDECOD"
	)
	# sum percentage by rowVar == 100
	expect_equal(subset(sumTableRVTPerc, AEDECOD == "A")$statPercTotalN, 4)
	expect_equal(subset(sumTableRVTPerc, AEDECOD == "B")$statPercTotalN, 2)
	expect_equal(
		unique(as.numeric(na.omit(sumTableRVTPerc$statPercN))),
		100
	)
	
})

test_that("A warning is generated if the variable to compute row percentages is not available", {
			
	data <- data.frame(
		USUBJID = c("a", "b", "c", "d", "a", "b"),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
	
	# variable not in rowVar
	expect_warning(
		summaryTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "AEDECOD",
			rowVarTotalPerc = "a"
		),
		"rowVarTotalPerc.*ignored.*not available in: rowVar"
	)
	
})
	
test_that("The row percentages are correctly computed by the variable to summarize", {
				
	data <- data.frame(
		USUBJID = c("a", "b", "c", "d", "a", "b"),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		AESEV = c("Mild", "Moderate", "Moderate", "Severe", "Moderate", "Mild"),
		AESEV2 = c("Mild", "Moderate", NA_character_, NA_character_, "Moderate", "Mild"),
		stringsAsFactors = FALSE
	)

	expect_silent(
		sumTableRVTPerc <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "AEDECOD",
			var = c("AESEV", "AESEV2"), 
			rowVarTotalPerc = "variable",
			varLabInclude = TRUE # because 'variable' not included if only one variable
		)
	)
	expect_equal(
		unique(subset(sumTableRVTPerc, variable == "AESEV")$statPercTotalN), 
		4
	)
	expect_equal(
		unique(subset(sumTableRVTPerc, variable == "AESEV2")$statPercTotalN), 
		2
	)
	
})

test_that("A warning is generated if row percentages should be computed by the variable to summarize but the variable is not included", {
		
	data <- data.frame(
		USUBJID = c("a", "b", "c", "d", "a", "b"),
		AEDECOD = c("A", "A", "A", "A", "B", "B"),
		AESEV = c("Mild", "Moderate", "Moderate", "Severe", "Moderate", "Mild"),
		AESEV2 = c("Mild", "Moderate", NA_character_, NA_character_, "Moderate", "Mild"),
		stringsAsFactors = FALSE
	)
	
	# variable not included in summary table:
	expect_warning(
		computeSummaryStatisticsTable(
			data = data, 
			rowVar = "AEDECOD",
			var = "AESEV", 
			rowVarTotalPerc = "variable"
		),
		"Percentages cannot be computed by.*variable.* because variable not included"
	)
	
})

