context("Compute summary statistics table: col var specification")

library(plyr)
library(dplyr)

test_that("summary table is computed with column variable", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT"
		)
	)
	expect_s3_class(sumTable, "data.frame")
	expect_true("TRT" %in% colnames(sumTable))
	expect_identical(levels(sumTable$TRT), unique(data$TRT))
	
	# table with colVar = cbind of table created for each element of colVar
	statsContName <- c("statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	for(trt in unique(data$TRT)){
		expect_equal(
			object = subset(sumTable, TRT == !!trt, select = -TRT),
			expected = computeSummaryStatisticsTable(data = subset(data, TRT == !!trt), var = "AGE"),
			check.attributes = FALSE
		)
	}

})

test_that("correct order of columns", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	
	# by default columns are ordered based on alphabetical order
	expect_equal(
		levels(computeSummaryStatisticsTable(data, var = "AGE", colVar = "TRT")$TRT),
		c("A", "B")
	)
	
	# or order of levels if colVar is a factor
	dataColFact <- data
	data$TRT <- factor(data, levels = c("B", "A"))
	expect_equal(
		levels(computeSummaryStatisticsTable(data, var = "AGE", colVar = "TRT")$TRT),
		c("B", "A")
	)
			
})

test_that("column total is extracted", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT",
			colTotalInclude = TRUE
		)
	)
	expect_s3_class(sumTable, "data.frame")
	expect_true("TRT" %in% colnames(sumTable))
	expect_identical(
		levels(sumTable$TRT),
		c(unique(data$TRT), "Total")
	)
	
	# total should be the same as stats computed on entire dataset:
	expect_equal(
		subset(sumTable, TRT == "Total", select = -TRT),	
		computeSummaryStatisticsTable(data, var = "AGE"),
		check.attributes = FALSE # row.names diff
	)
	
})

test_that("Levels are specified for col var", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = factor(rep(c("A", "B"), each = 3)),
		DOSE = factor(rep(c("100", "200"), each = 3)),
		stringsAsFactors = FALSE
	)
	
	# by default, only the columns appearing in the data are included
	expect_silent(
		sumTableBase <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE")
		)
	)
	expect_equal(
		object = unique(sumTableBase[, c("TRT", "DOSE")]),
		expected = data.frame(
			TRT = factor(c("A", "B")), 
			DOSE = factor(c("100", "200"))
		),
		check.attributes = FALSE
	)
		
	## if other groups should be included, 'colVarDataLevels' can be used
			
	# different doses for all treatment and non alphabetical order:
	colVarDataLevels <- data.frame(
		TRT = factor(
			rep(c("A", "B", "C"), length.out = 5), 
			levels = c("C", "A", "B")
		), 
		DOSE = factor(
			c("100", "200", "600", "400", "100")
		)
	)
	
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE"),
			colVarDataLevels = colVarDataLevels
		)
	)
	# all combinations are included for the 'total', in correct order:
	expect_equal(
		subset(sumTable, isTotal)[, c("TRT", "DOSE")],
		colVarDataLevels[with(colVarDataLevels, order(TRT, DOSE)), ],
		check.attributes = FALSE
	)
	
	# statistics for combinations appearing in the data are identical
	# than when 'colVarDataLevels' is not specified:
	colVarInData <- unique(data[, c("DOSE", "TRT")])
	expect_equal(
		merge(colVarInData, sumTable)[, colnames(sumTableBase)],
		sumTableBase,
		check.attributes = FALSE
	)
	
	# statistics for combinations NOT appearing in the data are empty
	sumTableNotInData <- dplyr::anti_join(sumTable, colVarInData)
	statsCont <- c("statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	expect_true(all(is.na(sumTableNotInData[, statsCont])))
	expect_true(all(sumTableNotInData[, c("statN", "statm", "statPercTotalN")] == 0))
	expect_true(all(is.nan(sumTableNotInData[, c("statPercN")])))
	
	## missing groups in colVarDataLevels
	colVarDataLevels <- data.frame(
		TRT = factor(c("A", "B")),
		DOSE = c("0", "200")
	)
	expect_warning(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE"),
			colVarDataLevels = colVarDataLevels
		),
		"Some variable records are not present in the data used for variable levels."
	)
	
})

test_that("more groups in colVar in dataTotalRow than in data to summarize", {
			
	dataAll <- data.frame(
		USUBJID = rep(c(1:6), each = 2),
		TRT = rep(c("A", "B"), each = 6),
		COD = rep(c("Term1", "Term2", "Term3"), each = 4),
		stringsAsFactors = FALSE
	)
	data <- subset(dataAll, TRT == "A")
	dataTotalRow <- list(COD = dataAll)
	
	expect_silent(
		summaryTable <- computeSummaryStatisticsTable(
			data = data,
			colVar = "TRT",
			rowVar = "COD",
			rowVarTotalInclude = "COD",
			stats = getStats("n (%)"),
			dataTotalRow = dataTotalRow
		)
	)
	expect_s3_class(summaryTable, "data.frame")
	expect_identical(levels(summaryTable$TRT), c("A", "B"))
	
	summaryTableGroupOnlyInTotal <- subset(summaryTable, TRT == "B")
	expect_equal(nrow(summaryTableGroupOnlyInTotal), 1)
	expect_equal(as.character(summaryTableGroupOnlyInTotal$COD), "Total")
	expect_equal(summaryTableGroupOnlyInTotal$statN, 3)
	expect_equal(summaryTableGroupOnlyInTotal$statm, 6)
	
})
