context("Compute summary statistics table with column variable")

library(plyr)

test_that("A summary table is correctly computed for a continuous variable by a column variable", {
			
	data <- data.frame(
		USUBJID = seq.int(7),
		AGE = seq(20, 62, length.out = 7),
		TRT = rep(c("A", "B"), times = c(3, 4)),
		stringsAsFactors = FALSE
	)
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT"
		)
	)
	expect_s3_class(sumTable, "summaryTable")
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
	
	# counts:
	expect_true(all(sumTable$statPercN %in% 100))
	expect_equal(subset(sumTable, TRT == "A" & isTotal)$statN, 3)
	expect_equal(subset(sumTable, TRT == "A")$statPercTotalN, c(3, 3))
	expect_equal(subset(sumTable, TRT == "B" & isTotal)$statN, 4)
	expect_equal(subset(sumTable, TRT == "B")$statPercTotalN, c(4, 4))	

})

test_that("Column elements are ordered alphabetically if the column variable is a character", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	expect_equal(
		levels(computeSummaryStatisticsTable(data, var = "AGE", colVar = "TRT")$TRT),
		c("A", "B")
	)
	
})

test_that("Column elements are ordered as specified if the column variable is a factor", {
	
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	data$TRT <- factor(data, levels = c("B", "A"))
	summaryTable <- computeSummaryStatisticsTable(data, var = "AGE", colVar = "TRT")
	expect_equal(
		levels(summaryTable$TRT),
		c("B", "A")
	)
			
})

test_that("The column totals are correctly computed", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	
	expect_silent(
		sumTableColTotal <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT",
			colTotalInclude = TRUE
		)
	)
	expect_identical(
		levels(sumTableColTotal$TRT),
		c(unique(data$TRT), "Total")
	)
	
	# total should be the same as stats computed on entire dataset:
	expect_equal(
		subset(sumTableColTotal, TRT == "Total", select = -TRT),	
		computeSummaryStatisticsTable(data, var = "AGE"),
		check.attributes = FALSE # row.names diff
	)
	
})
	
test_that("A label is correctly set to the column totals", {
				
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = rep(c("A", "B"), each = 3),
		stringsAsFactors = FALSE
	)
	
	sumTableColTotal <- computeSummaryStatisticsTable(
		data,
		var = "AGE",
		colVar = "TRT",
		colTotalInclude = TRUE
	)
	expect_equal(
		object = {
			sumTableColTotalLab <- computeSummaryStatisticsTable(
				data,
				var = "AGE",
				colVar = "TRT",
				colTotalInclude = TRUE, colTotalLab = "All treatments"
			)	
			subset(sumTableColTotalLab, TRT == "All treatments", select = -TRT)
		},
		expected = subset(sumTableColTotal, TRT == "Total", select = -TRT)
	)
	
})

test_that("The column totals are correctly extracted from a different dataset", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "3", "2", "3", "3"),
		AEBODSYS = c("A", "A", "A", "A", "B", "B", "B"),
		AEDECOD = c("a1", "a2", "a1", "a1", "b1", "b1", "b2"),
		TRT = c("X1", "X1", "X1", "X2", "X1", "X2", "X2"),
		stringsAsFactors = FALSE
	)
	dataTotalCol <- do.call(
		rbind, 
		replicate(2, data, simplify = FALSE)
	)
	dataTotalCol$USUBJID <- as.character(sample.int(nrow(dataTotalCol)))
		
	# full summary table
	summaryTable <- computeSummaryStatisticsTable(
		data,
		rowVar = c("AEBODSYS", "AEDECOD"),
		colVar = "TRT",
		colTotalInclude = TRUE,
		dataTotalCol = dataTotalCol
	)
	
	# counts in total column should be the same
	# as computed for the full data specified in 'dataTotalCol'
	expect_equal(
		object = subset(summaryTable, TRT == "Total", select = -TRT),
		expected = computeSummaryStatisticsTable(
			data = dataTotalCol,
			rowVar = c("AEBODSYS", "AEDECOD")
		),
		check.attributes = FALSE
	)
	
})

test_that("The column totals for row totals are correctly computed", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "3", "2", "3", "3"),
		AEBODSYS = c("A", "A", "A", "A", "B", "B", "B"),
		AEDECOD = c("a1", "a2", "a1", "a1", "b1", "b1", "b2"),
		TRT = c("X1", "X1", "X1", "X2", "X1", "X2", "X2"),
		stringsAsFactors = FALSE
	)
				
	# full summary table
	rowVar <- c("AEBODSYS", "AEDECOD")
	summaryTable <- computeSummaryStatisticsTable(
		data,
		rowVar = rowVar,
		rowVarTotalInclude = rowVar,
		colVar = "TRT",
		colTotalInclude = TRUE
	)
	
	# row total across AEDECOD
	expect_equal(
		object = subset(summaryTable, 
			subset = TRT == "Total" & AEDECOD == "Total" & AEBODSYS != "Total", 
			select = -c(TRT, AEDECOD),
		),
		expected = subset(
			computeSummaryStatisticsTable(data = data, rowVar = c("AEBODSYS")),
			subset = !isTotal
		),
		check.attributes = FALSE
	)
	
	# row total across AEBODYS		
	expect_equal(
		object = subset(summaryTable, 
			subset = TRT == "Total" & AEDECOD == "Total" & AEBODSYS == "Total", 
			select = -c(TRT, AEDECOD, AEBODSYS),
		),
		expected = subset(
			computeSummaryStatisticsTable(data = data),
			subset = !isTotal
		),
		check.attributes = FALSE
	)
	
})

test_that("The column totals for row totals are correctly computed from a different dataset", {
			
	data <- data.frame(
		USUBJID = c("1", "1", "2", "3", "2", "3", "3"),
		AEBODSYS = c("A", "A", "A", "A", "B", "B", "B"),
		AEDECOD = c("a1", "a2", "a1", "a1", "b1", "b1", "b2"),
		TRT = c("X1", "X1", "X1", "X2", "X1", "X2", "X2"),
		stringsAsFactors = FALSE
	)
	dataTotalColDummy <- do.call(
		rbind, 
		replicate(2, data, simplify = FALSE)
	)
	dataTotalColDummy$USUBJID <- as.character(sample.int(nrow(dataTotalColDummy)))
	dataTotalCol <- list(
		total = dataTotalColDummy,
		AEBODSYS = dataTotalColDummy[sample(nrow(dataTotalColDummy), 9), ],
		AEDECOD = dataTotalColDummy[sample(nrow(dataTotalColDummy), 5), ]
	)
			
	# full summary table
	rowVar <- c("AEBODSYS", "AEDECOD")
	summaryTable <- computeSummaryStatisticsTable(
		data,
		rowVar = rowVar,
		rowVarTotalInclude = rowVar,
		colVar = "TRT",
		colTotalInclude = TRUE,
		dataTotalCol = dataTotalCol
	)
	
	# total used for the percentages are based on 'dataTotalCol' with 'total' label
	summaryTableColTotal <- subset(summaryTable, TRT == "Total")
	nSubjTotalCol <- length(unique(dataTotalCol[["total"]]$USUBJID))
	expect_setequal(
		summaryTableColTotal$statPercTotalN,
		nSubjTotalCol
	)
	# total used for the column header is based on 'dataTotalCol' with 'total' label
	expect_equal(
		subset(summaryTableColTotal, isTotal)$statN,
		nSubjTotalCol
	)
			
	# counts in total column should be the same
	# as computed for the full data specified in 'dataTotalCol'
			
	# col total for the general row total
	expect_equal(
		object = subset(summaryTable, 
			TRT == "Total" & AEDECOD == "Total" & AEBODSYS == "Total", 
			select = c(-AEBODSYS, -AEDECOD, -TRT)
		),
		expected = subset(
			computeSummaryStatisticsTable(data = dataTotalCol[["total"]]),
			subset = !isTotal
		),
		check.attributes = FALSE
	)
	
	# col total across AEDECOD
	expect_equal(
		object = subset(summaryTable, 
			subset = (TRT == "Total" & AEDECOD == "Total" & AEBODSYS != "Total"), 
			select = c("statN", "statm")
		),
		expected = subset(
			computeSummaryStatisticsTable(
				data = dataTotalCol[["AEBODSYS"]], 
				rowVar = "AEBODSYS"
			),
			subset = !isTotal,
			select = c("statN", "statm")
		),
		check.attributes = FALSE
	)
	
	# col total for most nested row
	expect_equal(
		object = subset(summaryTable, 
			subset = (TRT == "Total" & AEDECOD != "Total" & AEBODSYS != "Total"), 
			select = c("statN", "statm")
		),
		expected = subset(
			computeSummaryStatisticsTable(
				data = dataTotalCol[["AEDECOD"]], 
				rowVar = "AEDECOD"
			),
			subset = !isTotal,
			select = c("statN", "statm")
		),
		check.attributes = FALSE
	)
			
})

test_that("Only column elements of the data are included by default", {
			
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
	
})

test_that("All combinations of column elements are correctly included when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = factor(rep(c("A", "B"), each = 3)),
		DOSE = factor(rep(c("100", "200"), each = 3)),
		stringsAsFactors = FALSE
	)
	
	# if colInclude0 is specified, all combinations of columns are included based on levels
	expect_silent(
		sumTableInclude0 <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE"),
			colInclude0 = TRUE
		)
	)
	
	# all combinations are used for the 'total', in correct order:
	colAllComb <- expand.grid(unique(data[, c("TRT", "DOSE")]))
	colAllComb <- colAllComb[with(colAllComb, order(TRT, DOSE)), ]
	sumTableInclude0Total <- subset(sumTableInclude0, isTotal)
	expect_equal(
		sumTableInclude0Total[, c("TRT", "DOSE")],
		colAllComb,
		check.attributes = FALSE
	)
	
	# statistics for combinations appearing in the data are identical
	# than when 'include0' is set to FALSE:
	colVarInData <- unique(data[, c("DOSE", "TRT")])
	sumTableBase <- computeSummaryStatisticsTable(
		data,
		var = "AGE",
		colVar = c("TRT", "DOSE")
	)
	expect_equal(
		merge(colVarInData, sumTableInclude0)[, colnames(sumTableBase)],
		sumTableBase,
		check.attributes = FALSE
	)
	
	# statistics for combinations NOT appearing in the data are empty
  idxNotInData <- which(
    !do.call(interaction, sumTableInclude0[, colnames(colVarInData)]) %in% 
    do.call(interaction, colVarInData)
  ) # ~ anti-join
	sumTableNotInData <- sumTableInclude0[idxNotInData, ]
	statsCont <- c("statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	expect_true(all(is.na(sumTableNotInData[, statsCont])))
	expect_true(all(sumTableNotInData[, c("statN", "statm", "statPercTotalN")] == 0))
	expect_true(all(is.nan(sumTableNotInData[, c("statPercN")])))
			
})

test_that("Specific combinations of column elements are included when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = factor(rep(c("A", "B"), each = 3)),
		DOSE = factor(rep(c("100", "200"), each = 3)),
		stringsAsFactors = FALSE
	)
	
	# sum table with combinations appearing in data
	sumTableBase <- computeSummaryStatisticsTable(
		data,
		var = "AGE",
		colVar = c("TRT", "DOSE")
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
 idxNotInData <- which(
  !do.call(interaction, sumTable[, colnames(colVarInData)]) %in% 
  do.call(interaction, colVarInData)
 ) # ~ anti-join
 sumTableNotInData <- sumTable[idxNotInData, ]
	statsCont <- c("statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	expect_true(all(is.na(sumTableNotInData[, statsCont])))
	expect_true(all(sumTableNotInData[, c("statN", "statm", "statPercTotalN")] == 0))
	expect_true(all(is.nan(sumTableNotInData[, c("statPercN")])))
	
})

test_that("A warning is generated if specified combinations of column elements are not available in the data", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		AGE = seq(20, 62, length.out = 6),
		TRT = factor(rep(c("A", "B"), each = 3)),
		DOSE = factor(rep(c("100", "200"), each = 3)),
		stringsAsFactors = FALSE
	)
			
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

test_that("Columns in the row total data but not in the data are correctly included", {
			
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
	expect_s3_class(summaryTable, "summaryTable")
	expect_identical(levels(summaryTable$TRT), c("A", "B"))
	
	summaryTableGroupOnlyInTotal <- subset(summaryTable, TRT == "B")
	expect_equal(nrow(summaryTableGroupOnlyInTotal), 1)
	expect_equal(as.character(summaryTableGroupOnlyInTotal$COD), "Total")
	expect_equal(summaryTableGroupOnlyInTotal$statN, 3)
	expect_equal(summaryTableGroupOnlyInTotal$statm, 6)
	
})

test_that("The column totals are correctly computed for a subset of the columns", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		TRT = c("A", "A", "A", "B", "B"),
		DOSE = c("100", "100", "200", "300", "400"),
		stringsAsFactors = FALSE
	)

	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE"),
			colVarTotal = "TRT"
		)
	)
	
	expect_equal(sum(sumTable$isTotal), 2)
	
	expect_equal(subset(sumTable, isTotal & TRT == "A")$statN, 3)
	expect_true(all(subset(sumTable, TRT == "A")$statPercTotalN == 3))
	
	expect_equal(subset(sumTable, isTotal & TRT == "B")$statN, 2)
	expect_true(all(subset(sumTable, TRT == "B")$statPercTotalN == 2))
	
	expect_equal(sumTable$statPercN, sumTable$statN/sumTable$statPercTotalN*100)
	
})

test_that("The column totals are correctly computed by the variable to summarize", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		SEX = c(NA_character_, c("F", "M", "F", "M")),
		TRT = c("A", "A", "A", "B", "B"),
		DOSE = c("100", "100", "200", "300", "400"),
		stringsAsFactors = FALSE
	)
	
	# by variable
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = c("AGE", "SEX"),
			colVar = c("TRT", "DOSE"),
			colVarTotal = "variable"
		)
	)
	expect_equal(sum(sumTable$isTotal), 2)
	
	expect_equal(subset(sumTable, isTotal & variable == "AGE")$statN, 5)
	expect_true(all(subset(sumTable, variable == "AGE")$statPercTotalN == 5))
	
	expect_equal(subset(sumTable, isTotal & variable == "SEX")$statN, 4)
	expect_true(all(subset(sumTable, variable == "SEX")$statPercTotalN == 4))
	
	expect_equal(sumTable$statPercN, sumTable$statN/sumTable$statPercTotalN*100)
	
})

test_that("A warning is generated if the column for the totals is not available in the data", {

	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		TRT = c("A", "A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT",
			colVarTotal = "TRT2"
		),
		"Variable.* in colVarTotal.* ignored because.*not available"
	)

})

test_that("Column percentages are correctly computed for a subset of the columns", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		TRT = c("A", "A", "A", "B", "B"),
		DOSE = c("100", "100", "200", "300", "400"),
		stringsAsFactors = FALSE
	)
	
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = c("TRT", "DOSE"),
			colVarTotalPerc = "TRT"
		)
	)
	
	# column total still computed per TRT and DOSE
	expect_equal(sum(sumTable$isTotal), 4)
	sumTableTotal <- subset(sumTable, isTotal)
	expect_equal(
		sumTableTotal[match(c("100", "200", "300", "400"), sumTableTotal$DOSE), "statN"], 
		c(2, 1, 1, 1)
	)
	
	# but percentage are computed by treatment
	expect_true(all(subset(sumTable, TRT == "A")$statPercTotalN == 3))
	expect_true(all(subset(sumTable, TRT == "B")$statPercTotalN == 2))
	expect_equal(sumTable$statPercN, sumTable$statN/sumTable$statPercTotalN*100)
	
})	

test_that("Column percentages are correctly computed by the variable to summarize", {
		
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		SEX = c(NA_character_, c("F", "M", "F", "M")),
		TRT = c("A", "A", "A", "B", "B"),
		DOSE = c("100", "100", "200", "300", "400"),
		stringsAsFactors = FALSE
	)		
	
	# by variable
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = c("AGE", "SEX"),
			colVar = c("TRT", "DOSE"),
			colVarTotalPerc = "variable"
		)
	)
	
	# column total still computed per TRT and DOSE
	expect_equal(sum(sumTable$isTotal), 4)
	
	# but percentage are computed by treatment
	expect_true(all(subset(sumTable, variable == "AGE")$statPercTotalN == 5))
	expect_true(all(subset(sumTable, variable == "SEX")$statPercTotalN == 4))
	expect_equal(sumTable$statPercN, sumTable$statN/sumTable$statPercTotalN*100)
		
})

test_that("A warning is generated if the column for the percentages is not available in the data", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		AGE = seq(20, 62, length.out = 5),
		TRT = c("A", "A", "A", "B", "B"),
		stringsAsFactors = FALSE
	)	
			
	# wrong specification
	expect_warning(
		sumTable <- computeSummaryStatisticsTable(
			data,
			var = "AGE",
			colVar = "TRT",
			colVarTotalPerc = "TRT2"
		),
		"Variable.* in colVarTotalPerc.* ignored because.*not available"
	)
	
})

