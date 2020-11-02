context("Creation of summary statistics table")

test_that("summary statistics table is created with only data specified", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
			
	# no variable specified: a count table is created
	sumNoVar <- computeSummaryStatisticsTable(dataCont)
	expect_s3_class(sumNoVar, "data.frame")
	expect_named(sumNoVar, c("isTotal", "statN", "statm", "statPercTotalN", "statPercN"), ignore.order = TRUE)
	expect_equal(
		unname(unlist(subset(sumNoVar, isTotal)[1, c("statN", "statm", "statPercTotalN", "statPercN")])),
		c(5, 5, 5, 100)
	)
	expect_equal(subset(sumNoVar, isTotal)[, -1], subset(sumNoVar, !isTotal)[, -1], check.attributes = FALSE)
			
})

test_that("summary statistics table is created with only data/continuous var specification", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
			
	sumTableCont <- computeSummaryStatisticsTable(data = dataCont, var = "x")
	expect_s3_class(sumTableCont, "data.frame")
	varsCont <- c(
		"statN", "statm", "statMean", "statSD", "statSE", "statMedian",
		"statMin", "statMax", "statPercTotalN", "statPercN"
	)
	expect_named(sumTableCont, c("isTotal", varsCont), ignore.order = TRUE)
	expect_equal(sumTableCont$isTotal, c(FALSE, TRUE))

	# stats computed via 'computeSummaryStatistics':
	sumTableContVar <- subset(sumTableCont, !isTotal)
	statsVar <- computeSummaryStatistics(dataCont, var = "x")
	expect_equal(sumTableContVar[, colnames(statsVar)], statsVar)
	
	# extra total statistics are added
	expect_equal(sumTableContVar$statPercTotalN, 5)
	expect_equal(sumTableContVar$statPercN, 4/5*100)
	
})

test_that("summary statistics table is created with only data/categorical var specification", {
			
	dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = seq.int(5))
			
	sumTableCat <- computeSummaryStatisticsTable(data = dataCat, var = "x")
	expect_s3_class(sumTableCat, "data.frame")
	varsCat <- c("statN", "statm", "statPercTotalN", "statPercN")
	expect_named(sumTableCat, c("variableGroup", "isTotal", varsCat), ignore.order = TRUE)
	expect_equal(sumTableCat$isTotal, c(FALSE, TRUE))
			
	# stats computed via 'computeSummaryStatistics':
	sumTableCatVar <- subset(sumTableCat, !isTotal)
	statsVar <- computeSummaryStatistics(dataCat, var = "x")
	expect_equal(sumTableCatVar[, colnames(statsVar)], statsVar)
			
	# extra total statistics are added
	expect_equal(sumTableCatVar$statPercTotalN, c(5, 5))
	expect_equal(sumTableCatVar$statPercN, with(sumTableCatVar, statN/statPercTotalN*100))
	
})

#TODO:
#test_that("summary statistics table is created with row variables specification", {
#	
#	data <- data.frame(
#		parent = c("A", "A", "A", "A", "B", "B"), 
#		child = c("a", "a", "a", "b", "c", "c"),
#		x = rnorm(n = 6),
#		USUBJID = seq.int(6)
#	)
#	sumTableRowVar <- computeSummaryStatisticsTable(data, rowVar = c("parent", "child"), var = "x")
#	
#	expect_named()
#	
#})


test_that("More columns in dataTotalRow than in data to summarize", {
		
	treats <- unique(dataAE$TRTA)
	dataTable <- subset(dataAE, !TRTA %in% treats[1])
	
	dataTotalRow <- list(AEDECOD = {
		ddply(dataAE, c("USUBJID", "TRTA"), function(x){
			x[which.max(x$AESEVN), ]
		})
	})
	
	expect_silent(
		getSummaryStatisticsTable(
			data = dataTable,
			colVar = "TRTA",
			rowVar = c("AEDECOD", "AESEV"),
			rowVarInSepCol = "AESEV",
			rowVarTotalInclude = "AEDECOD",
			stats = getStats("n (%)"),
			dataTotalRow = dataTotalRow,
			rowVarTotalByVar = "AESEV"
		)
	)
			
})

test_that("Custom set of statistics per variable", {

	# variable to summarize
	stats <- list(
		AVAL = getStats(c("n", "mean (se)")), 
		CHG = getStats(c("n", "Median", "mean (se)"))
	)
	summaryTable <- computeSummaryStatisticsTable(
		var = c("AVAL", "CHG"),
		data = subset(dataLB, PARAMCD == "ALB" & AN01FL == "Y"),
		colVar = c("TRTP", "AVISIT"),
		stats = stats
	)
	expect_identical(
		attr(summaryTable, "summaryTable")$statsVar,
		c("n", "Median", "Mean (SE)"),
		label = "Order of statistics is as specified by variable"
	)
	
	# extra variable
	dataTable <- subset(dataLB, PARAMCD %in% c("ALB", "ALP") & AN01FL == "Y")
	stats <- list(
		AVAL = list(
			ALB = getStats("n"), 
			ALP = getStats(c("n", "mean (se)"))
		), 
		CHG = list(
			ALB = getStats(c("n", "Median", "mean (se)")),
			ALP = getStats(c("n", "Median", "mean (se)"))
		)
	)
	summaryTable <- computeSummaryStatisticsTable(
		rowVar = "PARAMCD",
		var = c("AVAL", "CHG"),
		data = dataTable,
		colVar = c("TRTP", "AVISIT"),
		stats = stats, statsVarBy = "PARAMCD"
	)
	expect_identical(
		attr(summaryTable, "summaryTable")$statsVar,
		c("n", "Median", "Mean (SE)"),
		label = "Order of statistics is as specified by parameter and variable"
	)
	
})

test_that("Order of summary statistic variable is correct", {
		
	# If variable is a character, levels should be sorted in alphabetical order
	dataSL$SEX <- as.character(dataSL$SEX)
	tableCharac <- computeSummaryStatisticsTable(data = dataSL, var = "SEX")
	expect_identical(object = levels(tableCharac$variableGroup), expected = sort(unique(dataSL$SEX)))
	
	# If variable is a factor, levels order should be retained
	dataSL$SEX <- factor(dataSL$SEX, levels = rev(sort(unique(dataSL$SEX))))
	tableFactor <- computeSummaryStatisticsTable(data = dataSL, var = "SEX")
	expect_identical(object = levels(tableFactor$variableGroup), expected = levels(dataSL$SEX))
	
	# even if not present for all colVar
	dataSL$TRT01P <- factor(dataSL$TRT01P)
	dataSL$SEX <- factor(dataSL$SEX, levels = rev(sort(unique(dataSL$SEX))))
	# subset to retain:
	# only last element of SEX for first TRT01P (will be computed first)
	# the first element of SEX for the other TRT01P
	# to check that order of SEX is computed upfront grouping by TRT01P
	dataSLSubset <- subset(dataSL, (
			TRT01P == head(levels(dataSL$TRT01P), 1) &
			SEX %in% tail(levels(dataSL$SEX), 1)
		) | 
		!TRT01P == head(levels(dataSL$TRT01P), 1) &
		!SEX %in% tail(levels(dataSL$SEX), 1)
	)
#	with(dataSLSubset, table(TRT01P, SEX))
	tableFactorNotComplete <- computeSummaryStatisticsTable(data = dataSLSubset, var = "SEX")
	expect_identical(object = levels(tableFactorNotComplete$variableGroup), expected = levels(dataSLSubset$SEX))
	
	# even if the var is character
	dataSLSubset$SEX <- as.character(dataSLSubset$SEX)
	tableCharacNotComplete <- computeSummaryStatisticsTable(data = dataSLSubset, var = "SEX")
	expect_identical(object = levels(tableCharacNotComplete$variableGroup), expected = sort(unique(dataSLSubset$SEX)))
	
})

test_that("Variable name is included when needed", {
			
	# no variable:
	descTableNoVar <- computeSummaryStatisticsTable(data = dataSL)
	expect_false("variable" %in% colnames(descTableNoVar))	
	expect_warning(
		descTableNoVar <- computeSummaryStatisticsTable(data = dataSL, varLabInclude = TRUE),
		regexp = "Variable label is not included"
	)
	
	# one variable
	descTableOneVar <- computeSummaryStatisticsTable(data = dataSL, var = "AGE")
	expect_false("variable" %in% colnames(descTableOneVar))		
	descTableOneVarWithLabel <- computeSummaryStatisticsTable(
		data = dataSL, var = "AGE", varLabInclude = TRUE
	)
	expect_true("variable" %in% colnames(descTableOneVarWithLabel))		
	
	# > 1 variable
	descTableMoreOneVar <- computeSummaryStatisticsTable(data = dataSL, var = c("AGE", "SEX"))
	expect_true("variable" %in% colnames(descTableMoreOneVar))		
	expect_warning({
		descTableMoreOneVarWithLabel <- computeSummaryStatisticsTable(
			data = dataSL, var = c("AGE", "SEX"), varLabInclude = FALSE
		)
		},
		regexp = "Variable label is included"
	)
	expect_true("variable" %in% colnames(descTableMoreOneVarWithLabel))
	
})
