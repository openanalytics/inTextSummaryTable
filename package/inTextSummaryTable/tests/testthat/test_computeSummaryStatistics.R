context("Test 'computeSummaryStatisticsTable' error/warning tracking")

dataLB <- ADaMDataPelican$ADLB

# dataset used for the computation of the total (in one of the scenario)
dataSL <- ADaMDataPelican$ADSL
dataSL$TRTP <- dataSL$TRT01P 

dataAE <- ADaMDataPelican$ADAE

## tests different error/warning tracking mechanisms

# in case of a summaryTable, 'var' should be specified ...
expect_error(
	computeSummaryStatistics(
		var = NULL,
		data = subset(dataLB, PARAMCD == "ALB"),
		type = "summaryTable"
	)	
)
# ... and numeric
expect_error(
	computeSummaryStatistics(
		var = "PARAM",
		data = subset(dataLB, PARAMCD == "ALB"),
		type = "summaryTable"
	)
)

test_that("No different values of a continuous variable for the same subject ID", {
			
	dataSLDupl <- dataSL[1, ]
	dataSLDupl$AGE <- dataSLDupl$AGE-1
	dataTable <- rbind(dataSLDupl, dataSL)
	vars <- "AGE"
	stats <- getStatsData(
		data = dataTable,
		type = 'all',
		var = vars
	)$AGE
	expect_error(
		summaryTable <- computeSummaryStatisticsTable(
			data = dataTable, 
			var = vars,
			colVar = "TRTP",
			stats = stats
		),
		pattern = "multiple records are available"
	)
	
})

test_that("Filtering of duplicated records for the same subject ID for continuous variable", {
			
	treats <- unique(dataSL$TRTP)
	if(length(treats) > 1){
		
		dataTreat1 <- subset(dataSL, TRTP == treats[1])
		
		dataDupl <- subset(dataTreat1, USUBJID == dataTreat1[1, "USUBJID"])
		dataDupl$TRTP <- treats[2]
		
		dataTable <- rbind(dataDupl, dataSL)
		
		vars <- "AGE"
		stats <- getStatsData(
			data = dataTable,
			type = 'all',
			var = vars
		)$AGE

		expect_message(
			summaryTableDupl <- computeSummaryStatisticsTable(
				data = dataTable, 
				var = vars,
				colVar = "TRTP",
				stats = stats,
				colTotalInclude = TRUE
			),
			regexp = "duplicated values for AGE are filtered"
		)

		summaryTableInit <- computeSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			colVar = "TRTP",
			stats = stats,
			colTotalInclude = TRUE
		)
		
		expect_identical(
			object = subset(summaryTableDupl, TRTP == "Total" & !isTotal),
			expected = subset(summaryTableInit, TRTP == "Total" & !isTotal)
		)

	}
	
})

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
