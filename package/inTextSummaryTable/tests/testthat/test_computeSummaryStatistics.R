context("Test 'computeSummaryStatisticsTable' error/warning tracking")

dataLB <- ADaMDataPelican$ADLB

# dataset used for the computation of the total (in one of the scenario)
dataSL <- ADaMDataPelican$ADSL
dataSL$TRTP <- dataSL$TRT01P 

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
