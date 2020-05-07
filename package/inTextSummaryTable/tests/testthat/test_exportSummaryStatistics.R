context("Test 'exportSummaryStatisticsTable' error/warning tracking")

dataAE <- ADaMDataPelican$ADAE

dataAE <-  subset(dataAE, FASFL == "Y" & TRTEMFL == "Y")
dataAEInterest <- subset(dataAE, AESOC %in% c("Infections and infestations", "General disorders and administration site conditions", "Ear and labyrinth disorders"))

test_that("exportSummaryStatisticsTable on subsetted data", {
			
	summaryTable <- computeSummaryStatisticsTable(
		data = dataAEInterest,
		rowVar = "AEDECOD",
		colVar = "TRTA",
		stats = getStats("n (%)"),
		labelVars = labelVars
	)
	
	summaryTableTotal <- subset(summaryTable, isTotal)
	
	# no column var specified: error
	expect_error(
		exportSummaryStatisticsTable(summaryTableTotal),
		regexp = "Multiple values for the header total"
	)
	
	# column var specified: empty df after filtering of total rows:
	attr(summaryTableTotal, "summaryTable") <- attr(summaryTable, "summaryTable")
	expect_message(
		exportSummaryStatisticsTable(summaryTableTotal),
		regexp = "No data remain after filtering of total rows."
	)
	
})
