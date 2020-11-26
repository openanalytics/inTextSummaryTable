context("Compute summary statistics table: statistics specification")

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