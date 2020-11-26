context("Compute summary statistics table: col var specification")

test_that("Summary statistics table is created with col variable specification", {
			
			data <- data.frame(
					USUBJID = seq.int(6),
					SEX = rep(c("M", "F"), times = 3),
					AGE = seq(20, 62, length.out = 6),
					TRT = rep(c("A", "B"), each = 3),
					stringsAsFactors = FALSE
			)
			expect_silent(
					res <- computeSummaryStatisticsTable(
							data,
							var = "AGE",
							colVar = "TRT"
					)
			)
			expect_s3_class(res, "data.frame")
			expect_identical(res$isTotal, c(FALSE, TRUE, FALSE, TRUE))
			expect_identical(levels(res$TRT), unique(data$TRT))
			expect_true("TRT" %in% colnames(res))      
			
		})

test_that("Summary statistics table is created with col variable and total", {
			
			data <- data.frame(
					USUBJID = seq.int(6),
					SEX = rep(c("M", "F"), times = 3),
					AGE = seq(20, 62, length.out = 6),
					TRT = rep(c("A", "B"), each = 3),
					stringsAsFactors = FALSE
			)
			expect_silent(
					res <- computeSummaryStatisticsTable(
							data,
							var = "AGE",
							colVar = "TRT",
							colTotalInclude = TRUE
					)
			)
			expect_s3_class(res, "data.frame")
			expect_true("TRT" %in% colnames(res))
			expect_identical(
					levels(res$TRT),
					c(unique(data$TRT), "Total")
			)
			
		})


test_that("More groups in colVar in dataTotalRow than in data to summarize", {
			
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
