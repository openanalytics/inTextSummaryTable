context("Compute summary statistics table: data specification")

library(plyr)

test_that("No data to report", {
			
			data <- data.frame()
			expect_message(
					res <- computeSummaryStatisticsTable(data),
					"No data to report."
			)
			expect_null(res)
			
		})

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