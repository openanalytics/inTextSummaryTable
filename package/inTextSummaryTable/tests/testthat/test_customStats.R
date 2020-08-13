context("Test 'getStats'")

set.seed(123)

test_that("Compute SE", {
	
	x <- rnorm(10)		
	expect_equal(se(x = x), sd(x)/sqrt(length(x)))
	expect_equal(se(x = c(x, NA), na.rm = FALSE), NA_real_)
	expect_equal(se(x = c(x, NA), na.rm = TRUE), se(x = x, na.rm = TRUE))
			
})

test_that("Compute geometric mean", {
			
	x <- rbinom(10, size = 20, prob = 0.5)
	expect_equal(geomMean(x = x), exp(mean(log(x))))
	expect_equal(geomMean(x = c(x, NA), na.rm = FALSE), NA_real_)
	expect_equal(geomMean(x = c(x, NA), na.rm = TRUE), geomMean(x = x))
	
	expect_equal(geomMean(c(-3, -4)), NA_real_)
			
})

test_that("Compute geometric standard deviation", {
			
	x <- rbinom(10, size = 20, prob = 0.5)
	expect_equal(geomSD(x = x), exp(sd(log(x))))
	
	expect_equal(geomSD(x = c(x, NA), na.rm = FALSE), NA_real_)
	expect_equal(geomSD(x = c(x, NA), na.rm = TRUE), geomSD(x = x))
	expect_equal(geomSD(c(-3, -4)), NA_real_)
			
})

test_that("Get statistics with 'getStats'", {
			
	statsNPerc <- getStats("n (%)")
	expect_is(stats, "list")
	expect_length(stats, 1)
	expect_named(stats)
			
			expect_output(str(getStats(type = "summary")), "List of 9")
			
			namesStats <- names(getStats(type = "summary"))		
			expect_named(
					object = getStats(type = "summary"),
					expected = c("n", "Mean", "SD", "SE", "Median", "Min", "Max", "%", "m" ))
			expect_named(
					object = getStats(type = "n", includeName = FALSE),
					expected = NULL)
			expect_warning(object = getStats(type = "summary-default", includeName = FALSE))
			expect_error(object = getStats(type = "default", includeName = FALSE))
			
			library(glpgUtilityFct)
			data(ADaMDataPelican)
			getStatDefault <- getStatsData(data = ADaMDataPelican$ADSL, var = "WEIGHTBL", type = "default")
			expect_output(str(getStatDefault), "List")
			
			expect_true(grepl("roundCustomText", getStatDefault))
			sapply(names(getStatDefault), function(listArgs) 
						sapply(names(getStatDefault[[listArgs]]), function(listStats)
									expect_is(getStatDefault[[listArgs]][[listStats]], "call")))
			
			# We can still add more tests e.g. for the decimals ...
			
		})