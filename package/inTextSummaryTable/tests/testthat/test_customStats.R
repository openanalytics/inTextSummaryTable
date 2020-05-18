context("Test 'getStats'")

test_that("'getStats' function returns correct statistics", {
			
			expect_silent(getStats("n (%)"))
			expect_length(getStats("n (%)"), 1)
			
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