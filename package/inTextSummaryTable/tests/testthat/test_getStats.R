context("Get default set of statistics")

test_that("number of subjects is extracted", {	
		
	stat <- getStats(type = "n")
	
	# check if correct stat is specified
	statCode <- grep("stat\\w", as.character(stat$n), value = TRUE)
	expect_equal(sub("(stat\\w)", "\\1", statCode), "statN")

	# TODO
	
})

