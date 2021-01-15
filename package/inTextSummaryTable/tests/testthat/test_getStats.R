context("Get default set of statistics")

test_that("number of subjects is extracted", {	
		
	stat <- getStats(type = "n")
	
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "n")
	expect_type(stat$n, "language")
	
	# check if correct stat is specified
	statCode <- grep("stat\\w", as.character(stat$n), value = TRUE)
	expect_equal(sub("(stat\\w)", "\\1", statCode), "statN")

	summaryTable <- data.frame(statN = c(1, 2, NA_integer_))
	expect_equal(
		eval(expr = stat$n, envir = summaryTable),
		c("1", "2", "NA")
	)
	
})

test_that("number of records is extracted", {	
			
	stat <- getStats(type = "m")
			
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "m")
	expect_type(stat$m, "language")
	
	# check if correct stat is specified
	statCode <- grep("stat\\w", as.character(stat$m), value = TRUE)
	expect_equal(sub("(stat\\w)", "\\1", statCode), "statm")
	
	summaryTable <- data.frame(statm = c(1, 2, NA_integer_))
	expect_equal(
		eval(expr = stat$m, envir = summaryTable),
		c("1", "2", "NA")
	)
	
})

test_that("percentage of subjects is extracted", {	
			
	stat <- getStats(type = "%")
	
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "%")
	expect_type(stat$`%`, "language")
	
	# check if correct stat is specified
	statCode <- grep("stat\\w", as.character(stat$`%`), value = TRUE)
	expect_equal(sub("(stat\\w)", "\\1", statCode), "statPercN")
	
	summaryTable <- data.frame(
		statPercN = c(56.7, 0, 0.999, 1e-6, 99.99, NA_integer_)
	)
	expect_equal(
		eval(expr = stat$`%`, envir = summaryTable),
		formatPercentage(summaryTable$statPercN)
	)
	
})

test_that("percentage of records is extracted", {	
			
	stat <- getStats(type = "%m")
			
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "%m")
	expect_type(stat$`%m`, "language")
			
	# check if correct stat is specified
	statCode <- grep("stat\\w", as.character(stat$`%m`), value = TRUE)
	expect_equal(sub("(stat\\w)", "\\1", statCode), "statPercm")
		
	summaryTable <- data.frame(
		statPercm = c(56.7, 0, 0.999, 1e-6, 99.99, NA_integer_)
	)
	expect_equal(
		eval(expr = stat$`%m`, envir = summaryTable),
		formatPercentage(summaryTable$statPercm)
	)
	
})

