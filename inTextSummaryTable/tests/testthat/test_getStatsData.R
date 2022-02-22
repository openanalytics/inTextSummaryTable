context("Get set of statistics from data")

library(tibble)

test_that("Count statistics are extracted by default if the data and type of statistics are not specified", {
			
	expect_identical(
		getStatsData(data.frame()),	
		getStatsData(type = "count-default")
	)
		
})

test_that("Count statistics are extracted by default if a categorical variable is specified", {
		
	data <- data.frame(group = LETTERS[seq.int(10)])
	stat <- getStatsData(data, var = "group")
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "group")
	expect_identical(
		stat$group,
		getStats(type = "count-default")
	)
			
})

test_that("All count statistics are extracted for a categorical variable if specified", {
			
	data <- data.frame(group = LETTERS[seq.int(10)])
	stat <- getStatsData(data, var = "group", type = "all")
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "group")
	expect_identical(
		stat$group,
		getStats(type = "count", x = data$value)
	)
	
})


test_that("Summary statistics are extracted by default if a continuous variable is specified", {
			
	data <- data.frame(value = rnorm(10))
	stat <- getStatsData(data, var = "value")
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "value")
	expect_identical(
		stat$value,
		getStats(type = "summary-default", x = data$value)
	)
			
})

test_that("All summary statistics are extracted for a continuous variable if specified", {
			
	data <- data.frame(value = rnorm(10))
	stat <- getStatsData(data, var = "value", type = "all")
	expect_type(stat, "list")
	expect_length(stat, 1)
	expect_named(stat, "value")
	expect_identical(
		stat$value,
		getStats(type = "summary", x = data$value)
	)
	
})

test_that("Default statistics are correctly extracted if categorical and continuous variables are specified", {
	
	data <- data.frame(
		value = rnorm(10),
		group = LETTERS[seq.int(10)]
	)
	stat <- getStatsData(data, var = c("value", "group"))
	
	expect_identical(
		stat,
		c(getStatsData(data, var = "value"), getStatsData(data, var = "group"))
	)
			
})

test_that("Statistics are correctly extracted when specified separately for categorical and continuous variables", {
			
	data <- data.frame(
		value = rnorm(10),
		group = LETTERS[seq.int(10)]
	)
	type <- c(cat = "%", cont = "Mean", cont = "Median", "n")
	stat <- getStatsData(data, var = c("value", "group"), type = type)
			
	statSep <- c(
		getStatsData(data, var = "value", type = c("Mean", "Median", "n")), 
		getStatsData(data, var = "group", type = c("%", "n"))
	)
	
	expect_identical(stat, statSep)
	
})

test_that("Parameters are correctly passed for categorical and continuous variables separately", {

	data <- data.frame(
		value = rnorm(10),
		group = LETTERS[seq.int(10)]
	)
	args <- list(
		cat = list(includeName = FALSE),
		cont = list(includeName = TRUE)
	)
			
	stat <- getStatsData(
		data, 
		var = c("value", "group"), 
		type = "n", 
		args = args
	)
			
	statSep <- c(
		getStatsData(data, var = "value", type = "n", includeName = TRUE), 
		getStatsData(data, var = "group", type = "n", includeName = FALSE)
	)
			
	expect_identical(stat, statSep)
			
})

test_that("An extra custom statistic as a function of the variable is correctly extracted", {
			
	data <- data.frame(
		value = rnorm(10)
	)
	myCustomStat <- function(x){
		bquote(mean(x)/.(length(x)))
	}
	stat <- getStatsData(
		data = data, 
		var = "value", 
		type = "Mean",
		extra = list(myCustomStat = myCustomStat)
	)
	expect_named(stat$value, c("Mean", "myCustomStat"))
	expect_equal(
		stat$value$myCustomStat, 
		myCustomStat(x = data$value)
	)
			
})

test_that("An extra custom statistic as an expression is correctly extracted", {
			
	data <- data.frame(value = rnorm(10))
	myCustomStat <- bquote(mean(x))
	stat <- getStatsData(
		data = data, 
		var = "value", 
		type = "Mean",
		extra = list(myCustomStat = myCustomStat)
	)
	expect_named(stat$value, c("Mean", "myCustomStat"))
	expect_equal(
		stat$value$myCustomStat, 
		myCustomStat
	)
			
})

test_that("General parameters are correctly passed to the generic statistic extraction function", {
			
	data <- data.frame(value = rnorm(10))
	stat <- getStatsData(
		data = data, 
		var = "value", 
		type = "Mean",
		nDecCont = 3
	)
	expect_equal(
		stat$value, 
		getStats(type = "Mean", nDecCont = 3)
	)
		
})

test_that("Statistics are correctly extracted for a numeric variable for a tibble", {
		
	data <- data.frame(value = rnorm(10))
	data <- tibble::as_tibble(data)
	typeStats <- c(cont = "Median", cat = "n")
	stats <- getStatsData(
		data = data, 
		var = "value",
		type = typeStats
	)
	expect_named(stats[["value"]], "Median")
			
})