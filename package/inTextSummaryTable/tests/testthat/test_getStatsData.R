context("Get set of statistics from data")

test_that("variable is not specified", {
			
	expect_identical(
		getStatsData(data.frame()),	
		getStatsData(type = "count-default")
	)
		
})

test_that("default stats are extracted for a categorical variable", {
		
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

test_that("all stats are extracted for a categorical variable", {
			
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


test_that("default stats are extracted for a continuous variable", {
			
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

test_that("all stats are extracted for a continuous variable", {
			
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

test_that("default stats are extracted for both cat and cont variables", {
	
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

test_that("custom stats are specified for both cat and cont variables", {
			
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

test_that("args are passed to getStats for cat and cont variables separately", {

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

test_that("extra custom stat is specified as a function of var", {
			
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

test_that("extra custom stat is specified", {
			
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

test_that("general args are passed to getStats", {
			
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