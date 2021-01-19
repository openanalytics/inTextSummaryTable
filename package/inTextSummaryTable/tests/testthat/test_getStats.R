context("Get default set of statistics")

test_that("output format of base stats is correct", {	
			
	typeBase <- c("n", "m", "%", "%m", "Mean", "SD", "Median", "SE", "Min", "Max")
	statBase <- c(n = "statN", `%` = "statPercN", `%m` = "statPercm")
	statBase <- sapply(typeBase, function(type){
		if(type %in% names(statBase)){
			statBase[[type]]
		}else	paste0("stat", type)
	})
	
	for(type in typeBase){
				
		expect_type(getStats(type = !!type), "list")
		expect_length(getStats(type = !!type), 1)
		expect_equal(names(getStats(type = !!type)), !!type)
		expect_type(getStats(type = !!type)[[!!type]], "language")
				
		# check if correct stat is specified
		expect_equal({
			stat <- getStats(type = !!type)
			statCode <- grep("stat\\w", as.character(stat[[!!type]]), value = TRUE)
			sub("(stat\\w)", "\\1", statCode)
			}, 
			statBase[[!!type]]
		)
		
	}
	
})

test_that("count/summary base statistics are formatted correctly", {	
			
	typeBase <- c("n", "m", "Mean", "SD", "Median", "SE", "Min", "Max")
	statBase <- c(n = "statN", `%` = "statPercN", `%m` = "statPercm")
	statBase <- sapply(typeBase, function(type){
		if(type %in% names(statBase)){
			statBase[[type]]
		}else	paste0("stat", type)
	})
	for(type in names(statBase)){
		
		expect_equal({
				summaryTable <- setNames(
					data.frame(c(1, 2, NA_integer_)),
					statBase[!!type]
				)
				stat <- getStats(type = !!type)
				eval(expr = stat[[!!type]], envir = summaryTable)
			},
			formatC(x = summaryTable[[statBase[!!type]]])
		)
		
	}
})

test_that("percentage of subjects/records are formatted correctly", {	
			
	statPerc <- c(`%` = "statPercN", `%m` = "statPercm")
	for(type in names(statPerc)){
		
		expect_equal({
			summaryTable <- setNames(
				data.frame(c(56.7, 0, 0.999, 1e-6, 99.99, NA_integer_)),
				statPerc[!!type]
			)
			stat <- getStats(type = !!type)
			eval(expr = stat[[!!type]], envir = summaryTable)
			},
			formatPercentage(x = summaryTable[[statPerc[!!type]]])
		)
	}
	
})

test_that("number of subjects is formatted with specified number of decimals", {
	
	stat <- getStats(type = "n", nDecN = 1)
	summaryTable <- data.frame(statN = c(13, 100.56))
	expect_equal(
		eval(expr = stat$n, envir = summaryTable),
		c("13.0", "100.6")
	)
			
})

test_that("number of records is formatted with specified number of decimals", {
			
	stat <- getStats(type = "m", nDecN = 2)
	summaryTable <- data.frame(statm = c(13, 100.567))
	expect_equal(
		eval(expr = stat$m, envir = summaryTable),
		c("13.00", "100.57")
	)
			
})

test_that("mean is formatted with specified number of decimals", {
			
	stat <- getStats(type = "Mean", nDecCont = 4)
	summaryTable <- data.frame(statMean = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$Mean, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("median is formatted with specified number of decimals", {
			
	stat <- getStats(type = "Median", nDecCont = 4)
	summaryTable <- data.frame(statMedian = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$Median, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("standard deviation is formatted with specified number of decimals", {
			
	stat <- getStats(type = "SD", nDecCont = 4)
	summaryTable <- data.frame(statMedian = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$Median, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("standard error is formatted with specified number of decimals", {
			
	stat <- getStats(type = "SE", nDecCont = 4)
	summaryTable <- data.frame(statSE = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$SE, envir = summaryTable),
		c("1098.000000", "100.567568")
	)
			
})

test_that("minimum is formatted with specified number of decimals", {
			
	stat <- getStats(type = "Min", nDecCont = 4)
	summaryTable <- data.frame(statMin = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$Min, envir = summaryTable),
		c("1098.0000", "100.5676")
	)
			
})

test_that("maximum is formatted with specified number of decimals", {
			
	stat <- getStats(type = "Max", nDecCont = 4)
	summaryTable <- data.frame(statMax = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$Max, envir = summaryTable),
		c("1098.0000", "100.5676")
	)
			
})

test_that("default set of summary stats is extracted", {
			
	statType <- c("statN", "statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	type <- ifelse(statType == "statN", "n", sub("stat", "", statType))

	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
	
	expect_identical(
		getStats(type = "summary-default"),		
		getStats(type = type)
	)
			
})

test_that("entire set of summary stats is extracted", {
			
	statType <- c("statN", "statMean", "statSD", "statSE", "statMedian", 
		"statMin", "statMax", "statPercN", "statm")
	type <- sapply(statType, sub, pattern = "stat", replacement = "")
	type["statN"] <- "n"
	type["statPercN"] <- "%"
			
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	expect_identical(
		getStats(type = "summary"),		
		getStats(type = type)
	)
			
})

test_that("default set of count stats is extracted", {
			
	statType <- c("statN", "statPercN")
	type <- c(statN = "n", statm = "%")
			
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	expect_identical(
		getStats(type = "count-default"),		
		getStats(type = type)
	)
			
})

test_that("entire set of count stats is extracted", {
			
	statType <- c("statN", "statPercN", "statm")
	type <- c(statN = "n", statm = "%", statm = "m")
			
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	expect_identical(
		getStats(type = "count"),		
		getStats(type = type)
	)
			
})




