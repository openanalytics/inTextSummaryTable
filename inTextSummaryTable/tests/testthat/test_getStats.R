context("Get set of statistics")

library(clinUtils)

test_that("The output format of formatted base and combined statistics is correct", {	
			
	type <- c(
		"n", "m", "%", "%m", 
		"Mean", "SD", "Median", "SE", "Min", "Max",
		"median (range)", "median\n(range)", "mean (se)", "mean (range)", 
		"n (%)", "n/N (%)", "m (%)"
	)
	statName <- sapply(type, identity)
	statName[grep("^median|mean", type)] <- clinUtils::simpleCap(statName[grep("^median|mean", type)])
	statName["mean (se)"] <- "Mean (SE)"
	
	for(type in type){
				
		expect_type(getStats(type = !!type), "list")
		expect_length(getStats(type = !!type), 1)
		expect_equal(names(getStats(type = !!type)), statName[[!!type]])
		expect_type(getStats(type = !!type)[[ statName[[!!type]]]], "language")
		
	}
	
})

test_that("Base count and summary statistics are correctly formatted", {	
			
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

test_that("Percentage of subjects and records are formatted correctly", {	
			
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

test_that("The number of subjects is correctly formatted with specified number of decimals as number", {
	
	stat <- getStats(type = "n", nDecN = 1)
	summaryTable <- data.frame(statN = c(13, 100.56))
	expect_equal(
		eval(expr = stat$n, envir = summaryTable),
		c("13.0", "100.6")
	)
			
})

test_that("The number of records is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "m", nDecN = 2)
	summaryTable <- data.frame(statm = c(13, 100.567))
	expect_equal(
		eval(expr = stat$m, envir = summaryTable),
		c("13.00", "100.57")
	)
			
})

test_that("The mean is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "Mean", nDecCont = 4)
	summaryTable <- data.frame(statMean = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$Mean, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("The mean is correctly formatted with specified number of decimals as function", {
			
	stat <- getStats(
		type = "Mean", 
		x = seq.int(10),
		nDecCont = function(x) 2
	)
	
	summaryTable <- data.frame(statMean = c(1098, 100.567567))
		
	expect_equal(
		eval(expr = stat$Mean, envir = summaryTable),
		c("1098.000", "100.568")
	)
			
})

test_that("The median is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "Median", nDecCont = 4)
	summaryTable <- data.frame(statMedian = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$Median, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("The standard deviation is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "SD", nDecCont = 4)
	summaryTable <- data.frame(statSD = c(1098, 100.567567))
	expect_equal(
		eval(expr = stat$SD, envir = summaryTable),
		c("1098.00000", "100.56757")
	)
			
})

test_that("The standard error is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "SE", nDecCont = 4)
	summaryTable <- data.frame(statSE = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$SE, envir = summaryTable),
		c("1098.000000", "100.567568")
	)
			
})

test_that("The minimum is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "Min", nDecCont = 4)
	summaryTable <- data.frame(statMin = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$Min, envir = summaryTable),
		c("1098.0000", "100.5676")
	)
			
})

test_that("The maximum is correctly formatted with specified number of decimals as number", {
			
	stat <- getStats(type = "Max", nDecCont = 4)
	summaryTable <- data.frame(statMax = c(1098, 100.56756767))
	expect_equal(
		eval(expr = stat$Max, envir = summaryTable),
		c("1098.0000", "100.5676")
	)
			
})

test_that("The default set of summary statistics is correctly extracted", {
			
	statType <- c("statN", "statMean", "statSD", "statSE", "statMedian", "statMin", "statMax")
	type <- ifelse(statType == "statN", "n", sub("stat", "", statType))
	
	expect_identical(
		getStats(type = "summary-default"),		
		getStats(type = type)
	)
			
})

test_that("The entire set of summary statistics is correctly extracted", {
			
	statType <- c("statN", "statMean", "statSD", "statSE", "statMedian", 
		"statMin", "statMax", "statPercN", "statm")
	type <- sapply(statType, sub, pattern = "stat", replacement = "")
	type["statN"] <- "n"
	type["statPercN"] <- "%"
			
	expect_identical(
		getStats(type = "summary"),		
		getStats(type = type)
	)
			
})

test_that("The default set of count statistics is correctly extracted", {
			
	statType <- c("statN", "statPercN")
	type <- c(statN = "n", statm = "%")
			
	expect_identical(
		getStats(type = "count-default"),		
		getStats(type = type)
	)
			
})

test_that("The entire set of count statistics is correctly extracted", {
			
	statType <- c("statN", "statPercN", "statm")
	type <- c(statN = "n", statm = "%", statm = "m")
			
	expect_identical(
		getStats(type = "count"),		
		getStats(type = type)
	)
			
})

test_that("The median (range) is correctly extracted", {

	stat <- getStats(type = "median (range)")
	
	statType <- c("statMin", "statMax", "statMedian")
	type <- sub("stat", "", statType)
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
	
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable))
	expect_equal(
		statValue,
		with(statBaseValue, paste0(Median, " (", Min, ",", Max, ")"))
	)
			
})

test_that("The median\n(range) is correctly extracted", {
			
	stat <- getStats(type = "median\n(range)")
			
	statType <- c("statMin", "statMax", "statMedian")
	type <- sub("stat", "", statType)
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable))
	expect_equal(
		statValue,
		with(statBaseValue, paste0(Median, "\n(", Min, ",", Max, ")"))
	)
			
})

test_that("The mean (se) is correctly extracted", {
			
	stat <- getStats(type = "mean (se)")
			
	statType <- c("statMean", "statSE")
	type <- sub("stat", "", statType)
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable))
	expect_equal(
		statValue,
		with(statBaseValue, paste0(Mean, " (", SE, ")"))
	)
	
})

test_that("The mean (range) is correctly extracted", {
			
	stat <- getStats(type = "mean (range)")
			
	statType <- c("statMean", "statMin", "statMax")
	type <- sub("stat", "", statType)
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
	
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable))
	expect_equal(
		statValue,
		with(statBaseValue, paste0(Mean, " (", Min, ",", Max, ")"))
	)
	
})

test_that("The n (%) is correctly extracted", {
			
	stat <- getStats(type = "n (%)")
			
	statType <- c("statN", "statPercN")
	type <- c(statN = "n", statPercN = "%")
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable), check.names = FALSE)
	expect_equal(
		statValue,
		with(statBaseValue, paste0(n, " (", `%`, ")"))
	)
			
})

test_that("If case of 0 counts, the percentage is not included in n (%)", {
			
	stat0 <- eval(
		expr = getStats(type = "n (%)")[[1]],
		envir = data.frame(statN = 0, statPercN = Inf)
	)
	expect_equal(stat0, "0")
	
})

test_that("The m (%) is correctly extracted", {
			
	stat <- getStats(type = "m (%)")
	
	statType <- c("statm", "statPercm")
	type <- c(statm = "m", statPercm = "%m")
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
	
	# check value
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable), check.names = FALSE)
	expect_equal(
		statValue,
		with(statBaseValue, paste0(m, " (", `%m`, ")"))
	)
	
})

test_that("If case of 0 counts, the percentage is not included in m (%)", {
			
	stat0 <- eval(
		expr = getStats(type = "m (%)")[[1]],
		envir = data.frame(statm = 0, statPercm = Inf)
	)
	expect_equal(stat0, "0")
			
})

test_that("The n/N (%) is correctly extracted", {
			
	stat <- getStats(type = "n/N (%)")
			
	statType <- c("statN", "statPercN", "statPercTotalN")
	summaryTable <- setNames(
		as.data.frame(replicate(length(statType), rnorm(4))),
		statType
	)
			
	statValue <- eval(expr = stat[[1]], envir = summaryTable)
			
	# get base stats
	type <- c(statN = "n", statPercN = "%")
	statBase <- getStats(type = type)
	statBaseValue <- as.data.frame(lapply(statBase, eval, envir = summaryTable), check.names = FALSE)
	# + add statPercTotalN
	statBaseValue <- cbind(summaryTable, statBaseValue)		
	
	expect_equal(
		statValue,
		with(statBaseValue, paste0(n, "/", statPercTotalN, " (", `%`, ")"))
	)
			
})

test_that("If case of 0 counts, the percentage not included in n/N (%)", {
			
	stat0 <- eval(
		expr = getStats(type = "n/N (%)")[[1]],
		envir = data.frame(statN = 0, statPercN = Inf, statPercTotalN = NA)
	)
	expect_equal(stat0, "0")
			
})

test_that("A warning is generated if the statistics don't have unique nams", {
			
	expect_warning(
		getStats(type = c("count", "n")),
		"duplicated names"
	)
			
})

test_that("The statistic name is included if specified", {

	expect_named(
		getStats(type = "n", includeName = TRUE),
		"n"
	)
	
})

test_that("The statistic name is not included if specified", {
			
	expect_named(
		getStats(type = "n", includeName = FALSE),
		NULL
	)
			
})

test_that("The name is by default included if multiple statistics are requested", {
		
	type <- c("n", "m")
	expect_warning(
		stat <- getStats(type = type, includeName = FALSE),
		"The labels for the different types.*are retained.*"
	)
	
	expect_named(stat, type)
			
})

