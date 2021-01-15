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