context("Compute summary statistics table with a variable to summarize")

test_that("A summary table is correctly computed for a continuous variable", {
      
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
      
	sumTableCont <- computeSummaryStatisticsTable(data = dataCont, var = "x")
	expect_s3_class(sumTableCont, "summaryTable")
	varsCont <- c(
		"statN", "statm", "statMean", "statSD", "statSE", "statMedian",
		"statMin", "statMax", "statPercTotalN", "statPercN"
	)
	expect_named(sumTableCont, c("isTotal", varsCont), ignore.order = TRUE)
	expect_equal(object = sumTableCont$isTotal, expected = c(FALSE, TRUE))
      
	# stats computed via 'computeSummaryStatistics':
	sumTableContVar <- subset(sumTableCont, !isTotal)
	statsVar <- computeSummaryStatistics(dataCont, var = "x")
	expect_equal(
		object = sumTableContVar[, colnames(statsVar)], 
		expected = statsVar, 
		check.attributes = FALSE
	)
      
	# extra total statistics are added
	expect_equal(object = sumTableContVar$statPercTotalN, expected = 5)
	expect_equal(object = sumTableContVar$statPercN, expected = 4/5*100)
	  
})

test_that("A warning is generated if the variable to summarize is not available", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
	expect_warning(
		computeSummaryStatisticsTable(data = dataCont, var = "y"),
		"y.* ignored"
	)
      
})

test_that("A summary table is correctly computed for a categorical variable", {
      
	dataCat <- data.frame(
		x = c(NA_character_, "B", "B", "B", "A"), 
		USUBJID = seq.int(5), 
		stringsAsFactors = TRUE
	)
      
	sumTableCat <- computeSummaryStatisticsTable(data = dataCat, var = "x")
	expect_s3_class(sumTableCat, "summaryTable")
	varsCat <- c("statN", "statm", "statPercTotalN", "statPercN")
	expect_named(sumTableCat, c("variableGroup", "isTotal", varsCat), ignore.order = TRUE)
	expect_equal(sumTableCat$isTotal, c(FALSE, FALSE, TRUE))
      
	# stats computed via 'computeSummaryStatistics':
	sumTableCatVar <- subset(sumTableCat, !isTotal)
	statsVar <- computeSummaryStatistics(dataCat, var = "x")
	expect_equal(
		object = sumTableCatVar[, colnames(statsVar)], 
		expected = statsVar, 
		check.attributes = FALSE
	)
      
	# extra total statistics are added
	expect_equal(
		object = sumTableCatVar$statPercTotalN, 
		expected = c(5, 5)
	)
	expect_equal(
		object = sumTableCatVar$statPercN, 
		expected = with(sumTableCatVar, statN/statPercTotalN*100)
	)
      
})

test_that("A summary table is correctly computed for a flag variable", {
      
	data <- data.frame(
		USUBJID = seq.int(7),
		x = rep(c("A", "B"), times = c(3, 4)),
		xFlag = rep(c("", "Y"), length.out = 7),
		stringsAsFactors = FALSE
	)
      
	res <- computeSummaryStatisticsTable(
		data = data, 
		var = c("x", "xFlag"), 
		varFlag = "xFlag"
	)
	expect_s3_class(res, "summaryTable")
	expect_true("variable" %in% colnames(res))
	expect_true("variableGroup" %in% colnames(res))
	  
	resAll <- computeSummaryStatisticsTable(
		data = data, 
		var = c("x", "xFlag")
	)
	  
	# variable not specified in varFlag is retained
	expect_equal(
		object = subset(res, variable == "x"),
		expected = subset(resAll, variable == "x"),
		check.attributes = FALSE
	)
	  
	# only flagged records are retained for varFlag
	expect_equal(
		object = subset(res, variable == "xFlag", -variableGroup),
		expected = subset(resAll, variable == "xFlag" & variableGroup == "Y", -variableGroup),
		check.attributes = FALSE
	)	
      
})

test_that("A warning is generated if the flag variable is not available in the variables to summarize", {
			
	data <- data.frame(
		USUBJID = seq.int(7),
		x = rep(c("A", "B"), times = c(3, 4)),
		xFlag = rep(c("", "Y"), length.out = 7),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		computeSummaryStatisticsTable(data = data, var = "x", varFlag = "xFlag"),
		"xFlag.* in varFlag.*ignored"
	)

})

test_that("Zero counts for a categorical variable are correctly included when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C"))
	)
	
	sumTable0 <- computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = TRUE)
	expect_identical(as.character(na.omit(sumTable0$variableGroup)), c("A", "B", "C"))
	sumTable0Rows <- subset(sumTable0, variableGroup == "C")
	expect_equal(object = sumTable0Rows$statN, expected = 0)
	expect_equal(object = sumTable0Rows$statm, expected = 0)
	
})

test_that("Zero counts for a categorical variable are correctly not included when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C"))
	)
			
	sumTable <- computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = FALSE)
	expect_identical(
		object = as.character(na.omit(sumTable$variableGroup)), 
		expected = c("A", "B")
	)
			
})

test_that("Zero counts for a categorical variable are not included by default", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C"))
	)
	expect_identical(
		object = computeSummaryStatisticsTable(data = data, var = "x"),
		expected = computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = FALSE)
	)
	
})

test_that("Zero counts for a categorical flag variable are correctly included when requested", {
	
	data <- data.frame(
		USUBJID = seq.int(6), 
		xFlag = factor("N", c("N", "Y"))
	)		

	sumTableVarFlag0 <- computeSummaryStatisticsTable(
		data = data,
		var = "xFlag", varFlag = "xFlag",
		varInclude0 = TRUE
	)
	sumTableVarFlag0Rows <- subset(sumTableVarFlag0, !isTotal)
	expect_equal(object = sumTableVarFlag0Rows$statN, expected = 0)
	expect_equal(object = sumTableVarFlag0Rows$statm, expected = 0)
			
})

test_that("Zero counts for a categorical flag variable are correctly included when requested", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		xFlag = factor("N", c("N", "Y"))
	)		
			
	sumTable <- computeSummaryStatisticsTable(
		data = data,
		var = "xFlag", varFlag = "xFlag",
		varInclude0 = FALSE
	)
	expect_equal(nrow(subset(sumTable, !isTotal)), 0)
			
})

test_that("Zero counts for a categorical flag variable are not included by default", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		xFlag = factor("N", c("N", "Y"))
	)
	
	expect_identical(
		object = computeSummaryStatisticsTable(data = data, var = "xFlag", varFlag = "xFlag", varInclude0 = FALSE),
		expected = computeSummaryStatisticsTable(data = data, var = "xFlag", varFlag = "xFlag")
	)
	
})

test_that("Zero counts for a categorical variable are correctly included for a subset of the variables to summarize", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C")),
		xFlag = factor("N", c("N", "Y"))
	)
	sumTableVarFlag0 <- computeSummaryStatisticsTable(
		data = data,
		var = c("x", "xFlag"), varFlag = "xFlag",
		varInclude0 = "x"
	)
	expect_identical(
		object = as.character(na.omit(sumTableVarFlag0$variableGroup)), 
		expected = c("A", "B", "C")
	)
			
})	

test_that("A warning is generated if the variable for which zero counts are included is not specified in the variables to summarize", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C"))
	)
	expect_warning(
		computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = "y"),
		".*y.* in varInclude0 are ignored.*"
	)
	
})

test_that("Elements in a categorical variable to summarize are correctly ignored when requested", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          x = rep(c("A", "B"), times = 3),
          stringsAsFactors = FALSE
      )
      res <- computeSummaryStatisticsTable(
          data = data, var = "x", varIgnore = "A"
      )
      expect_s3_class(res, "summaryTable")
      expect_identical(levels(res$variableGroup), "B")
      
})

test_that("The total for the categorical variables to summarize is correctly computed", {
			
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		USUBJID = c(1, 2, 3, 1)
	)
			
	sumTableVarTotal <- computeSummaryStatisticsTable(
		data = data, var = "x", varTotalInclude = TRUE
	)
	
	expect_equal(
		as.character(na.omit(sumTableVarTotal$variableGroup)),
		c("A", "B", "Total")
	)
	sumTableVarTotalRows <- subset(sumTableVarTotal, variableGroup == "Total")
	expect_equal(sumTableVarTotalRows$statN, 3)
	expect_equal(sumTableVarTotalRows$statm, 4)
	expect_equal(sumTableVarTotalRows$statPercTotalN, 3)
	expect_equal(sumTableVarTotalRows$statPercN, 100)
	
	expect_identical(
		object = computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = "x"),
		expected = computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = TRUE)
	)
			
})

test_that("The total for the categorical variables to summarize is not included by default", {
			
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		USUBJID = c(1, 2, 3, 1)
	)

	expect_identical(
		object = computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = FALSE),
		expected = computeSummaryStatisticsTable(data = data, var = "x")
	)
	
})

test_that("The total for a subset of the categorical variables to summarize is correctly computed", {
			
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		y = c("a", "d", "g", "t"), 
		USUBJID = c(1, 2, 3, 1)
	)
	sumTableVarTotalChar <- computeSummaryStatisticsTable(
		data = data, 
		var = c("x", "y"), 
		varTotalInclude = "y"
	)
	sumTableVarTotalCharRows <- subset(sumTableVarTotalChar, variableGroup == "Total")
	expect_identical(as.character(sumTableVarTotalCharRows$variable), "y")

})

test_that("A warning is generated if the variable to compute totals on is not available", {
			
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		USUBJID = c(1, 2, 3, 1)
	)
	
	expect_warning(
		computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = "blabla"),
		".* in varTotalInclude.*ignored"
	)
	
})

test_that("The total for the variables to summarize is correctly defined in a separated row", {
			
	# the total is included in a separated row
	# during the export step
	# -> check the attribute of the output
	# to make sure that user specification is correctly stored
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		USUBJID = c(1, 2, 3, 1)
	)
	
	sumTable <- computeSummaryStatisticsTable(data = data, varTotalInSepRow = TRUE)
	expect_true("variableGroup" %in% attr(sumTable, "summaryTable")$rowVarTotalInSepRow)

})

test_that("The total for the variables to summarize is not defined in a separated row by default", {
			
	# the total is included in a separated row
	# during the export step
	# -> check the attribute of the output
	# to make sure that user specification is correctly stored
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		USUBJID = c(1, 2, 3, 1)
	)
		
	sumTable <- computeSummaryStatisticsTable(data = data, varTotalInSepRow = FALSE	)
	expect_false("variableGroup" %in% attr(sumTable, "summaryTable")$rowVarTotalInSepRow)	
			
})

test_that("A warning is generated if the deprecated parameter 'varIncludeTotal' is used", {
			
	dataCont <- data.frame(
		x = c(NA, 1, 3, 6, 10), 
		USUBJID = seq.int(5)
	)
	expect_warning(
		computeSummaryStatisticsTable(dataCont, varIncludeTotal = TRUE),
		"Argument: 'varIncludeTotal' is deprecated, please use 'varTotalInclude' instead."
	)
	
})

test_that("Elements of a categorical variable to summarize are ordered alphabetically if the variable is a character", {
      
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M"),
		stringsAsFactors = FALSE
	)
      
	# If variable is a character, levels should be sorted in alphabetical order
	tableCharac <- computeSummaryStatisticsTable(data = data, var = "SEX")
	expect_identical(
		object = levels(tableCharac$variableGroup),
		expected = sort(unique(data$SEX))
	)
	  
})

test_that("A categorical variable to summarize is converted to a factor if the variable is a character", {
			
	# edge-case: alphabetical order is used
	# all variables should already be converted as factor as input of the fct
	data <- data.frame(group = c("B", "Z", "A", "G"), stringsAsFactors = FALSE)
	varConverted <- inTextSummaryTable:::convertVarToFactorWithOrder(
		data = data, var = "group",
		method = "auto"
	)	
	expect_s3_class(varConverted, "factor")
	expect_equal(levels(varConverted), c("A", "B", "G", "Z"))
			
})

test_that("Elements of a categorical variable to summarize are ordered as specified if the variable is a factor", {
			
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = factor(c("F", "M"), levels = c("M", "F")),
		stringsAsFactors = FALSE
	)
      
	tableFactor <- computeSummaryStatisticsTable(data = data, var = "SEX")
	expect_identical(levels(tableFactor$variableGroup), levels(data$SEX))
	  
})
      
test_that("Elements of a categorical variable to summarize are ordered correctly even if they are not available for all columns", {
			
	# only last element of SEX for first TRT (will be computed first)
	# the first element of SEX for the other TRT
	# to check that order of SEX is computed upfront grouping by TRT
	data <- data.frame(
		USUBJID = seq.int(4),
		TRT = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
		SEX = factor(c("F", "F", "M", "M"), levels = c("M", "F")),
		stringsAsFactors = FALSE
	)
	# even if not present for all column variables
	tableFactorNotComplete <- computeSummaryStatisticsTable(
		data = data, 
		var = "SEX", 
		colVar = "TRT"
	)
	expect_identical(
		levels(tableFactorNotComplete$variableGroup), 
		levels(data$SEX)
	)
      
	# even if the variable is character
	data$SEX <- as.character(data$SEX)
	tableCharacNotComplete <- computeSummaryStatisticsTable(
		data = data, 
		var = "SEX", 
		colVar = "TRT"
	)
	expect_identical(
		object = levels(tableCharacNotComplete$variableGroup), 
		expected = sort(unique(data$SEX))
	)
  
})

test_that("No variable name is included in the table when no variable is specified", {
      
	data <- data.frame(USUBJID = seq.int(2))
	descTableNoVar <- computeSummaryStatisticsTable(data = data)
	expect_false("variable" %in% colnames(descTableNoVar))	
	  
})

test_that("A warning is generated if the name of the variable is requested but no variable is specified", {
			
	data <- data.frame(USUBJID = seq.int(2))
	expect_warning(
		descTableNoVar <- computeSummaryStatisticsTable(
			data = data, 
			varLabInclude = TRUE
		),
		regexp = "Variable label is not included"
	)
	  
})

test_that("No variable name is included in the table when only one variable is specified", {
				  
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M")
	)
      
	descTableOneVar <- computeSummaryStatisticsTable(data = data, var = "SEX")
	expect_false("variable" %in% colnames(descTableOneVar))
	
})

test_that("The variable name is included in the table when requested for one variable", {
			
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M")
	)
	descTableOneVarWithLabel <- computeSummaryStatisticsTable(
		data = data, var = "SEX", varLabInclude = TRUE
	)
	expect_true("variable" %in% colnames(descTableOneVarWithLabel))	
	
})


test_that("A warning is generated if the variable should be included in columns but the variable name should not be included", {
			
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M")
	)
	expect_warning(
		computeSummaryStatisticsTable(
			data = data, 
			var = "SEX", colVar = "variable",
			varLabInclude = FALSE
	  	),
		"var' not included in columns because 'varLabInclude' is FALSE"
	)
	
})

test_that("The variable name is included if more than one variable is specified", {
			
	set.seed(123)
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M"),
		AGE = rnorm(2)
	)
      
	descTableMoreOneVar <- computeSummaryStatisticsTable(
		data = data, var = c("AGE", "SEX")
	)
	expect_true("variable" %in% colnames(descTableMoreOneVar))
	
})

test_that("A warning is generated if more than one variable is specified but the variable name should not be included", {
		
	set.seed(123)
	data <- data.frame(
		USUBJID = seq.int(2),
		SEX = c("F", "M"),
		AGE = rnorm(2)
	)
	expect_warning({
		descTableMoreOneVarWithLabel <- computeSummaryStatisticsTable(
			data = data, var = c("AGE", "SEX"), varLabInclude = FALSE
		)
		},
		regexp = "Variable label is included"
	)
	
	expect_true("variable" %in% colnames(descTableMoreOneVarWithLabel))
	
})

test_that("Variables to summarize are labelled with the variable name by default", {
									
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	var <- c("SEX", "AGE")	
	sumTable <- computeSummaryStatisticsTable(data, var = var)
	
	for(varI in var){
		sumTableVarI <- computeSummaryStatisticsTable(data = data, var = varI)
		expect_equal(
			object = subset(sumTable, variable == !!varI, select = colnames(sumTableVarI)),
			expected = subset(sumTableVarI, !isTotal),
			check.attributes = FALSE
		)
	}
	
})

test_that("The labels of the variables to summarize are correctly extracted from a specified labels", {
		
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	var <- c("SEX", "AGE")
	varLab <- c(SEX = "Gender", AGE = "Age in years")
	sumTable <- computeSummaryStatisticsTable(data, var = var, varLab = varLab)
	
	for(varI in var){
		sumTableVarI <- computeSummaryStatisticsTable(data = data, var = varI)
		expect_equal(
			object = subset(sumTable, variable == varLab[!!varI], select = colnames(sumTableVarI)),
			expected = subset(sumTableVarI, !isTotal),
			check.attributes = FALSE
		)
	}
	
})

test_that("The labels of the variables to summarize are correctly extracted from the labels of all variables", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	var <- c("SEX", "AGE")
	labelVars <- c(SEX = "Gender", AGE = "Age in years")
	sumTable <- computeSummaryStatisticsTable(data, var = var, labelVars = labelVars)
	
	for(varI in var){
		sumTableVarI <- computeSummaryStatisticsTable(data = data, var = varI)
		expect_equal(
			object = subset(sumTable, variable == labelVars[!!varI], select = colnames(sumTableVarI)),
			expected = subset(sumTableVarI, !isTotal),
			check.attributes = FALSE
		)
	}
	
})
	

test_that("The labels of the variables to summarize are correctly extracted from the labels of a subset of the variables", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	var <- c("SEX", "AGE")

	sumTableVarLabNotFull <- computeSummaryStatisticsTable(
		data = data, var = var, varLab = c(SEX = "Gender")
	)
	sumTable <- computeSummaryStatisticsTable(data, var = var)
	
	expect_identical(
		object = subset(sumTableVarLabNotFull, variable == "Gender", select = -variable),
		expected = subset(sumTable, variable == "SEX", select = -variable)
	)
	expect_identical(
		object = subset(sumTableVarLabNotFull, variable == "AGE", select = -variable),
		expected = subset(sumTable, variable == "AGE", select = -variable)
	)
	
})

test_that("The general label for the variables is correctly included by default", {
		
	# general label for var set during export step
	# currently only stored in the output			
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	
	sumTable <- computeSummaryStatisticsTable(data, var = c("SEX", "AGE"))
	expect_equal(
		object = unname(attr(sumTable, "summaryTable")$rowVarLab["variable"]),
		expected = "Variable"
	)

})

test_that("The general label for the variables is correctly set when specified", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	
	sumTable <- computeSummaryStatisticsTable(
		data = data, var = c("SEX", "AGE"), 
		varGeneralLab = "test"
	)
			
	expect_equal(
		object = unname(attr(sumTable, "summaryTable")$rowVarLab["variable"]),
		expected = "test"
	)
	
})

test_that("The general label is correctly set with a warning when specified as empty", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	expect_warning(
		sumTableVarGenLabEmpty <- computeSummaryStatisticsTable(data, 
			var = c("SEX", "AGE"), varGeneralLab = NULL),
		".*varGeneralLab.* set to .* by default"
	)
	expect_equal(
		object = unname(attr(sumTableVarGenLabEmpty, "summaryTable")$rowVarLab["variable"]),
		expected = "Variable"
	)
			
})

test_that("The label for the variable subgroups is correctly included by default", {
			
	# label for var subgroup set during export step
	# currently only stored in the output			
	
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
			
	sumTable <- computeSummaryStatisticsTable(data, var = "SEX")
	expect_equal(
		object = unname(attr(sumTable, "summaryTable")$rowVarLab["variableGroup"]),
		expected = "Variable group"
	)

})

test_that("The label for the variable subgroups is correctly set when specified", {
		
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)

	sumTable <- computeSummaryStatisticsTable(
		data = data, 
		var = "SEX", 
		varSubgroupLab = "test"
	)
	expect_equal(
		object = unname(attr(sumTable, "summaryTable")$rowVarLab["variableGroup"]),
		expected = "test"
	)
	
})

test_that("The label for the variable subgroups is correctly set with a warning when specified as empty", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		sumTableVarSubgroupLabEmpty <- computeSummaryStatisticsTable(
			data = data, 
			var = "SEX", 
			varSubgroupLab = NULL
		),		
		".*'varSubgroupLab.* set to .* by default"
	)
	expect_equal(
		object = unname(attr(sumTableVarSubgroupLabEmpty, "summaryTable")$rowVarLab["variableGroup"]),
		expected = "Variable group"
	)
			
})
