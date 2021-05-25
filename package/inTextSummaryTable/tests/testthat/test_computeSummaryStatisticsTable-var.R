context("Compute summary statistics table: var specification")

test_that("continuous var specification is successful", {
      
      dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
      
      sumTableCont <- computeSummaryStatisticsTable(data = dataCont, var = "x")
      expect_s3_class(sumTableCont, "data.frame")
      varsCont <- c(
          "statN", "statm", "statMean", "statSD", "statSE", "statMedian",
          "statMin", "statMax", "statPercTotalN", "statPercN"
      )
      expect_named(sumTableCont, c("isTotal", varsCont), ignore.order = TRUE)
      expect_equal(sumTableCont$isTotal, c(FALSE, TRUE))
      
      # stats computed via 'computeSummaryStatistics':
      sumTableContVar <- subset(sumTableCont, !isTotal)
      statsVar <- computeSummaryStatistics(dataCont, var = "x")
      expect_equal(sumTableContVar[, colnames(statsVar)], statsVar)
      
      # extra total statistics are added
      expect_equal(sumTableContVar$statPercTotalN, 5)
      expect_equal(sumTableContVar$statPercN, 4/5*100)
	  
	  # wrong var
	  expect_warning(
		computeSummaryStatisticsTable(data = dataCont, var = "y"),
		"y.* ignored"
	 )
      
})

test_that("categorical var specification is successful", {
      
      dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = seq.int(5), stringsAsFactors = TRUE)
      
      sumTableCat <- computeSummaryStatisticsTable(data = dataCat, var = "x")
      expect_s3_class(sumTableCat, "data.frame")
      varsCat <- c("statN", "statm", "statPercTotalN", "statPercN")
      expect_named(sumTableCat, c("variableGroup", "isTotal", varsCat), ignore.order = TRUE)
      expect_equal(sumTableCat$isTotal, c(FALSE, FALSE, TRUE))
      
      # stats computed via 'computeSummaryStatistics':
      sumTableCatVar <- subset(sumTableCat, !isTotal)
      statsVar <- computeSummaryStatistics(dataCat, var = "x")
      expect_equal(sumTableCatVar[, colnames(statsVar)], statsVar)
      
      # extra total statistics are added
      expect_equal(sumTableCatVar$statPercTotalN, c(5, 5))
      expect_equal(sumTableCatVar$statPercN, with(sumTableCatVar, statN/statPercTotalN*100))
      
    })

test_that("flag var specification is successful", {
      
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
      
	res <- computeSummaryStatisticsTable(
		data = data, 
		var = c("x", "xFlag"), 
		varFlag = "xFlag"
	)
	expect_s3_class(res, "data.frame")
	expect_true("variable" %in% colnames(res))
	expect_true("variableGroup" %in% colnames(res))
	  
	resAll <- computeSummaryStatisticsTable(
		data = data, 
		var = c("x", "xFlag")
	)
	  
	# variable not specified in varFlag is retained
	expect_equal(
		subset(res, variable == "x"),
		subset(resAll, variable == "x"),
		check.attributes = FALSE
	)
	  
	# only flagged records are retained for varFlag
	expect_equal(
		subset(res, variable == "xFlag", -variableGroup),
		subset(resAll, variable == "xFlag" & variableGroup == "Y", -variableGroup),
		check.attributes = FALSE
	)	
      
})

test_that("zero counts in variable are included", {
			
	data <- data.frame(
		USUBJID = seq.int(6), 
		x = factor(c("A", "B", "A"), levels = c("A", "B", "C")),
		xFlag = factor("N", c("N", "Y"))
	)
	
	### specification as a boolean
	
	## for variable
	
	# include 0 counts
	expect_silent(sumTable0 <- computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = TRUE))
	expect_identical(as.character(na.omit(sumTable0$variableGroup)), c("A", "B", "C"))
	sumTable0Rows <- subset(sumTable0, variableGroup == "C")
	expect_equal(sumTable0Rows$statN, 0)
	expect_equal(sumTable0Rows$statm, 0)
	
	# varInclude0 set to 'FALSE'
	expect_identical(
		computeSummaryStatisticsTable(data = data, var = "x"),
		computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = FALSE)
	)
	computeSummaryStatisticsTable(
		data = data,
		var = "x",
		varInclude0 = FALSE
	)
	
	## for variable flag
	expect_identical(
		computeSummaryStatisticsTable(data = data, var = "xFlag", varFlag = "xFlag", varInclude0 = FALSE),
		computeSummaryStatisticsTable(data = data, var = "xFlag", varFlag = "xFlag")
	)
	sumTableVarFlag0 <- computeSummaryStatisticsTable(
		data = data,
		var = c("x", "xFlag"), varFlag = "xFlag",
		varInclude0 = TRUE
	)
	expect_true("xFlag" %in% sumTableVarFlag0$variable)
	sumTableVarFlag0Rows <- subset(sumTableVarFlag0, variable == "xFlag")
	expect_equal(sumTableVarFlag0Rows$statN, 0)
	expect_equal(sumTableVarFlag0Rows$statm, 0)
	
	## specification as a character
	
	# correct specification
	sumTableVarFlag0 <- computeSummaryStatisticsTable(
		data = data,
		var = c("x", "xFlag"), varFlag = "xFlag",
		varInclude0 = "x"
	)
	expect_identical(as.character(na.omit(sumTableVarFlag0$variableGroup)), c("A", "B", "C"))
			
	# wrong variable
	expect_warning(
		computeSummaryStatisticsTable(data = data, var = "x", varInclude0 = "y"),
		".*y.* in varInclude0 are ignored.*"
	)
	
})

test_that("a variable is properly ignored", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          x = rep(c("A", "B"), times = 3),
          xFlag = rep(c("Y", ""), times = 3),
          stringsAsFactors = FALSE
      )
      res <- computeSummaryStatisticsTable(
          data = data, var = "x", varIgnore = "A"
      )
      expect_s3_class(res, "data.frame")
      expect_identical(levels(res$variableGroup), "B")
      
})


test_that("total for a variable is included", {
			
	data <- data.frame(
		x = c("A", "B", "A", "B"), 
		y = c("a", "d", "g", "t"), 
		USUBJID = c(1, 2, 3, 1)
	)
	
	## no total
	expect_identical(
		computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = FALSE),
		computeSummaryStatisticsTable(data = data, var = "x")
	)
	
	## with total
			
	# logical
	expect_silent(sumTableVarTotal <- computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = TRUE))
	expect_equal(
		as.character(na.omit(sumTableVarTotal$variableGroup)),
		c("A", "B", "Total")
	)
	sumTableVarTotalRows <- subset(sumTableVarTotal, variableGroup == "Total")
	expect_equal(sumTableVarTotalRows$statN, 3)
	expect_equal(sumTableVarTotalRows$statm, 4)
	expect_equal(sumTableVarTotalRows$statPercTotalN, 3)
	expect_equal(sumTableVarTotalRows$statPercN, 100)
	
	# character
	expect_silent(
		sumTableVarTotalChar <- computeSummaryStatisticsTable(data = data, var = c("x", "y"), varTotalInclude = "y")
	)
	sumTableVarTotalCharRows <- subset(sumTableVarTotalChar, variableGroup == "Total")
	expect_identical(as.character(sumTableVarTotalCharRows$variable), "y")

	expect_identical(
		computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = "x"),
		computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = TRUE)
	)
	
	# wrong spec
	expect_warning(
		computeSummaryStatisticsTable(data = data, var = "x", varTotalInclude = "blabla"),
		".* in varTotalInclude.*ignored"
	)
	
})

test_that("total in separated row correctly stored", {
			
	# inclusion in separated row during export step
	# currently only stored in the output
	data <- data.frame(x = c("A", "B", "A", "B"), USUBJID = c(1, 2, 3, 1))
	expect_true({
		sumTable <- computeSummaryStatisticsTable(data = data, varTotalInSepRow = TRUE)
		"variableGroup" %in% attr(sumTable, "summaryTable")$rowVarTotalInSepRow
	})
	expect_false({
		sumTable <- computeSummaryStatisticsTable(data = data, varTotalInSepRow = FALSE	)
		"variableGroup" %in% attr(sumTable, "summaryTable")$rowVarTotalInSepRow
	})	
			
})

test_that("parameter 'varIncludeTotal' is deprecated", {
			
	dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
	expect_warning(
		computeSummaryStatisticsTable(dataCont, varIncludeTotal = TRUE),
		"Argument: 'varIncludeTotal' is deprecated, please use 'varTotalInclude' instead."
	)
	
})



test_that("Order of variable group is correct", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          stringsAsFactors = FALSE
      )
      
      # If variable is a character, levels should be sorted in alphabetical order
      tableCharac <- computeSummaryStatisticsTable(data = data, var = "SEX")
      expect_identical(
          levels(tableCharac$variableGroup),
          sort(unique(data$SEX))
      )
      
      # If variable is a factor, levels order should be retained
      data$SEX <- factor(data$SEX, levels = rev(sort(unique(data$SEX))))
      tableFactor <- computeSummaryStatisticsTable(data = data, var = "SEX")
      expect_identical(levels(tableFactor$variableGroup), levels(data$SEX))
      
      # even if not present for all colVar
      data$TRT <- factor(rep(c("A", "B"), each = 3))
      data$SEX <- factor(data$SEX, levels = rev(sort(unique(data$SEX))))
      # subset to retain:
      # only last element of SEX for first TRT01P (will be computed first)
      # the first element of SEX for the other TRT01P
      # to check that order of SEX is computed upfront grouping by TRT01P
      dataSubset <- subset(data, (
                TRT == head(levels(data$TRT), 1) &
                SEX %in% tail(levels(data$SEX), 1)
                ) | 
              !TRT == head(levels(data$TRT), 1) &
              !SEX %in% tail(levels(data$SEX), 1)
      )
      tableFactorNotComplete <- computeSummaryStatisticsTable(data = dataSubset, var = "SEX")
      expect_identical(levels(tableFactorNotComplete$variableGroup), levels(dataSubset$SEX))
      
	# even if the var is character
      dataSubset$SEX <- as.character(dataSubset$SEX)
      tableCharacNotComplete <- computeSummaryStatisticsTable(data = dataSubset, var = "SEX")
      expect_identical(levels(tableCharacNotComplete$variableGroup), sort(unique(dataSubset$SEX)))
      
    })

test_that("label of variable name is included when needed", {
      
	data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          AGE = seq(20, 62, length.out = 6),
          stringsAsFactors = FALSE
      )
      
      # no variable:
      descTableNoVar <- computeSummaryStatisticsTable(data = data)
      expect_false("variable" %in% colnames(descTableNoVar))	
      expect_warning(
          descTableNoVar <- computeSummaryStatisticsTable(
				data = data, 
				varLabInclude = TRUE
			),
          regexp = "Variable label is not included"
      )
      
      # one variable
      descTableOneVar <- computeSummaryStatisticsTable(
          data = data, var = "AGE"
      )
      expect_false("variable" %in% colnames(descTableOneVar))		
      descTableOneVarWithLabel <- computeSummaryStatisticsTable(
          data = data, var = "AGE", varLabInclude = TRUE
      )
	expect_true("variable" %in% colnames(descTableOneVarWithLabel))	
	expect_warning(
		computeSummaryStatisticsTable(
			data = data, 
			var = "AGE", colVar = "variable",
			varLabInclude = FALSE
	  	),
		"var' not included in columns because 'varLabInclude' is FALSE"
	)
      
      # > 1 variable
      descTableMoreOneVar <- computeSummaryStatisticsTable(
          data = data, var = c("AGE", "SEX")
      )
      expect_true("variable" %in% colnames(descTableMoreOneVar))		
      expect_warning({
            descTableMoreOneVarWithLabel <- computeSummaryStatisticsTable(
                data = data, var = c("AGE", "SEX"), varLabInclude = FALSE
            )
          },
          regexp = "Variable label is included"
      )
      expect_true("variable" %in% colnames(descTableMoreOneVarWithLabel))
      
})
	
test_that("specified var label is used", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	var <- c("SEX", "AGE")
	varLab <- c(SEX = "Gender", AGE = "Age in years")
	
	# by default, variable name is used as label:
	expect_silent(sumTable <- computeSummaryStatisticsTable(data, var = var))
	expect_true(all(c("SEX", "AGE") %in% sumTable$variable))
	
	# specify variable labels with 'varLab'
	expect_silent(sumTableVarLab <- computeSummaryStatisticsTable(data, var = var, varLab = varLab))
	for(varI in var){
		expect_identical(
			subset(sumTableVarLab, variable == varLab[!!varI], select = -variable),
			subset(sumTable, variable == !!varI, select = -variable)
		)
	}
	
	# specify variable names in 'labelVars'
	expect_identical(computeSummaryStatisticsTable(data, var = var, labelVars = varLab), sumTableVarLab)
	
	# no errors if labels are not specified for all variables
	expect_silent(sumTableVarLabNotFull <- computeSummaryStatisticsTable(data, var = var, varLab = c(SEX = "Gender")))
	expect_identical(
		subset(sumTableVarLabNotFull, variable == "Gender", select = -variable),
		subset(sumTable, variable == "SEX", select = -variable)
	)
	expect_identical(
		subset(sumTableVarLabNotFull, variable == "AGE", select = -variable),
		subset(sumTable, variable == "AGE", select = -variable)
	)
	
})

test_that("general var label is specified", {
		
	# general label for var set during export step
	# currently only stored in the output			
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		AGE = seq(20, 62, length.out = 6),
		stringsAsFactors = FALSE
	)
	
	# by default:
	expect_equal({
		sumTable <- computeSummaryStatisticsTable(data, var = c("SEX", "AGE"))
		unname(attr(sumTable, "summaryTable")$rowVarLab["variable"])
	}, "Variable")
	
	# custom specification
	expect_equal({
		sumTable <- computeSummaryStatisticsTable(data, var = c("SEX", "AGE"), varGeneralLab = "test")
		unname(attr(sumTable, "summaryTable")$rowVarLab["variable"])
	}, "test")
	
	# wrong specification
	expect_warning(
		sumTableVarGenLabEmpty <- computeSummaryStatisticsTable(data, 
			var = c("SEX", "AGE"), varGeneralLab = NULL),
		".*varGeneralLab.* set to .* by default"
	)
	expect_equal(
		unname(attr(sumTableVarGenLabEmpty, "summaryTable")$rowVarLab["variable"]),
		"Variable"
	)
			
})

test_that("label for variable subgroup is specified", {
			
	# label for var subgroup set during export step
	# currently only stored in the output			
	
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
			
	# by default:
	expect_equal({
		sumTable <- computeSummaryStatisticsTable(data, var = "SEX")
		unname(attr(sumTable, "summaryTable")$rowVarLab["variableGroup"])
	}, "Variable group")

	# custom specification
	expect_equal({
		sumTable <- computeSummaryStatisticsTable(data, var = "SEX", varSubgroupLab = "test")
		unname(attr(sumTable, "summaryTable")$rowVarLab["variableGroup"])
	}, "test")
			
	# wrong specification
	expect_warning(
		sumTableVarSubgroupLabEmpty <- computeSummaryStatisticsTable(data, var = "SEX", varSubgroupLab = NULL),
		".*'varSubgroupLab.* set to .* by default"
	)
	expect_equal(
		unname(attr(sumTableVarSubgroupLabEmpty, "summaryTable")$rowVarLab["variableGroup"]),
		"Variable group"
	)
			
})

test_that("non factor variable is automatically ordered", {
			
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
