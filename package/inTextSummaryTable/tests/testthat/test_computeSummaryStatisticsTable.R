context("Creation of summary statistics table")

library(plyr)

test_that("No data to report", {
      
      data <- data.frame()
      expect_message(
          res <- computeSummaryStatisticsTable(data),
          "No data to report."
      )
      expect_null(res)
      
    })

test_that("Parameter 'varIncludeTotal' is deprecated", {
      
      dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
      expect_warning(
          computeSummaryStatisticsTable(dataCont, varIncludeTotal = TRUE),
          "Argument: 'varIncludeTotal' is deprecated, please use 'varTotalInclude' instead."
      )
      
    })

test_that("summary statistics table is created with only data specified", {
      
      dataCont <- data.frame(x = c(NA, 1, 3, 6, 10), USUBJID = seq.int(5))
      
      # no variable specified: a count table is created
      sumNoVar <- computeSummaryStatisticsTable(dataCont)
      expect_s3_class(sumNoVar, "data.frame")
      expect_named(sumNoVar, c("isTotal", "statN", "statm", "statPercTotalN", "statPercN"), ignore.order = TRUE)
      expect_equal(
          unname(unlist(subset(sumNoVar, isTotal)[1, c("statN", "statm", "statPercTotalN", "statPercN")])),
          c(5, 5, 5, 100)
      )
      expect_equal(subset(sumNoVar, isTotal)[, -1], subset(sumNoVar, !isTotal)[, -1], check.attributes = FALSE)
      
    })

test_that("summary statistics table is created with only data/continuous var specification", {
      
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

test_that("summary statistics table is created with only data/categorical var specification", {
      
      dataCat <- data.frame(x = c(NA_character_, "B", "B", "B", "A"), USUBJID = seq.int(5))
      
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

test_that("summary statistics table is created with flagged variables", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          x = rep(c("A", "B"), times = 3),
          xFlag = rep(c("Y", ""), times = 3),
          stringsAsFactors = FALSE
      )
      
      expect_warning(
          computeSummaryStatisticsTable(data = data, var = "x", varFlag = "xFlag"),
          "xFlag.* in varFlag.*ignored"
      )
      
      res <- computeSummaryStatisticsTable(data = data, var = c("x", "xFlag"), varFlag = "xFlag")
      expect_s3_class(res, "data.frame")
      expect_true("variable" %in% colnames(res))
      expect_true("variableGroup" %in% colnames(res))
      
    })

test_that("summary statistics is created with with 'varIgnore' argument", {
      
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

test_that("summary statistics table is created with row variables specification", {
      
      data <- data.frame(
          parent = c("A", "A", "A", "A", "B", "B"), 
          child = c("a", "a", "a", "b", "c", "c"),
          x = rnorm(n = 6),
          USUBJID = seq.int(6)
      )
	  
	  expect_warning(
		computeSummaryStatisticsTable(data, rowVar = "Y"),
		"Y.* in rowVar.*ignored"
	  )
	  
      sumTableRowVar <- computeSummaryStatisticsTable(
          data,
          rowVar = c("parent", "child"),
          var = "x"
      )    
      expect_s3_class(sumTableRowVar, "data.frame")
      varsCat <- c(
          "statN", "statm",
          "statMean",
          "statSD", "statSE",
          "statMedian", "statMin",
          "statMax",
          "statPercTotalN", "statPercN"
      )
      expect_identical(
          names(sumTableRowVar),
          c("parent", "child", "isTotal", varsCat)
      )
      lastRowIdx <- nrow(sumTableRowVar)
      expect_true(sumTableRowVar$isTotal[lastRowIdx])
      expect_identical(
          sumTableRowVar$statPercN[lastRowIdx],
          100
      )
      
})

test_that("row variables are automatically ordered correctly", {
      
		# from character
      data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          stringsAsFactors = FALSE
      )
	  
	  # wrong param
	  expect_error(
		  resAuto <- computeSummaryStatisticsTable(
			  data,
			  rowVar = "SEX",
			  rowOrder = "test"
		  ),
		  '.*should be one of.*auto.*alphabetical.*total.*'
	  )
      
      expect_silent(
          resAuto <- computeSummaryStatisticsTable(
              data,
              rowVar = "SEX",
              rowOrder = "auto"
          )
      )
      expect_identical(
          levels(resAuto$SEX),
          c("F", "M")
      )
           
      expect_silent(
          resAlphabet <- computeSummaryStatisticsTable(
              data,
              rowVar = "SEX",
              rowOrder = "auto"
          )
      )
      expect_identical(resAuto, resAlphabet)
	  
	  # from factor:
	  data$SEX <- factor(data$SEX, levels = c("M", "F"))
	  expect_silent(
		resAuto <- computeSummaryStatisticsTable(
			 data,
			 rowVar = "SEX",
			 rowOrder = "auto"
		)
	  )
	  expect_identical(levels(resAuto$SEX), c("M", "F"))
      
})

test_that("row variables are correctly ordered based on alphabetical order", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          TRT = c("B", "B", "B", "B", "A", "A"),
          stringsAsFactors = FALSE
      )
      # Alphabetical
      expect_silent(
          resAlphabet <- computeSummaryStatisticsTable(
              data,
              rowVar = "TRT",
              rowOrder = "alphabetical"
          )
      )
      expect_identical(levels(resAlphabet$TRT), c("A", "B"))
	  
})
	  
test_that("row variables are correctly ordered based on the total", {
				  
	  data <- data.frame(
		  USUBJID = seq.int(6),
		  TRT = c("B", "B", "B", "B", "A", "A"),
		  stringsAsFactors = FALSE
	  )
      
      # Total
      expect_silent(
          resTotal <- computeSummaryStatisticsTable(
              data,
              rowVar = "TRT",
              rowOrder = "total"
          )
      )
      expect_identical(
          levels(resTotal$TRT),
          c("B", "A")
      )
	  
})
	  
test_that("row are ordered correctly when different order of row variables are specified", {
				  
	  data <- data.frame(
		  USUBJID = seq.int(6),
		  SEX = c("F", "F", "M", "M", "M", "M"),
		  TRT = c("B", "B", "B", "B", "A", "A"),
		  stringsAsFactors = FALSE
	  )
      
      # Different for each row variable
      expect_silent(
          resDoubleOrder <- computeSummaryStatisticsTable(
              data,
              rowVar = c("SEX", "TRT"),
              rowOrder = c(SEX = "auto", TRT = "total")
          )
      )      
      expect_identical(
          levels(resDoubleOrder$TRT),
          c("B", "A")
      )
      expect_identical(
          levels(resDoubleOrder$SEX),
          c("F", "M")
      )
	  
})

test_that("Summary statistics with order of row variable based on function", {

	  data <- data.frame(
		USUBJID = seq.int(6),
		TRT = c("B", "B", "B", "B", "A", "A"),
		stringsAsFactors = FALSE
	 )
	 resFct <- computeSummaryStatisticsTable(
		data,
		rowVar = "TRT",
		rowOrder = function(sumTable){
			data <- subset(sumTable, !isTotal)
			data[order(data$statN, decreasing = TRUE), "TRT"]
		}
     )
	 expect_identical(levels(resFct$TRT), c("B", "A"))
	 
	 # if function is wrong, e.g. doesn't return all values in sumTable
	 # the remaining values are added anyway
	 expect_silent(
		resFct <- computeSummaryStatisticsTable(
			 data,
			 rowVar = "TRT",
			 rowOrderTotalFilterFct = function(sumTable){
				 c("")
			 }
		 )
	)
	expect_identical(levels(resFct$TRT), c("A", "B"))
      
})

test_that("Summary statistics with order of row variable based on filtered data total", {
	
	# example order based on total would be different than based on specified subset data total
	data <- data.frame(
		USUBJID = seq.int(5),
		TRT = c("B", "B", "B", "A", "A"),
		SEX = c("M", "M", "F", "F", "F"),
		stringsAsFactors = FALSE
	)
			
	expect_silent(
		resTotal <- computeSummaryStatisticsTable(
			data,
			rowVar = c("TRT", "SEX"),
			rowOrder = c(SEX = "total", TRT = "auto"),
			rowOrderTotalFilterFct = function(x) subset(x, TRT == "B")
		)
	)
	expect_identical(levels(resTotal$SEX), c("M", "F"))
			
})

test_that("Summary statistics with order of row variable based on filtered data total", {
			
	data <- data.frame(
		USUBJID = seq.int(5),
		TRT = c("Z", "B", "Other", "Y", "M"),
		catLast = "Other",
		stringsAsFactors = FALSE
	)
	
	expect_silent(
		res <- computeSummaryStatisticsTable(
			data,
			rowVar = "TRT",
			rowOrder = "auto",
			rowOrderCatLast = "Other"
		)
	)
	expect_identical(levels(res$TRT), c("B", "M", "Y", "Z", "Other"))
			
})

test_that("Summary statistics with order of row variable based on filtered data total", {
		
	# for example: table of lab abnormalities
	data <- data.frame(
		USUBJID = seq.int(7),
		PARAM = factor(c("A", "A", "A", "B", "B", "B", "B")),
		AVALC = factor(c(">10", ">10", ">10", "<=2", "]2, 10]", "]2, 10]", "]2, 10]")),
		stringsAsFactors = FALSE
	)
	
	avalcFact <- c("<=10", ">10", "<=2", "]2, 10]", "> 10")
	rowVarDataLevels <- data.frame(
		PARAM = c(rep("A", 2), rep("B", 3)),
		AVALC = factor(avalcFact, levels = avalcFact)
	)
	expect_silent(
		resSpecLevels <- computeSummaryStatisticsTable(
			data,
			rowVar = c("PARAM", "AVALC"),
			rowVarDataLevels = rowVarDataLevels
		)
	)
	# all levels specified in rowVarDataLevels are present:
	expect_identical(levels(resSpecLevels$AVALC), levels(rowVarDataLevels$AVALC))
	
	# categories not in the data are also present with a count of 0:
	avalcNotInData <- setdiff(avalcFact, data$AVALC)
	expect_true(all(avalcNotInData %in% resSpecLevels$AVALC))
	expect_identical(
		subset(resSpecLevels, AVALC %in% avalcNotInData)$statN,
		as.integer(rep(0, length(avalcNotInData)))
	)
			
})

test_that("total per row variables included when requested", {
			
	data <- data.frame(
		USUBJID = c(1, 2, 3, 4, 5, 6, 6),
		SEX = c("M", "M", "M", "F", "F", "F", "F"),
		stringsAsFactors = FALSE
	)
	
	# wrong specification
	sumTableBase <- computeSummaryStatisticsTable(data = data, rowVar = "SEX")
	expect_warning(
		sumTableRowTotalWrong <- computeSummaryStatisticsTable(
			data = data,
			rowVar = "SEX",
			rowVarTotalInclude = TRUE
		),
		"Variable.*TRUE.* are not available"
	)
	expect_identical(sumTableRowTotalWrong, sumTableBase)
	
	# correct specification
	sumTableRowTotal <- computeSummaryStatisticsTable(
		data = data,
		rowVar = "SEX",
		rowVarTotalInclude = "SEX"
	)
	expect_true("Total" %in% sumTableRowTotal$SEX)
	sumTableTotal <- subset(sumTableRowTotal, SEX == "Total")
	expect_equal(sumTableTotal$statN, 6)
	expect_equal(sumTableTotal$statm, 7)
			
})

test_that("total in separated row stored in output", {
			
	data <- data.frame(
		USUBJID = seq.int(6),
		SEX = rep(c("M", "F"), times = 3),
		stringsAsFactors = FALSE
	)
	expect_silent(
		sumTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "SEX",
			rowVarTotalInclude = "SEX",
			rowVarTotalInSepRow = "SEX"
		)
	)
	# inclusion of row var in separated row during export step
	# currently only stored in the output
	expect_true("SEX" %in% attr(sumTable, "summaryTable")$rowVarTotalInSepRow)
	
	expect_warning(
		sumTable <- computeSummaryStatisticsTable(
			data = data, 
			rowVar = "SEX",
			rowVarTotalInSepRow = "SEX"
		),
		"SEX.*in rowVarTotalInSepRow are ignored.*"
	)
	
})

test_that("Summary statistics table is created with col variable specification", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          AGE = seq(20, 62, length.out = 6),
          TRT = rep(c("A", "B"), each = 3),
          stringsAsFactors = FALSE
      )
      expect_silent(
          res <- computeSummaryStatisticsTable(
              data,
              var = "AGE",
              colVar = "TRT"
          )
      )
      expect_s3_class(res, "data.frame")
      expect_identical(res$isTotal, c(FALSE, TRUE, FALSE, TRUE))
      expect_identical(levels(res$TRT), unique(data$TRT))
      expect_true("TRT" %in% colnames(res))      
      
    })

test_that("Summary statistics table is created with col variable and total", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          AGE = seq(20, 62, length.out = 6),
          TRT = rep(c("A", "B"), each = 3),
          stringsAsFactors = FALSE
      )
      expect_silent(
          res <- computeSummaryStatisticsTable(
              data,
              var = "AGE",
              colVar = "TRT",
              colTotalInclude = TRUE
          )
      )
      expect_s3_class(res, "data.frame")
      expect_true("TRT" %in% colnames(res))
      expect_identical(
          levels(res$TRT),
          c(unique(data$TRT), "Total")
      )
      
    })


test_that("More groups in colVar in dataTotalRow than in data to summarize", {
      
      dataAll <- data.frame(
          USUBJID = rep(c(1:6), each = 2),
          TRT = rep(c("A", "B"), each = 6),
          COD = rep(c("Term1", "Term2", "Term3"), each = 4),
		  stringsAsFactors = FALSE
      )
	  data <- subset(dataAll, TRT == "A")
      dataTotalRow <- list(COD = dataAll)
      
      expect_silent(
          summaryTable <- computeSummaryStatisticsTable(
              data = data,
              colVar = "TRT",
              rowVar = "COD",
              rowVarTotalInclude = "COD",
              stats = getStats("n (%)"),
              dataTotalRow = dataTotalRow
          )
      )
      expect_s3_class(summaryTable, "data.frame")
      expect_identical(levels(summaryTable$TRT), c("A", "B"))
      
	  summaryTableGroupOnlyInTotal <- subset(summaryTable, TRT == "B")
	  expect_equal(nrow(summaryTableGroupOnlyInTotal), 1)
	  expect_equal(as.character(summaryTableGroupOnlyInTotal$COD), "Total")
	  expect_equal(summaryTableGroupOnlyInTotal$statN, 3)
	  expect_equal(summaryTableGroupOnlyInTotal$statm, 6)
	
})

test_that("Custom set of statistics per variable", {
      
      # variable to summarize
      stats <- list(
          AVAL = getStats(c("n", "mean (se)")), 
          CHG = getStats(c("n", "Median", "mean (se)"))
      )
      summaryTable <- computeSummaryStatisticsTable(
          var = c("AVAL", "CHG"),
          data = subset(dataLB, PARAMCD == "ALB" & AN01FL == "Y"),
          colVar = c("TRTP", "AVISIT"),
          stats = stats
      )
      expect_identical(
          attr(summaryTable, "summaryTable")$statsVar,
          c("n", "Median", "Mean (SE)"),
          label = "Order of statistics is as specified by variable"
      )
      
      # extra variable
      dataTable <- subset(dataLB, PARAMCD %in% c("ALB", "ALP") & AN01FL == "Y")
      stats <- list(
          AVAL = list(
              ALB = getStats("n"), 
              ALP = getStats(c("n", "mean (se)"))
          ), 
          CHG = list(
              ALB = getStats(c("n", "Median", "mean (se)")),
              ALP = getStats(c("n", "Median", "mean (se)"))
          )
      )
      summaryTable <- computeSummaryStatisticsTable(
          rowVar = "PARAMCD",
          var = c("AVAL", "CHG"),
          data = dataTable,
          colVar = c("TRTP", "AVISIT"),
          stats = stats, statsVarBy = "PARAMCD"
      )
      expect_identical(
          attr(summaryTable, "summaryTable")$statsVar,
          c("n", "Median", "Mean (SE)"),
          label = "Order of statistics is as specified by parameter and variable"
      )
      
    })

test_that("Order of summary statistic variable is correct", {
      
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

test_that("Label of variable name is included when needed", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          SEX = rep(c("M", "F"), times = 3),
          AGE = seq(20, 62, length.out = 6),
          stringsAsFactors = FALSE
      )
      labels <- c(
          USUBJID = "Subject ID",
          SEX = "Sex",
          AGE = "Age, years"
      )
      
      # no variable:
      descTableNoVar <- computeSummaryStatisticsTable(data = data)
      expect_false("variable" %in% colnames(descTableNoVar))	
      expect_warning(
          descTableNoVar <- computeSummaryStatisticsTable(data = data, varLabInclude = TRUE),
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
