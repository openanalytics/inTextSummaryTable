context("Creation of summary statistics table")

library(plyr)

test_that("No data to report", {
      
      data <- data.frame()
      expect_error(
          computeSummaryStatisticsTable(data),
          "No data to report."
      )
      
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

test_that("Summary statistics is created with flagged variables", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          x = rep(c("A", "B"), times = 3),
          xFlag = rep(c("Y", ""), times = 3),
          stringsAsFactors = FALSE
      )
      
      expect_error(
          computeSummaryStatisticsTable(data = data, var = "x", varFlag = "xFlag"),
          "All flag variables in 'varFlag' should be specified in the 'var' parameter."
      )
      
      res <- computeSummaryStatisticsTable(data = data, var = c("x", "xFlag"), varFlag = "xFlag")
      expect_s3_class(res, "data.frame")
      expect_true("variable" %in% colnames(res))
      expect_true("variableGroup" %in% colnames(res))
      
    })

test_that("Summary statistics with 'varIgnore' argument", {
      
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

test_that("Summary statistics table is created with row variables specification", {
      
      data <- data.frame(
          parent = c("A", "A", "A", "A", "B", "B"), 
          child = c("a", "a", "a", "b", "c", "c"),
          x = rnorm(n = 6),
          USUBJID = seq.int(6)
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


test_that("More columns in dataTotalRow than in data to summarize", {
      
      data <- data.frame(
          USUBJID = seq.int(6),
          TRT = rep(c("A", "B"), each = 3),
          SEV = rep(c("MILD", "SEVERE"), times = 3),
          SEVN = rep(c(1, 2), times = 3),
          COD = rep(c("Term1", "Term2", "Term3"), times = 2)
      )
      dataTotalRow <- list(COD = {
            ddply(data, c("USUBJID", "TRT"), function(x){
                  x[which.max(x$SEVN), ]
                })
          })
      
      expect_silent(
          res <- computeSummaryStatisticsTable(
              data = data,
              colVar = "TRT",
              rowVar = c("COD", "SEV"),
              rowVarTotalInclude = "COD",
              stats = getStats("n (%)"),
              dataTotalRow = dataTotalRow,
              rowVarTotalByVar = "SEV"
          )
      )
      expect_s3_class(res, "data.frame")
      expect_true(inherits(res$statPercN, "numeric"))
      expect_true(inherits(res$statN, "numeric"))
      expect_identical(
          levels(res$COD),
          c(
              "Total", "Term1",
              "Term2", "Term3"
          )
      )
      
      idxTotalA <- max(which(res$TRT == "A"))
      idxTotalB <- max(which(res$TRT == "B"))
      expect_true(is.na(res$COD[idxTotalA]))
      expect_true(res$isTotal[idxTotalA])
      expect_identical(res$statPercN[idxTotalA], 100)
      expect_true(is.na(res$COD[idxTotalB]))
      expect_true(res$isTotal[idxTotalB])
      expect_identical(res$statPercN[idxTotalB], 100)    
      
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
