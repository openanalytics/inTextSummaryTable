context("Create a subject profile summary plot with a x variable")

library(ggplot2)
library(plyr)

test_that("A plot is correctly created with a continuous x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit"
	)
	
	ggData <- ggplot_build(gg)$data
	
	# combine across layers
	ggDataAll <- do.call(plyr::rbind.fill, ggData)
	ggDataAll <- unique(ggDataAll[, c("x", "y")])
	
	expect_equal(
		object = ggDataAll, 
		expected = summaryTable, 
		check.attributes = FALSE
	)
	
})

test_that("A plot is successfully created with a continuous x variable with only one element", {
			
	expect_s3_class(
		subjectProfileSummaryPlot(
			data = data.frame(
				visit = 1, 
				statMean = rnorm(1)
			), 
			xVar = "visit"
		),
		"ggplot"
	)
			
})

test_that("A plot is correctly created with a discrete x variable", {
			
	summaryTable <- data.frame(
		visit = factor(c("B", "A")), 
		statMean = rnorm(2)
	)
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit"
	)
			
	ggData <- ggplot_build(gg)$data
			
	# combine across layers
	ggDataAll <- do.call(plyr::rbind.fill, ggData)
	ggDataAll <- unique(ggDataAll[, c("x", "y")])
			
	summaryTable$visitN <- as.numeric(summaryTable$visit)
	summaryTable <- summaryTable[order(summaryTable$visitN), ]
	ggDataAll$x <- as.numeric(ggDataAll$x) # x is also of type: 'mapped_discrete'
	expect_equal(
		object = ggDataAll, 
		expected = summaryTable[, c("visitN", "statMean")], 
		check.attributes = FALSE
	)
		
})

test_that("The x-axis labels are correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	xAxisLabs <- c("Visit 1" = 2, "Baseline" = 1)
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		xAxisLabs = xAxisLabs
	)
	
	# extract labels from the ggplot object
	ggScales <- gg$scales$scales
	isScaleX <- sapply(ggScales, function(x) 
		"x" %in% x[["aesthetics"]]
	)
	ggScaleX <- gg$scales$scales[[which(isScaleX)]]
	ggXAxisLabs <- setNames(ggScaleX$breaks, ggScaleX$labels)
	expect_equal(
		object = ggXAxisLabs, 
		expected = xAxisLabs
	)
	
})

test_that("A x-axis gap is correctly set", {

	summaryTable <- data.frame(
		visit = c(1, 2, 3), 
		statMean = rnorm(3)
	)
	xGap <- c(1, 3)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", xGap = xGap
	)
	ggData <- ggplot_build(gg)$data
	
	# a '//' symbol is included in the x-axis
	idxLabel <- which(
		sapply(ggData, function(x) 
			"label" %in% colnames(x) && x$label == "//")
	)
	expect_length(idxLabel, 1)
	expect_equal(ggData[[idxLabel]]$x, 2)
	expect_equal(ggData[[idxLabel]]$y, -Inf)
	
	# vertical lines are included
	idxVLine <- which(
		sapply(ggData, function(x) 
			"xintercept" %in% colnames(x)
		)
	)
	expect_length(idxVLine, 1)
	expect_equal(ggData[[idxVLine]]$xintercept, xGap)
	
	# check that data is correctly filtered:
	ggDataPlot <- ggData[-c(idxVLine, idxLabel)]
	ggDataPlot <- lapply(ggDataPlot, `[`, c("x", "y"))
	ggDataPlot <- unique(do.call(rbind, ggDataPlot))
	expect_equal(
		object = ggDataPlot,
		expected = setNames(summaryTable[-2, ], c("x", "y")),
		check.attributes = FALSE
	)
	
})

test_that("A warning is generated if a gap is requested for the x-axis but the x variable is not continuous", {

	summaryTable <- data.frame(
		visit = c("1", "2"), 
		statMean = rnorm(2)
	)			
	
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit",
			xGap = c(1, 2)
		),
		"'xGap' should only be specified for continuous x-variable"
	)
	
})

test_that("A warning is generated if a gap is requested for the x-axis but the x variable is not specified", {

	summaryTable <- data.frame(
		visit = c("1", "2"), 
		statMean = rnorm(2)
	)
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			colorVar = "visit",
			xGap = c(1, 2)
		),
		"'xGap' should only be specified if 'xVar' is specified"
	)		

})

test_that("A warning is generated if a x-axis gap is not of length 2", {

	summaryTable <-  data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit",
			xGap = 1
		),
		"'xGap' should be of length 2"
	)	

})

test_that("The range of the x-axis gap is correctly set to a specified value", {
	
	summaryTable <- data.frame(
		visit = c(1, 2, 3), 
		statMean = rnorm(3)
	)
	xGap <- c(1, 2)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		xGap = xGap, xGapDiffNew = 0.5
	)
	ggData <- ggplot_build(gg)$data
	
	# a '//' symbol is included in the x-axis
	idxLabel <- which(
		sapply(ggData, function(x) 
			"label" %in% colnames(x) && x$label == "//")
	)
	expect_length(idxLabel, 1)
	expect_equal(ggData[[idxLabel]]$x, 1.25)
	
	# check that data is correctly filtered:
	ggDataPlot <- ggData[-c(idxLabel)]
	ggDataX <- lapply(ggDataPlot, function(x){
		if("x" %in% colnames(x))
			x[, c("x")]
	})
	ggDataX <- unique(unlist(ggDataX))
	expect_equal(
		object = ggDataX,
		expected = c(1, 1.5, 2.5)
	)
	
})
		
test_that("A jitter is correctly set for the x-axis ", {
		
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2), 
		TRT = c("A", "B", "A", "B"),
		statMean = rnorm(4)
	)
		
	jitter <- 1
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		jitter = jitter
	)
		
	ggDataAll <- do.call(plyr::rbind.fill, ggplot_build(gg)$data)
	ggXJitter <- unique(with(ggDataAll, xmax-xmin)*2)
	expect_equal(
		object = ggXJitter, 
		expected = jitter
	)
		
})
		
test_that("The limits are correctly set for the x-axis", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	
	xLim <- c(1, 10)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		xLim = xLim
	)
	expect_equal(
		object = ggplot_build(gg)$layout$coord$limits$x, 
		expected = xLim
	)		
	
})

test_that("The x-axis is correctly expanded", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
			
	xAxisExpand <- expansion(mult = 4, add = 0)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		xAxisExpand = xAxisExpand
	)
	
	# extract labels from the ggplot object
	ggScales <- gg$scales$scales
	isScaleX <- sapply(ggScales, function(x) 
		"x" %in% x[["aesthetics"]]
	)
	expect_equal(
		object = ggScales[[which(isScaleX)]]$expand, 
		expected = xAxisExpand
	)
	
})