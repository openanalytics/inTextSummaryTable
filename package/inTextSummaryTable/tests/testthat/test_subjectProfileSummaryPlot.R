context("Create a subject profile summary plot")

library(ggplot2)

test_that("plot fails if variable is not available", {
			
	expect_error(
		subjectProfileSummaryPlot(data = data.frame()),
		"Variable.* not in data"
	)
			
})

test_that("x and mean variables are specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean1 = rnorm(2)
	)	
	
	expect_silent(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			meanVar = "statMean1"
		)
	)
	
	expect_s3_class(gg, "ggplot")
	
	# check data is correctly retained
	ggData <- lapply(ggplot_build(gg)$data, `[`, c("x", "y"))
	ggData <- unique(do.call(rbind, ggData))
	expect_identical(
		object = ggData,
		expected = setNames(summaryTable, c("x", "y"))
	)

})

test_that("variable for standard error is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		statSE = c(1, 2)
	)
	
	expect_silent(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			meanVar = "statMean",
			seVar = "statSE"
		)
	)
	
	expect_s3_class(gg, "ggplot")
	
	# check data is correctly retained
	ggData <- ggplot_build(gg)$data
	idxErrorBar <- which(
		sapply(ggData, function(x)
			all(c("ymin", "ymax") %in% colnames(x))
		)
	)
	expect_length(idxErrorBar, 1)
	expect_identical(
		ggData[[idxErrorBar]][, c("ymin", "ymax")],
		setNames(
			with(summaryTable, cbind.data.frame(statMean-statSE, statMean+statSE)),
			c("ymin", "ymax")
		)
	)

})

test_that("variables for min/max are specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statMin = c(0, 1),
		statMax = c(2, 3)
	)
			
	# warnings if min/max are not both specified
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			minVar = "statMin"
		),
		"'minVar' is not used because 'maxVar' is not specified."
	)
	expect_true(!
		any(sapply(ggplot_build(gg)$data, function(x)
			any(c("statMin", "statMax") %in% colnames(x))
		))
	)
	
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			maxVar = "statMax"
		),
		"'maxVar' is not used because 'minVar' is not specified."
	)
	expect_true(!
		any(sapply(ggplot_build(gg)$data, function(x)
			any(c("statMin", "statMax") %in% colnames(x))
		))
	)
	
	## correct specification:
	expect_silent(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			minVar = "statMin", maxVar = "statMax"
		)
	)
	ggData <- ggplot_build(gg)$data
	idxErrorBar <- which(
		sapply(ggData, function(x)
			all(c("ymin", "ymax") %in% colnames(x))
		)
	)
	expect_length(idxErrorBar, 1)
	expect_identical(
		ggData[[idxErrorBar]][, c("ymin", "ymax")],
		setNames(
			summaryTable[, c("statMin", "statMax")],
			c("ymin", "ymax")
		)
	)
			
})

test_that("label is specified for x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	xLab <- "Study visit"
	expect_identical(
		subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit",
			xLab = xLab
		)$labels$x, 
		xLab
	)
	
})

test_that("label is specified for y variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	yLab <- "Study visit"
	
	# by default: X is used as a title from xVar = stat[X] 
	expect_identical(
		subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit"
		)$labels$y, 
		"Mean"
	)
	
	# custom label
	yLab <- "Mean of the actual values"
	expect_identical(
		subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit",
			yLab = yLab
		)$labels$y, 
		yLab
	)
			
})

test_that("gap is specified in the x-axis ", {
			
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

test_that("new gap is specified in the x-axis", {

	summaryTable <- data.frame(
		visit = c(1, 2, 3), 
		statMean = rnorm(3)
	)
	xGap <- c(1, 3)
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
		expected = c(1, 1.5)
	)
			
})


test_that("plot is facetted", {
			
	summaryTable <- data.frame(
		PARAM = factor(
			c("AAA", "AAA", "ZZZ", "ZZZ"), 
			levels = c("ZZZ", "AAA")
		),
		visit = c(1, 2, 1, 2), 
		statMean = rnorm(4)
	)
	
	expect_silent(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit", 
			facetVar = "PARAM"
		)
	)
	
	# check that the plots is facetted
	# and that facetted are ordered according to levels of factor
	ggData <- lapply(ggplot_build(gg)$data, `[`, c("x", "y", "PANEL"))
	ggData <- unique(do.call(rbind, ggData))
	facets <- levels(summaryTable$PARAM)
	for(i in seq_along(facets)){
		expect_equal(
			object = subset(ggData, PANEL == !!i)[, c("x", "y")],
			expected = setNames(
				subset(summaryTable, PARAM == facets[[!!i]])[, c("visit", "statMean")],
				c("x", "y")
			),
			check.attributes = FALSE
		)
	}
			
})

test_that("facet scale is specified", {
			
	summaryTable <- data.frame(
		PARAM = c("AAA", "AAA", "ZZZ", "ZZZ"),
		visit = c(1, 2, 1, 2), 
		statMean = rnorm(4)
	)
			
	expect_true({
				
		gg <- subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit", 
			facetVar = "PARAM",
			facetScale = "free_y"
		)
		expect_true(
			with(ggplot_build(gg)$layout$facet$params, 
				free$y & !free$x
			)
		)
		
	})
			
})