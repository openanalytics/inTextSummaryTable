context("Create a subject profile summary plot with a y variable")

library(ggplot2)
library(plyr)

test_that("The mean data is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean1 = rnorm(2)
	)	
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		meanVar = "statMean1"
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


test_that("The standard error data is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		statSE = c(1, 2)
	)
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		meanVar = "statMean",
		seVar = "statSE"
	)
			
	expect_s3_class(gg, "ggplot")
			
	# extract data behind the error bars
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 1)
	
	ggErrorBar <- layer_data(gg, which(isGeomErrorBar))

	expect_identical(
		ggErrorBar[, c("ymin", "ymax")],
		setNames(
			with(summaryTable, cbind.data.frame(statMean-statSE, statMean+statSE)),
			c("ymin", "ymax")
		)
	)
			
})

test_that("The data with the minimum and maximum values are correctly set", {
		
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statMin = c(0, 1),
		statMax = c(2, 3)
	)		

	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		minVar = "statMin", maxVar = "statMax"
	)
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 1)
	ggErrorBar <- layer_data(gg, which(isGeomErrorBar))

	expect_identical(
		object = ggErrorBar[, c("ymin", "ymax")],
		expected = setNames(
			summaryTable[, c("statMin", "statMax")],
			c("ymin", "ymax")
		)
	)

})

test_that("A warning is generated if a variable is specified for the minimum value but not for the maximum value", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statMin = c(0, 1)
	)
			
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			minVar = "statMin"
		),
		"'minVar' is not used because 'maxVar' is not specified."
	)
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 0)
	
})

test_that("A warning is generated if a variable is specified for the maximum value but not for the minimum value", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statMax = c(2, 3)
	)
			
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			maxVar = "statMax"
		),
		"'maxVar' is not used because 'minVar' is not specified."
	)
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 0)
	
})
			
test_that("A y-axis transformation is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rlnorm(2)
	)
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		yTrans = "log10"
	)
	ggDataAll <- do.call(rbind.fill, ggplot_build(gg)$data)
	ggDataAll <- unique(ggDataAll[, c("x", "y")])
	
	ggDataWithInput <- merge(
		x = summaryTable, y = ggDataAll, 
		by.x = "visit",
		by.y = "x",
		all = TRUE
	)	
	with(ggDataWithInput, 
		expect_equal(
			object = y, 
			expected = log10(statMean)
		)
	)		
	
})

test_that("A warning is generated if the y-axis transformation is not set as a character", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rlnorm(2)
	)

	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = log
		)
	)
	
})

test_that("A warning is generated if the y-axis transformation is not available", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rlnorm(2)
	)	
	
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = 'sqrt'
		)
	)
	
})

test_that("Negative values are correctly set to a default minimum value with a log10 transformation with a warning", {
		
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statSE = c(0.5, 3)
	)
	
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = "log10"
		),
		"1 negative value\\(s\\) in the error bars"
	)
	
	# extract data behind the error bars
	isGeomEB <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	ggDataEB <- layer_data(gg, which(isGeomEB))

	expect_equal(
		object = subset(ggDataEB, x == 2)$ymin, 
		expected = log10(0.5/10)
	)
	
})

test_that("Negative values are correctly set to a specified minimum value with a log10 transformation with a warning", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statSE = c(0.5, 3)
	)
	
	## set negative values to lower limit of y if specified
	expect_warning(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = "log10",
			yLim = c(0.005, 20)
		),
		"1 negative value\\(s\\) in the error bars"
	)
	
	# extract data behind the lines
	isGeomEB <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	ggDataEB <- layer_data(gg, which(isGeomEB))
	
	expect_equal(subset(ggDataEB, x == 2)$ymin, log10(0.005))
			
})

test_that("The limits are correctly set for the y-axis", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	
	yLim <- c(-1, 1)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		yLim = yLim
	)
	expect_equal(
		object = ggplot_build(gg)$layout$coord$limits$y, 
		expected = yLim
	)		
	
})

test_that("The y-axis is correctly expanded", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	
	yAxisExpand <- expansion(mult = 0, add = 2)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		yAxisExpand = yAxisExpand
	)
			
	# extract labels from the ggplot object
	ggScales <- gg$scales$scales
	isScaleY <- sapply(ggScales, function(x) 
		"y" %in% x[["aesthetics"]]
	)
	expect_equal(
		object = ggScales[[which(isScaleY)]]$expand, 
		expected = yAxisExpand
	)
			
})

test_that("A warning is generated if the old specification to expand the y-axis is used", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	yAxisExpand <- expansion(mult = 0, add = 2)
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yLimExpand = yAxisExpand
			),
		"'yLimExpand' is deprecated."
	)
	
})