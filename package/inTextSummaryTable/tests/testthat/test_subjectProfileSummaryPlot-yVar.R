context("Create a subject profile summary plot: y variable")

library(ggplot2)

test_that("variable for the mean is specified", {
			
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
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 0)
			
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
			
	## correct specification:
	
	expect_silent(
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			minVar = "statMin", maxVar = "statMax"
		)
	)
	isGeomErrorBar <- sapply(gg$layers, function(l) inherits(l$geom, "GeomErrorbar"))
	expect_length(which(isGeomErrorBar), 1)
	ggErrorBar <- layer_data(gg, which(isGeomErrorBar))
	
	expect_identical(
		ggErrorBar[, c("ymin", "ymax")],
		setNames(
			summaryTable[, c("statMin", "statMax")],
			c("ymin", "ymax")
		)
	)
	
})

test_that("y-axis is transformed", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rlnorm(2)
	)
			
	# transformation should be specified as a character
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = log
		)
	)
			
	# only 'log10' transformation is supported currently
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = 'sqrt'
		)
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
	with(ggDataWithInput, expect_equal(y, log10(statMean)))		
	
})

test_that("negative y values are transformed with a log10 transformation", {
		
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(1, 2),
		statSE = c(0.5, 3)
	)
	
	## default: set negative values to a minimum value
	
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

	expect_equal(subset(ggDataEB, x == 2)$ymin, log10(0.5/10))
	
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

test_that("limit is specified for the y-axis", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	
	yLim <- c(-1, 1)
	expect_equal({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yLim = yLim
		)
		ggplot_build(gg)$layout$coord$limits$y
		}, 
		yLim
	)		
	
})

test_that("y-axis is expanded", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	
	yAxisExpand <- expansion(mult = 0, add = 2)
	
	# yLimExpand -> yAxisExpand for consistency
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yLimExpand = yAxisExpand
		),
		"'yLimExpand' is deprecated."
	)
		
	# correct specification
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
	expect_equal(ggScales[[which(isScaleY)]]$expand, yAxisExpand)
			
})