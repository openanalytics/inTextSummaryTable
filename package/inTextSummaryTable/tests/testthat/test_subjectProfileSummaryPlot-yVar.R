context("Create a subject profile summary plot: y variable")

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
	expect_error(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			yTrans = log
		)
	)
			
	# only 'log10' transformation is supported currently
	expect_error(
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