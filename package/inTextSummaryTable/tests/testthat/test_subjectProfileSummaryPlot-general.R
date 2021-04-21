context("Create a subject profile summary plot")

library(ggplot2)

test_that("plot fails if variable is not available", {
			
	expect_error(
		subjectProfileSummaryPlot(data = data.frame()),
		"Variable.* not in data"
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

test_that("horizontal lines are specified", {
			
	hLine <- c(1, 3)
	hLineColor <- c("blue", "red")
	hLineLty <- c("dotted", "dashed")
	gg <- subjectProfileSummaryPlot(
		data = data.frame(
			visit = c(1, 2), 
			statMean = rnorm(2)
		),
		xVar = "visit", 
		hLine = hLine,
		hLineColor = hLineColor,
		hLineLty = hLineLty
	)
	
	# extract data behind the lines
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomHline"))
	ggDataLine <- do.call(rbind, ggplot_build(gg)$data[isGeomLine])
	
	expect_equal(ggDataLine$yintercept, hLine)
	expect_equal(ggDataLine$colour, hLineColor)
	expect_equal(ggDataLine$linetype, hLineLty)
	
})

test_that("horizontal lines are specified by facet", {
			
	summaryTable <- data.frame(
		PARAM = factor(
			c("AAA", "AAA", "ZZZ", "ZZZ"),
			levels = c("ZZZ", "AAA")
		),
		visit = c(1, 2, 1, 2), 
		statMean = rnorm(4)
	)
			
	expect_equal({
				
		gg <- subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit", 
			facetVar = "PARAM",
			hLine = c(AAA = 3, ZZZ = 1)
		)
		
		# extract data behind the lines
		isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomHline"))
		ggDataLine <- do.call(rbind, ggplot_build(gg)$data[isGeomLine])
	
		ggDataLine[match(c(1, 2), ggDataLine$PANEL), "yintercept"]
		
		}, 
		expected = c(1, 3)
	)
	
})

test_that("vertical lines are specified", {
			
	vLine <- c(1, 2)
	vLineColor <- c("green", "yellow")
	vLineLty <- c("dashed", "solid")
	gg <- subjectProfileSummaryPlot(
		data = data.frame(
			visit = c(1, 2), 
			statMean = rnorm(2)
		),
		xVar = "visit", 
		vLine = vLine,
		vLineColor = vLineColor,
		vLineLty = vLineLty
	)
	
	# extract data behind the lines
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomVline"))
	ggDataLine <- do.call(rbind, ggplot_build(gg)$data[isGeomLine])
	
	expect_equal(ggDataLine$xintercept, vLine)
	expect_equal(ggDataLine$colour, vLineColor)
	expect_equal(ggDataLine$linetype, vLineLty)
	
})

test_that("plot is created with a table", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		n = c(10, 20)
	)
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		tableText = "n"
	)
	
	expect_s3_class(gg, "ggplot")
	
	ggData <- ggplot_build(gg)$data
	expect_length(ggData, 2) # plot + table
	
})

test_that("height is specified for the table", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		n = c(10, 20)
	)
	
	# error
	expect_error(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			tableText = "n",
			tableHeight = 1.5
		),
		"Table height should be between 0 and 1."
	)
			
	# correct specification:
	tableHeight <- 0.45
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		tableText = "n",
		tableHeight = tableHeight
	)
	
	ggData <- ggplot_build(gg)$data
	ggData <- do.call(rbind.fill, ggData)
	
	# 2 panels are created
	expect_equal(nrow(ggData), 2)
	
	# check that created panels have correct height:
	gDataYCoord <- as.list(as.data.frame(t(ggData[, c("ymin", "ymax")])))
	gDataYCoord <- gDataYCoord[order(sapply(gDataYCoord, min))]
	expect_setequal(
		gDataYCoord,
		list(c(0, 0.45), c(0.45, 1))
	)
	
})

test_that("height is specified for the table", {
			
			summaryTable <- data.frame(
					visit = c(1, 2), 
					statMean = rnorm(2),
					n = c(10, 20)
			)
			
			# error
			expect_error(
					subjectProfileSummaryPlot(
							data = summaryTable, 
							xVar = "visit",
							tableText = "n",
							tableHeight = 1.5
					),
					"Table height should be between 0 and 1."
			)
			
			# correct specification:
			tableHeight <- 0.45
			gg <- subjectProfileSummaryPlot(
					data = summaryTable, 
					xVar = "visit",
					tableText = "n",
					tableHeight = tableHeight
			)
			
			ggData <- ggplot_build(gg)$data
			ggData <- do.call(rbind.fill, ggData)
			
			# 2 panels are created
			expect_equal(nrow(ggData), 2)
			
			# check that created panels have correct height:
			gDataYCoord <- as.list(as.data.frame(t(ggData[, c("ymin", "ymax")])))
			gDataYCoord <- gDataYCoord[order(sapply(gDataYCoord, min))]
			expect_setequal(
					gDataYCoord,
					list(c(0, 0.45), c(0.45, 1))
			)
			
		})



test_that("facetting and text variable are not compatible", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		PARAM = c("AAA", "ZZZ"),
		statMean = rnorm(2),
		n = c(1, 2)
	)	
			
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit",
			tableText = "n",
			facetVar = "PARAM"
		),
		"Table cannot be used in combination with 'facetVar', no table is included."
	)
	
})

