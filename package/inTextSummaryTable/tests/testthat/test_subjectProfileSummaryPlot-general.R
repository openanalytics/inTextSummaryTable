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

test_that("plot is created by a variable", {

	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"), 
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)
	
	# variable not available
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit", 
			byVar = "TRT1"
		),
		"'byVar' is not available in the 'data'"
	)

	# correct specification
	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT"
	)
	
	groups <- levels(summaryTable$TRT)
	expect_named(res, groups)
	
	# compare the data behind the plot
	for(group in groups){
		
		expect_identical(
				
			object = ggplot_build(res[[!!group]])$data, 
			
			expected = {
				ggGroup <- subjectProfileSummaryPlot(
					data = subset(summaryTable, TRT == !!group),
					xVar = "visit",
					yLab = paste("Mean", !!group)
				)
				ggplot_build(ggGroup)$data
			}
	
		)
		
	}
	
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