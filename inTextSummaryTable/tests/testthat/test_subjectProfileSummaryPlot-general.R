context("Create a subject profile summary plot")

library(ggplot2)
library(plyr)

test_that("An error is generated if the default variable for the plot is not available in the data", {
			
	expect_error(
		subjectProfileSummaryPlot(data = data.frame()),
		"Variable.* not in data"
	)
			
})

test_that("The plot is correctly facetted based on a variable", {
			
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

test_that("The scale of the facet is correctly set", {
			
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

test_that("Horizontal lines are correctly set", {
			
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

test_that("Horizontal lines are correctly set by facet", {
			
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

test_that("Vertical lines are correctly set", {
			
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

test_that("A table is successfully included in a plot", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		n = c(10, 20),
		TRT = c("A", "B")
	)
			
	# ggplot2: a (expected) warning is created
	# because geom_point is used with size = NA to 
	# avoid the 'a' in the legend
	withCallingHandlers(
		expr = {
			gg <- subjectProfileSummaryPlot(
				data = summaryTable, 
				xVar = "visit",
				tableText = "n"
			)
		},
		warning = function(w){
			if(grepl("missing values \\(geom_point\\)", conditionMessage(w)))
				invokeRestart("muffleWarning")
		}
	)
	
	expect_s3_class(gg, "ggplot")
	ggData <- ggplot_build(gg)$data
	expect_length(ggData, 2) # plot + table
	
})

test_that("A table with specific height is correctly included in a plot", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		n = c(10, 20)
	)

	tableHeight <- 0.45
	
	# ggplot2: a (expected) warning is created
	# because geom_point is used with size = NA to 
	# avoid the 'a' in the legend
	withCallingHandlers(
		expr = {
			gg <- subjectProfileSummaryPlot(
				data = summaryTable, 
				xVar = "visit",
				tableText = "n",
				tableHeight = tableHeight
			)
		},
		warning = function(w){
			if(grepl("missing values \\(geom_point\\)", conditionMessage(w)))
				invokeRestart("muffleWarning")
		}
	)
	
	ggData <- ggplot_build(gg)$data
	ggData <- do.call(plyr::rbind.fill, ggData)
	
	# 2 panels are created
	expect_equal(nrow(ggData), 2)
	
	# check that created panels have correct height:
	gDataYCoord <- as.list(as.data.frame(t(ggData[, c("ymin", "ymax")])))
	gDataYCoord <- gDataYCoord[order(sapply(gDataYCoord, min))]
	expect_setequal(
		object = gDataYCoord,
		expected = list(c(0, 0.45), c(0.45, 1))
	)
	
})

test_that("An error is generated if the height for the table is not correctly specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2),
		n = c(10, 20)
	)
			
	expect_error(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			tableText = "n",
			tableHeight = 1.5
		),
		"Table height should be between 0 and 1."
	)
			
})

test_that("A warning is generated if the facet and text variables are specified", {
			
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

test_that("Extra ggplot with a data point outside the plot range is correctly included", {
  
  summaryTable <- data.frame(
    visit = c(1, 2), 
    statMean = c(2, 3),
    statSE = c(0.1, 0.2)
  )
  
  dataExtra <- data.frame(visit = 4, y = 2)
  ggExtra <- ggplot2::geom_point(ggplot2::aes(x = visit, y = y), data = dataExtra)
  gg <- subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    ggExtra = ggExtra
  )
  xScales <- ggplot_build(gg)$layout$panel_scales_x[[1]]
  # test that the data point from ggExtra is included inside plot limits
  expect_gte(object = max(xScales$limits), expected = 4)
  
})

test_that("Extra ggplot specified as a function is correctly included", {
  
  summaryTable <- data.frame(
    visit = c(1, 2), 
    statMean = c(2, 3),
    statSE = c(0.1, 0.2)
  )
  
  ggExtra <- function(gg){
    gg <- gg + 
      ggplot2::geom_vline(xintercept = 1) +
      ggplot2::geom_hline(yintercept = 2)
    return(gg)
  }
  gg <- subjectProfileSummaryPlot(
    data = summaryTable,
    xVar = "visit", 
    ggExtra = ggExtra
  )
  
  getDataGeom <- function(geom){
    isGeom <- sapply(gg$layers, function(l) inherits(l$geom, geom))
    data <- do.call(rbind, ggplot_build(gg)$data[isGeom])
    return(data)
  }
  expect_equal(getDataGeom(geom = "GeomVline")$xintercept, 1)
  expect_equal(getDataGeom(geom = "GeomHline")$yintercept, 2)
  
})