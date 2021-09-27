context("Create a subject profile summary plot with specific theme")

library(ggplot2)

test_that("The point size is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	sizePoint <- 4
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		sizePoint = sizePoint
	)
	# extract data behind the points
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	expect_setequal(ggDataPoint$size, sizePoint)
			
})

test_that("The line size is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	sizeLine <- 5
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		sizeLine = sizeLine
	)
	# extract data behind the lines
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomLine"))
	ggDataLine <- layer_data(gg, which(isGeomLine))
	expect_setequal(ggDataLine$size, sizeLine)
	
})

test_that("The label size is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
			
	sizeLabel <- 6
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		label = TRUE, 
		sizeLabel = sizeLabel
	)
			
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomTextRepel"))
	ggDataText <- layer_data(gg, which(isGeomText))
	expect_setequal(ggDataText$size, sizeLabel)
		
})