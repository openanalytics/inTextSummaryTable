context("Create a subject profile summary plot with aesthetics")

library(ggplot2)
library(plyr)

test_that("A color variable is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = c("A", "A", "B", "B"),
		statMean = rnorm(4),
		stringsAsFactors = TRUE
	)	
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT"
	)
	ggData <- ggplot_build(gg)$data
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# combine across layers
	ggDataAll <- do.call(plyr::rbind.fill, ggData)
	
	ggDataWithInput <- merge(
		x = summaryTable, y = ggDataAll, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)		
	colors <- with(ggDataWithInput, tapply(colour, TRT, unique))
	expect_type(colors, "character")
	expect_length(colors, 2)
	expect_length(unique(colors), 2)
	
})

test_that("A color palette is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statMean = rnorm(4)
	)	
	
	colorPalette <- c(A = "red", B = "yellow")
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		colorPalette = colorPalette
	)
	ggData <- ggplot_build(gg)$data
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# combine across layers
	ggDataAll <- do.call(plyr::rbind.fill, ggData)
	
	ggDataWithInput <- merge(
		x = summaryTable, y = ggDataAll, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)
	
	colors <- with(ggDataWithInput, tapply(colour, TRT, unique))
	expect_type(colors, "character")
	expect_equal(as.vector(colors[names(colorPalette)]), unname(colorPalette))
	
})

test_that("A label for the color variable is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = c("A", "A", "B", "B"), 
		statMean = rnorm(4)
	)	
	colorLab <- "Study Treatment"
		
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		colorLab = colorLab
	)
		
	# extract color scale
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	expect_equal(sum(isColorAes), 1)
	expect_equal(ggScales[[which(isColorAes)]]$name, colorLab)
		
})

test_that("Line types are correctly used to differenciate the groups of the color variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = c("A", "A", "B", "B"),
		statMean = rnorm(4),
		stringsAsFactors = TRUE
	)	
			
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		useLinetype = TRUE
	)
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# extract data behind the lines
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomLine"))
	ggDataLine <- layer_data(gg, which(isGeomLine))
	
	ggDataLineWithInput <- merge(
		x = summaryTable, y = ggDataLine, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)		

	ltys <- with(ggDataLineWithInput, tapply(linetype, TRT, unique, incomparable = NA_character_))
	expect_type(ltys, "character")
	expect_length(ltys, 2)
	expect_length(unique(ltys), 2)
	
})

test_that("A linetype palette is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statMean = rnorm(4)
	)	
	
	linetypePalette <- c(A = "dotted", B = "dashed")
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		useLinetype = TRUE,
		linetypePalette = linetypePalette
	)
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# extract data behind the lines
	isGeomLine <- sapply(gg$layers, function(l) inherits(l$geom, "GeomLine"))
	ggDataLine <- layer_data(gg, which(isGeomLine))
	
	ggDataLineWithInput <- merge(
		x = summaryTable, y = ggDataLine, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)			
	
	ltys <- with(ggDataLineWithInput, tapply(linetype, TRT, unique))
	expect_type(ltys, "character")
	expect_equal(as.vector(ltys[names(linetypePalette)]), unname(linetypePalette))
	
})

test_that("Shapes are correctly used to differenciate the groups of the color variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = c("A", "A", "B", "B"),
		statMean = rnorm(4),
		stringsAsFactors = TRUE
	)	
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		useShape = TRUE
	)
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# extract data behind the points
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	ggDataPointWithInput <- merge(
		x = summaryTable, y = ggDataPoint, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)		
	
	shapes <- with(ggDataPointWithInput, tapply(shape, TRT, unique, incomparable = NA_character_))
	expect_type(shapes, "integer")
	expect_length(shapes, 2)
	expect_length(unique(shapes), 2)
	
})

test_that("A shape palette is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statMean = rnorm(4)
	)
	
	shapePalette <- c(A = 5, B = 9)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		colorVar = "TRT",
		useShape = TRUE,
		shapePalette = shapePalette
	)
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# extract data behind the points
	isGeomPoint <- sapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"))
	ggDataPoint <- layer_data(gg, which(isGeomPoint))
	
	ggDataPointWithInput <- merge(
		x = summaryTable, y = ggDataPoint, 
		by.x = c("statMean", "TRTN"),
		by.y = c("y", "group"),
		all = TRUE
	)	
	
	shapes <- with(ggDataPointWithInput, tapply(shape, TRT, unique, incomparable = NA_character_))
	expect_type(shapes, "double")
	expect_equal(as.vector(shapes[names(shapePalette)]), unname(shapePalette))
	
})

test_that("Points are correctly labelled with the y-variable values", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		label = TRUE
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomTextRepel"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	ggDataTextWithInput <- merge(
		x = summaryTable, y = ggDataText, 
		by.x = c("visit", "statMean"),
		by.y = c("x", "y"),
		all = TRUE
	)		
	# labels are set to 'statMean' by default
	with(ggDataTextWithInput, 
		expect_equal(object = label, expected = statMean)
	)
	
})

test_that("Points are correctly labelled with an expression of the data variables", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
	
	labelExpr <- bquote(paste(
		"Visit:", visit, "\n", "Mean:", 
		round(statMean, 2)
	))
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		label = labelExpr
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) 
		inherits(l$geom, "GeomTextRepel"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	ggDataTextWithInput <- merge(
		x = summaryTable, y = ggDataText, 
		by.x = c("visit", "statMean"),
		by.y = c("x", "y"),
		all = TRUE
	)		
	
	with(ggDataTextWithInput, expect_equal(
		object = label, 
		expected = eval(labelExpr)
	))
	
})
test_that("An error is generated if the label for the points is not correctly specified", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(4, 5)
	)	
			
	expect_error(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			label = list(a = bquote(statMean))
		),
		"label.*should contain at least 'textLabel'"
	)
			
})

test_that("An error is generated if the text for the label for the points is not an expression", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(4, 5)
	)
	expect_error(
		subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			label = list(textLabel = "blabla")
		),
		"label.*should be a list of expressions"
	)
	
})

test_that("Points are correctly labelled with a text justified based on data variables", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = c(4, 5)
	)
	
	labelExpr <- list(
		textLabel = bquote(paste("Mean:", round(statMean, 2))),
		textHjust = bquote(ifelse(visit == 1, -1, 1)),
		textVjust = bquote(ifelse(statMean == 4, 1, -1))
	)
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		label = labelExpr
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) 
		inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	ggDataTextWithInput <- merge(
		x = summaryTable, y = ggDataText, 
		by.x = c("visit", "statMean"),
		by.y = c("x", "y"),
		all = TRUE
	)		
	
	# labels are correctly extracted
	with(ggDataTextWithInput, expect_equal(
		object = label, 
		expected = eval(labelExpr[["textLabel"]])
	))
	
	# horizontal justification is correct
	with(ggDataTextWithInput, expect_equal(
		object = hjust, 
		expected = eval(labelExpr[["textHjust"]])
	))
	
	# vertical justification is correct
	with(ggDataTextWithInput, expect_equal(
		object = vjust, 
		expected = eval(labelExpr[["textVjust"]])
	))
	
})

test_that("The padding between points and labels is correctly set when specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
			
	labelPadding <- unit(2, "cm")
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		label = TRUE,
		labelPadding = labelPadding
	)
			
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomTextRepel"))
	ggDataText <- layer_data(gg, which(isGeomText))
	expect_identical(
		gg$layers[[which(isGeomText)]]$geom_params$point.padding,
		labelPadding
	)
	
})