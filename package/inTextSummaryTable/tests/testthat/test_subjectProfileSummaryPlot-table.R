context("Create a subject profile summary plot with table")

test_that("a text variable is specified", {
		
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
	
	# error is variable is not available
	expect_error(
		subjectProfileSummaryTable(
			data = summaryTable,
			xVar = "visit",
			text = "n2"
		),
		"'n2' should be among the columns of 'data'"
	)
	
	# correct specification
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n"
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	expect_identical(
		unname(ggDataText[, c("x", "label")]),
		unname(summaryTable)
	)
				
})

test_that("an expression is specified as text", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20),
		TRT = c("A", "B")
	)		
			
	textExpr <- bquote(paste("# patients:", n, "\ntreatment:", TRT))
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = textExpr
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
			
	summaryTable$label <- with(summaryTable, eval(textExpr))
	expect_identical(
		unname(ggDataText[, c("x", "label")]),
		unname(summaryTable[, c("visit", "label")])
	)
			
})

test_that("label is specified for x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20)
	)
	xLab <- "Study visit"
	expect_identical({
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			xLab = xLab
		)
		gg$labels$x
		}, xLab
	)
			
})

test_that("x-axis labels are specified for a continuous x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20)
	)
	xAxisLabs <- c(1, 4)
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		xAxisLabs = xAxisLabs
	)
			
	# extract labels from the ggplot object
	ggScales <- gg$scales$scales
	isScaleX <- sapply(ggScales, function(x) 
		"x" %in% x[["aesthetics"]]
	)
	expect_equal(gg$scales$scales[[which(isScaleX)]]$limits, xLim)
	
})

test_that("a color variable is specified", {
		
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A"))
	)	
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		colorVar = "TRT"
	)
	
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	## check if labels are based on color var
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
		
	ggDataTextWithInput <- merge(ggDataText, summaryTable,
		by.x = c("x", "y"),
		by.y = c("visit", "TRTN"),
		all = TRUE
	)
	
	# label is correct
	with(ggDataTextWithInput, expect_equal(label, n))
	
	## color is different for the groups for the text and point (used for the legend)
	
	isGeomTextPoint <- sapply(gg$layers, function(l) inherits(l$geom, c("GeomText", "GeomPoint")))
	ggDataTextPoint <- do.call(plyr::rbind.fill, ggplot_build(gg)$data[isGeomTextPoint])
			
	ggDataTextPointWithInput <- merge(ggDataTextPoint, summaryTable,
		by.x = c("x", "y"),
		by.y = c("visit", "TRTN"),
		all = TRUE
	)
			
	colors <- with(ggDataTextPointWithInput, tapply(colour, TRT, unique))
	expect_type(colors, "character")
	expect_length(colors, 2)
	expect_length(unique(colors), 2)
			
})

test_that("a color palette is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A"))
	)	
			
	colorPalette <- c(A = "red", B = "yellow")
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		colorVar = "TRT", colorPalette = colorPalette
	)
			
	summaryTable$TRTN <- as.numeric(summaryTable$TRT)
	
	# extract data behind point and text:
	isGeomTextPoint <- sapply(gg$layers, function(l) inherits(l$geom, c("GeomText", "GeomPoint")))
	ggDataTextPoint <- do.call(plyr::rbind.fill, ggplot_build(gg)$data[isGeomTextPoint])
			
	ggDataTextPointWithInput <- merge(ggDataTextPoint, summaryTable,
		by.x = c("x", "y"),
		by.y = c("visit", "TRTN"),
		all = TRUE
	)
			
	colors <- with(ggDataTextPointWithInput, tapply(colour, TRT, unique))
	expect_equal(as.vector(colors[names(colorPalette)]), unname(colorPalette))
			
})

test_that("variable labels specified with 'labelVars'", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("A", "B", "A", "B")
	)
	labelVars <- c(visit = "Study visit", TRT = "Study treatment")
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		colorVar = "TRT",
		labelVars = labelVars
	)
	
	# check label for coloring
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	expect_equal(sum(isColorAes), 1)
	expect_equal(ggScales[[which(isColorAes)]]$name, labelVars["TRT"])
			
})

test_that("y-axis labels are included", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A"))
	)
			
	# y-labels only available if color variable is specified:
	expect_warning(
		subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			yAxisLabs = TRUE
		),
		"Labels for the y-axis are not included because color variable is not specified."
	)

	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		colorVar = "TRT",
		yAxisLabs = TRUE
	)
	
#	expect_equal(
#		object = layer_scales(gg)$y$range$range, 
#		expected = levels(summaryTable$TRT)
#	)
	# check if axis labels have been removed
	expect_true({
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			colorVar = "TRT",
			yAxisLabs = FALSE
		)
		inherits(gg$theme$axis.text.y, "element_blank")
	})
	
	expect_false({
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			colorVar = "TRT",
			yAxisLabs = TRUE
		)
		inherits(gg$theme$axis.text.y, "element_blank")
	})
	
})

