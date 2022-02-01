context("Create a subject profile summary plot with a table")

library(ggplot2)
library(plyr)

test_that("A text variable is correctly set", {
		
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n"
	)
	
	expect_s3_class(gg, "ggplot")
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	expect_identical(
		object = unname(ggDataText[, c("x", "label")]),
		expected = unname(summaryTable)
	)
				
})

test_that("An error is generated if the text variable is not available", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	expect_error(
		subjectProfileSummaryTable(
			data = summaryTable,
			xVar = "visit",
			text = "n2"
		),
		"'n2' should be among the columns of 'data'"
	)
	
})

test_that("A text variable is correctly set as an expression", {
			
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
		object = unname(ggDataText[, c("x", "label")]),
		expected = unname(summaryTable[, c("visit", "label")])
	)
			
})

test_that("The size of the text is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	textSize <- 67
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n", textSize = textSize
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	expect_setequal(
		object = ggDataText$size, 
		expected = textSize
	)
		
})

test_that("The label for the x variable is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20)
	)
	xLab <- "Study visit"
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		xLab = xLab
	)
	expect_identical(
		object = gg$labels$x,
		expected = xLab
	)
			
})

test_that("The x-axis labels are correctly set for a continuous x variable", {
			
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
	expect_equal(
		object = gg$scales$scales[[which(isScaleX)]]$limits, 
		expected = xAxisLabs
	)
	
})

test_that("The x-axis labels are correctly set for a categorical x variable", {
			
	# Note: these labels are not displayed by default
	# but still set in the ggplot object
	summaryTable <- data.frame(
		visit = c("Visit 0", "Visit 1"), 
		n = c(10, 20)
	)
	xAxisLabs <- c(
		`Visit 0` = "Baseline", 
		"Visit 1" = "First visit"
	)
			
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
	expect_equal(
		object = gg$scales$scales[[which(isScaleX)]]$breaks, 
		expected = xAxisLabs
	)
			
})


test_that("The limits are correctly set for the x-axis", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20)
	)
			
	xLim <- c(1, 10)
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		xLim = xLim
	)
	expect_equal(
		object = ggplot_build(gg)$layout$coord$limits$x,
		expected = xLim
	)		
			
})

test_that("The labels of the color variable are correctly displayed in the y-axis", {
		
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = c("1", "2", "3", "4"),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A"))
	)	
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		colorVar = "TRT"
	)
	
	## check if labels are based on color var
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	ggDataText$y <- as.numeric(ggDataText$y) # ggplot set target to 'mapped_discrete' class
	ggDataText <- ggDataText[with(ggDataText, order(x, y)), ]
	
	dataPlotReference <- data.frame(
		x = c(1, 1, 2, 2),
		y = c(1, 2, 1, 2), # 1: bottom (last level), 2: top (first level)
		label = c("1", "2", "3", "4")
	)
	# check that correct data is displayed (and in the correct order)
	expect_equal(
		object = ggDataText[, c("x", "y", "label")],
		expected = dataPlotReference,
		check.attributes = FALSE
	)
	
})
	
test_that("The text and point are colored based on the specified color variable", {
			
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
	
	## color is different for the groups for the text and point (used for the legend)
	
	isGeomTextPoint <- sapply(gg$layers, function(l) inherits(l$geom, c("GeomText", "GeomPoint")))
	ggDataTextPoint <- do.call(plyr::rbind.fill, ggplot_build(gg)$data[isGeomTextPoint])
			
	colors <- with(ggDataTextPoint, tapply(colour, y, unique))
	expect_type(colors, "character")
	expect_length(colors, 2)
	expect_length(unique(colors), 2)
			
})

test_that("A color palette is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = c("A", "B", "A", "B"),
		stringsAsFactors = TRUE
	)	
			
	colorPalette <- c(A = "red", B = "yellow")
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		colorVar = "TRT", colorPalette = colorPalette
	)
			
	# levels of the color variable are sorted from the top (high y) to the bottom (low y)
	summaryTable$y <- as.numeric(factor(summaryTable$TRT, levels = rev(levels(summaryTable$TRT))))
	
	# extract data behind point and text:
	isGeomTextPoint <- sapply(gg$layers, function(l) inherits(l$geom, c("GeomText", "GeomPoint")))
	ggDataTextPoint <- do.call(plyr::rbind.fill, ggplot_build(gg)$data[isGeomTextPoint])
			
	ggDataTextPointWithInput <- merge(ggDataTextPoint, summaryTable,
		by.x = c("x", "y"),
		by.y = c("visit", "y"),
		all = TRUE
	)
			
	colors <- with(ggDataTextPointWithInput, tapply(colour, TRT, unique))
	expect_equal(
		object = as.vector(colors[names(colorPalette)]), 
		expected = unname(colorPalette)
	)
			
})

test_that("A label for the color variable is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = c("A", "B", "A", "B")
	)	
	colorLab <- "Study Treatment"
			
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit", 
		text = "n",
		colorVar = "TRT",
		colorLab = colorLab
	)
			
	# extract color scale
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	expect_equal(
		object = sum(isColorAes), 
		expected = 1
	)
	expect_equal(
		object = ggScales[[which(isColorAes)]]$name, 
		expected = colorLab
	)
			
})

test_that("The size of the points (in the legend) is correctly set", {
			
	# Note: this affect the size of the points in the legend
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20),
		TRT = c("a", "b")
	)
			
	pointSize <- 10
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n", 
		colorVar = "TRT",
		pointSize = pointSize
	)
			
	expect_equal(
		object = gg$guides$colour$override.aes$size, 
		expected = pointSize
	) 
			
})

test_that("The variable labels are correctly extracted from the labels of all variables", {
			
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

test_that("The labels of the y-axis are correctly included with a color variable as a factor", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A", "C", "Z"))
	)
	
	colorPalette <- c(A = "red", B = "blue")
	
	# ggplot2: a (expected) warning is created
	# because of colors specified to text element
	withCallingHandlers(
		expr = {
			gg <- subjectProfileSummaryTable(
				data = summaryTable, 
				xVar = "visit",
				text = "n",
				colorVar = "TRT",
				colorPalette = colorPalette,
				yAxisLabs = TRUE
			)
		},
		warning = function(w){
			if(grepl("Vectorized input", conditionMessage(w)))
				invokeRestart("muffleWarning")
		}
	)
	
	expect_false(inherits(gg$theme$axis.text.y, "element_blank"))
	# check that color of labels are correct in the y-axis
	# Warning: labels are set from the lowest y (last level factor) to the highest y (first level factor)
	expect_equal(
		object = gg$theme$axis.text.y$colour,
		expected = c("red", "blue")
	)
	
})

test_that("A warning is generated if the labels for the y-axis are requested but no color variable is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A", "C", "Z"))
	)
			
	expect_warning(
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			yAxisLabs = TRUE
		),
		"Labels for the y-axis are not included because color variable is not specified."
	)
	
})

test_that("The labels of the y-axis are correctly included with a color variable as a character", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("B", "A", "B", "A"),
		stringsAsFactors = FALSE
	)		
	colorPalette <- c(A = "red", B = "blue")
	
	# ggplot2: a (expected) warning is created
	# because of colors specified to text element
	withCallingHandlers(
		expr = {
			gg <- subjectProfileSummaryTable(
				data = summaryTable, 
				xVar = "visit",
				text = "n",
				colorVar = "TRT",
				colorPalette = colorPalette,
				yAxisLabs = TRUE
			)
		},
		warning = function(w){
			if(grepl("Vectorized input", conditionMessage(w)))
				invokeRestart("muffleWarning")
		}
	)

	# check that color of labels are correct in the y-axis
	# Warning: labels are set from the lowest y (last level factor) to the highest y (first level factor)
	expect_equal(
		object = gg$theme$axis.text.y$colour,
		expected = c("blue", "red")
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	expect_equal(
		unname(c(with(ggDataText, tapply(colour, y, unique)))),
		c("blue", "red")
	)
	
})

test_that("The labels of the y-axis are correctly not included", {

	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("A", "B", "A", "B")
	)
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		colorVar = "TRT",
		yAxisLabs = FALSE
	)
			
	# check if axis labels have been removed
	expect_s3_class(gg$theme$axis.text.y, "element_blank")
			
})

test_that("A color palette is correctly set for the labels of the y-axis ", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("A", "B")
	)
	
	colorPalette <- c(A = "red", B = "blue")
	
	# ggplot2: a (expected) warning is created
	# because of colors specified to text element
	withCallingHandlers(
		expr = {
			gg <- subjectProfileSummaryTable(
				data = summaryTable, 
				xVar = "visit",
				text = "n",
				colorVar = "TRT",
				colorPalette = colorPalette,
				yAxisLabs = TRUE
			)
		},
		warning = function(w){
			if(grepl("Vectorized input", conditionMessage(w)))
				invokeRestart("muffleWarning")
		}
	)
	
	expect_equal(
		object = gg$theme$axis.text.y$colour,
		expected = c("blue", "red")
	)
			
})

test_that("The font size of the text is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	fontsize <- 10
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		fontsize = fontsize
	)
	expect_equal(
		object = gg$theme$text$size, 
		expected = fontsize
	)

})

test_that("The font face of the text is correctly set", {
      
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
      
	fontface <- 3
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		fontface = fontface
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	
	expect_setequal(
		object = ggDataText$fontface, 
		expected = fontface
	)
       
})

test_that("The font of the text is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	fontname <- "Arial"
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		fontname = fontname
	)
	expect_equal(
		object = gg$theme$text$family, 
		expected = fontname
	)
			
})

test_that("A theme is correctly set for the plot", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		themeFct = function()
			ggplot2::theme(aspect.ratio = 0.75)
	)
			
	expect_equal(
		object = gg$theme$aspect.ratio, 
		expected = 0.75
	)
			
})

test_that("A legend is correctly included", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = c("A", "B", "A", "B")
	)	
			
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit", 
		text = "n",
		colorVar = "TRT",
		showLegend = TRUE
	)
	expect_false(gg$theme$legend.position == "none")
	
})

test_that("A legend is correctly not included", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = c("A", "B", "A", "B")
	)
	
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit", 
		text = "n",
		colorVar = "TRT",
		showLegend = FALSE
	)
	expect_equal(
		object = gg$theme$legend.position,
		expected = "none"
	)

})
