context("Create a subject profile summary plot with table")

library(ggplot2)
library(plyr)

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
	
	expect_s3_class(gg, "ggplot")
	
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

test_that("size of text is specified", {
			
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
	
	expect_setequal(ggDataText$size, textSize)
		
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
	expect_equal(gg$scales$scales[[which(isScaleX)]]$limits, xAxisLabs)
	
})

test_that("x-axis labels are specified for a discrete x variable", {
			
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
	expect_equal(gg$scales$scales[[which(isScaleX)]]$breaks, xAxisLabs)
			
})


test_that("limit is specified for the x-axis", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20)
	)
			
	xLim <- c(1, 10)
	expect_equal({
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			xLim = xLim
		)
		ggplot_build(gg)$layout$coord$limits$x
		}, 
		xLim
	)		
			
})

test_that("the color variable is displayed in the y-axis", {
		
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
		ggDataText[, c("x", "y", "label")],
		dataPlotReference,
		check.attributes = FALSE
	)
	
})
	
test_that("different colors are used based on a variable", {
			
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

test_that("a color palette is specified", {
			
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
	expect_equal(as.vector(colors[names(colorPalette)]), unname(colorPalette))
			
})

test_that("color label is specified", {
			
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
	expect_equal(sum(isColorAes), 1)
	expect_equal(ggScales[[which(isColorAes)]]$name, colorLab)
			
})

test_that("size of point is specified", {
			
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
			
	expect_equal(gg$guides$colour$override.aes$size, pointSize) 
			
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

test_that("y-axis labels are included with color var as factor", {
	
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = factor(c("A", "B", "A", "B"), levels = c("B", "A", "C", "Z"))
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
	
	# correct spec, with color var as factor:
	colorPalette <- c(A = "red", B = "blue")
	expect_warning(
		gg <- subjectProfileSummaryTable(
			data = summaryTable, 
			xVar = "visit",
			text = "n",
			colorVar = "TRT",
			colorPalette = colorPalette,
			yAxisLabs = TRUE
		)
	)
	expect_false(inherits(gg$theme$axis.text.y, "element_blank"))
	# check that color of labels are correct in the y-axis
	# Warning: labels are set from the lowest y (last level factor) to the highest y (first level factor)
	expect_equal(
		gg$theme$axis.text.y$colour,
		c("red", "blue")
	)
	
})

test_that("y-axis labels are included with color var as character", {
			
	# not as a factor:
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("B", "A", "B", "A"),
		stringsAsFactors = FALSE
	)		
	colorPalette <- c(A = "red", B = "blue")
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		colorVar = "TRT",
		colorPalette = colorPalette,
		yAxisLabs = TRUE
	)
	# check that color of labels are correct in the y-axis
	# Warning: labels are set from the lowest y (last level factor) to the highest y (first level factor)
	expect_equal(
		gg$theme$axis.text.y$colour,
		c("blue", "red")
	)
	
	# extract data behind the text
	isGeomText <- sapply(gg$layers, function(l) inherits(l$geom, "GeomText"))
	ggDataText <- layer_data(gg, which(isGeomText))
	expect_equal(
		unname(c(with(ggDataText, tapply(colour, y, unique)))),
		c("blue", "red")
	)
	
})

test_that("y-axis labels are not included", {

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
	expect_true(inherits(gg$theme$axis.text.y, "element_blank"))
			
})

test_that("y-axis labels are included for a non factor variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		n = c(10, 20),
		TRT = c("A", "B")
	)
	
	colorPalette <- c(A = "red", B = "blue")
	gg <- subjectProfileSummaryTable(
		data = summaryTable, 
		xVar = "visit",
		text = "n",
		colorVar = "TRT",
		colorPalette = colorPalette,
		yAxisLabs = TRUE
	)
	
	expect_equal(
		gg$theme$axis.text.y$colour,
		c("blue", "red")
	)
			
})

test_that("fontsize is specified", {
			
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
	expect_equal(gg$theme$text$size, fontsize)

})

test_that("fontface is specified", {
      
      summaryTable <- data.frame(
          visit = c(1, 2),
          n = c(10, 20)
      )		
      
      fontface <- 3
      gg <- subjectProfileSummaryTable(
          data = summaryTable,
          xVar = "visit",
          text = "n",
          fontface = 2
      )
      expect_equal(gg$labels$fontface, "fontface")
       
    })

test_that("fontname is specified", {
			
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
	
	expect_equal(gg$theme$text$family, fontname)
			
})

test_that("theme is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2),
		n = c(10, 20)
	)		
			
	gg <- subjectProfileSummaryTable(
		data = summaryTable,
		xVar = "visit",
		text = "n",
		themeFct = function() theme(base_size = 30)
	)
			
	expect_equal(gg$theme$base_size, 30)
			
})

test_that("legend is shown", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2),
		n = sample.int(4),
		TRT = c("A", "B", "A", "B")
	)	
			
	expect_equal({
		gg <- subjectProfileSummaryTable(
			data = summaryTable,
			xVar = "visit", 
			text = "n",
			showLegend = FALSE
		)
		gg$theme$legend.position
		}, 
		expected = "none"
	)
	
	expect_false({
		gg <- subjectProfileSummaryTable(
			data = summaryTable,
			xVar = "visit", 
			text = "n",
			showLegend = TRUE
		)
		(gg$theme$legend.position == "none")
		}
	)

})
