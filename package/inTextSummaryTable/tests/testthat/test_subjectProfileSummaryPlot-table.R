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
