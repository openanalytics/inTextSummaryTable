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
