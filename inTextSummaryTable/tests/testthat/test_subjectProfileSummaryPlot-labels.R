context("Create a subject profile summary plot with labels")

library(ggplot2)

test_that("A label is correctly specified for a x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	xLab <- "Study visit"
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, xVar = "visit",
		xLab = xLab
	)
	expect_identical(
		object = gg$labels$x,
		expected = xLab
	)
	
})

test_that("The label for the y variable is correctly set by default", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	# by default: X is used as a title from xVar = stat[X] 
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, xVar = "visit"
	)
	expect_identical(
		object = gg$labels$y, 
		expected = "Mean"
	)

})

test_that("A label is correctly set for a y variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	yLab <- "Mean of the actual values"
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, xVar = "visit",
		yLab = yLab
	)
	expect_identical(
		object = gg$labels$y, 
		expected = yLab
	)
	
})

test_that("A title is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
			
	title <- "Example of summary plot"
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		title = title
	)
	expect_equal(
		object = gg$labels$title,
		expected = title
	)
			
})

test_that("A caption is correctly set", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
	
	caption <- "This plot has been created by the in-text package."
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		caption = caption
	)
	expect_equal(
		object = gg$labels$caption,
		expected = caption
	)
	
})

test_that("The variable labels are correctly extracted from the labels of all variables", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2), 
		TRT = c("A", "B", "A", "B"),
		statMean = rnorm(4)
	)
	
	labelVars <- c(visit = "Study visit", TRT = "Study treatment")
	
	gg <- subjectProfileSummaryPlot(
		data = summaryTable, 
		xVar = "visit",
		colorVar = "TRT",
		labelVars = labelVars
	)
	
	expect_equal(gg$labels$x, labelVars["visit"])
	
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	expect_equal(sum(isColorAes), 1)
	expect_equal(ggScales[[which(isColorAes)]]$name, labelVars["TRT"])
	
})
