context("Create a subject profile summary plot: specify labels")

library(ggplot2)

test_that("label is specified for x variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	xLab <- "Study visit"
	expect_identical({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit",
			xLab = xLab
		)
		gg$labels$x
		}, xLab
	)
	
})

test_that("label is specified for y variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)
	yLab <- "Study visit"
			
	# by default: X is used as a title from xVar = stat[X] 
	expect_identical(
		subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit"
		)$labels$y, 
		"Mean"
	)
	
	# custom label
	yLab <- "Mean of the actual values"
	expect_identical({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, xVar = "visit",
			yLab = yLab
		)
		gg$labels$y
		}, yLab
	)
	
})

test_that("title is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
			
	title <- "Example of summary plot"
	expect_equal({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			title = title
		)
		gg$labels$title
	}, title)
			
})

test_that("caption is specified", {
			
	summaryTable <- data.frame(
		visit = c(1, 2), 
		statMean = rnorm(2)
	)	
	
	caption <- "This plot has been created by the in-text package."
	expect_equal({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			caption = caption
		)
		gg$labels$caption
		}, caption
	)
	
})

test_that("variable labels specified with 'labelVars'", {
			
	summaryTable <- data.frame(
		visit = c(1, 1, 2, 2), 
		TRT = c("A", "B", "A", "B"),
		statMean = rnorm(4)
	)
	
	labelVars <- c(visit = "Study visit", TRT = "Study treatment")
	
	expect_silent({
		gg <- subjectProfileSummaryPlot(
			data = summaryTable, 
			xVar = "visit",
			colorVar = "TRT",
			labelVars = labelVars
		)
	})
	
	expect_equal(gg$labels$x, labelVars["visit"])
	
	ggScales <- gg$scales$scales
	isColorAes <- sapply(ggScales, function(x) 
		all(x[["aesthetics"]] == "colour")
	)
	expect_equal(sum(isColorAes), 1)
	expect_equal(ggScales[[which(isColorAes)]]$name, labelVars["TRT"])
	
})
