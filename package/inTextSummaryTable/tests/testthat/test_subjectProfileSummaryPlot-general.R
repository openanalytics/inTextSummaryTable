context("Create a subject profile summary plot")

library(ggplot2)

test_that("plot fails if variable is not available", {
			
	expect_error(
		subjectProfileSummaryPlot(data = data.frame()),
		"Variable.* not in data"
	)
			
})

test_that("plot is facetted", {
			
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

test_that("facet scale is specified", {
			
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

test_that("plot is created by a variable", {

	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"), 
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)

	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT"
	)
	
	groups <- levels(summaryTable$TRT)
	expect_named(res, groups)
	
	# compare the data behind the plot
	for(group in groups){
		
		expect_identical(
				
			object = ggplot_build(res[[!!group]])$data, 
			
			expected = {
				ggGroup <- subjectProfileSummaryPlot(
					data = subset(summaryTable, TRT == !!group),
					xVar = "visit",
					yLab = paste("Mean", !!group)
				)
				ggplot_build(ggGroup)$data
			}
	
		)
		
	}
	
})