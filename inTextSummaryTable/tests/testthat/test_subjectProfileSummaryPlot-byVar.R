context("Create a subject profile summary plot by a variable")

library(ggplot2)

test_that("A plot is correctly created by a variable", {
			
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

test_that("A warning is generated if the 'by' variable is not available in the data", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"), 
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)
			
	expect_warning(
		subjectProfileSummaryPlot(
			data = summaryTable,
			xVar = "visit", 
			byVar = "TRT1"
		),
		"'byVar' is not available in the 'data'"
	)
			
})

test_that("Titles are correctly set by group of a variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"),
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)

	title <- c(A = "Treatment A", B = "")	
	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT",
		title = title
	)
	
	for(group in levels(summaryTable$TRT)){
		expect_equal(res[[!!group]]$labels$title, title[[!!group]])
	}
	
})

test_that("Labels for the y-axis are correctly set by group of a variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"),
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)
			
	yLab <- c(A = "Mean treat A", B = "Mean treat B")
			
	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT",
		yLab = yLab
	)
			
	for(group in levels(summaryTable$TRT)){
		expect_equal(res[[!!group]]$labels$y, yLab[[!!group]])
	}
	
})

test_that("Vertical and horizontal lines are correctly set by group of a variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(c("A", "A", "B", "B"), levels = c("B", "A")),
		statMean = rnorm(4)
	)
			
	hLine <- list(A = 1, B = NULL)
	vLine <- list(A = 2, B = 1)
	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT",
		hLine = hLine, vLine = vLine
	)
	
	for(group in levels(summaryTable$TRT)){
			
		expect_equal({
						
			ggGroup <- res[[!!group]]
			isGeomHLine <- sapply(ggGroup$layers, function(l) inherits(l$geom, "GeomHline"))
			
			if(!is.null(hLine[[!!group]])){
				layer_data(ggGroup, which(isGeomHLine))$yintercept
			}
			
			}, 
			expected = hLine[[!!group]]
		)
		
		expect_equal({
					
			ggGroup <- res[[!!group]]
			isGeomVLine <- sapply(ggGroup$layers, function(l) inherits(l$geom, "GeomVline"))
					
			if(!is.null(vLine[[!!group]])){
				layer_data(ggGroup, which(isGeomVLine))$xintercept
			}
					
			}, 
			expected = vLine[[!!group]]
		)
	
	}
			
})

test_that("Extra ggplot specifications are correctly set by group of a variable", {
			
	summaryTable <- data.frame(
		visit = c(1, 2, 1, 2), 
		TRT = factor(
			c("A", "A", "B", "B"),
			levels = c("B", "A")
		),
		statMean = rnorm(4)
	)
		
	legPos <- c(A = "right", B = "left")
	ggExtra <- sapply(legPos, function(x)
		theme(legend.position = x),
		simplify = FALSE
	)
	res <- subjectProfileSummaryPlot(
		data = summaryTable,
		xVar = "visit", 
		byVar = "TRT",
		ggExtra = ggExtra
	)
	
	for(group in levels(summaryTable$TRT)){
		
		expect_equal(
			object = res[[!!group]]$theme$legend.position,
			expected = legPos[[!!group]]
		)
		
	}
	
})				