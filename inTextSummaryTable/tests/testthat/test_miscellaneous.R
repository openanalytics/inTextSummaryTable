context("Test miscellaneous functions")

test_that("A variable is correctly converted to a flag variable", {
      
	x <- c("Y", "N", '')	
	xFL <- inTextSummaryTable:::convertVarFlag(x)
	expect_is(xFL, "factor")
      
	# correct levels
	expect_equivalent(levels(xFL), c("Y", "N"))
      
	# correct conversions
	expect_equal(as.character(xFL), c("Y", "N", NA_character_))
	
})

test_that("An error is generated if a flag variable contains missing values", {
      
	x <- c("Y", "N", '')
	expect_error(
		inTextSummaryTable:::convertVarFlag(c(NA_character_, x)), 
		pattern = "*should only contain*"
	)
	
})

test_that("An error is generated if a flag variable contains unexpected elements", {
			
	expect_error(
		object = inTextSummaryTable:::convertVarFlag(c("blabla", x)), 
		pattern = "*should only contain*"
	)

})

test_that("An error is generated if the specified page dimensions are not a numeric", {
      
	expect_error(
		getDimPage(type = "width", pageDim = "ciao"),
		"'pageDim' should be a numeric vector."
	)
	
})
      
test_that("An error is generated if the specified page dimensions are not of correct length", {
			
	expect_error(
		getDimPage(type = "width", pageDim = c(10, 2, 3)),
		"'pageDim' should be of length 2."
	)
      
})

test_that("The page width is correctly extracted for a report", {
      
	expect_equal(
		object = getDimPage(type = "width", style = "report", margin = 0),
		expected = 21/2.54
	)
	
})

test_that("The page height is correctly extracted for a report", {
			
	expect_equal(
		object = getDimPage(type = "height", style = "report", margin = 0),
		expected = 29.7/2.54
	)
      
})

test_that("The page dimensions are correctly extracted (and in correct order) if multiple dimensions are specified", {
      
	expect_equal(
		object = getDimPage(type = c("height", "width"), style = "report"),
		expected = rev(getDimPage(type = c("width", "height"), style = "report"))
	)
      
})

test_that("The page width is correctly extracted for a presentation", {
      
	expect_equal(
		object = getDimPage(type = "width", style = "presentation", margin = 0),
		expected = 10
	)
	
})

test_that("The page height is correctly extracted for a presentation", {
			
	expect_equal(
		object = getDimPage(type = "height", style = "presentation", margin = 0),
		expected = 7.5
	)
      
})

test_that("The dimensions in portrait and landscape match", {
      
	expect_equal(
		object = getDimPage(type = "width", style = "report", margin = 0, landscape = FALSE),
		expected = getDimPage(type = "height", style = "report", margin = 0, landscape = TRUE)
	)
	expect_equal(
		object = getDimPage(type = "height", style = "report", margin = 0, landscape = FALSE),
		expected = getDimPage(type = "width", style = "report", margin = 0, landscape = TRUE)
	)
      
})

test_that("The page width is correctly set if a margin is specified", {
      
	widthPageNoMargin <- getDimPage(type = "width", style = "report", margin = 0)
	expect_equal(
		getDimPage(type = "width", style = "report", margin = 3),
		expected = widthPageNoMargin-3*2
	)
	
})

test_that("The page height is correctly set if a margin is specified", {
			
	heightPageNoMargin <- getDimPage(type = "height", style = "report", margin = 0)
	expect_equal(
		getDimPage(type = "height", style = "report", margin = 3),
		expected = heightPageNoMargin-3*2
	)
      
})

test_that("The page width is correctly set when page dimensions are specified", {
      
	pageDimSpecified <- c(10, 20)
	expect_equal(
		getDimPage(type = "width", style = "report", margin = 0, pageDim = pageDimSpecified),
		expected = pageDimSpecified[1]
	)
	
})

test_that("The page height is correctly set when page dimensions are specified", {
			
	pageDimSpecified <- c(10, 20)
	expect_equal(
		getDimPage(type = "height", style = "report", margin = 0, pageDim = pageDimSpecified),
		expected = pageDimSpecified[2]
	)	
      
})