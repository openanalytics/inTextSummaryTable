context("Create a listing")

data <- head(mtcars)

library(flextable)

test_that("An error is generated if no data or flextable object is specified for a listing", {
	expect_error(ft <- getListing())
})

test_that("A listing is correctly styled for a report", {
	ft <- getListing(data = data, style = "report")
	expect_is(ft, "flextable")
	expect_setequal(ft$body$styles$text$font.size$data, 8)
	expect_setequal(ft$body$styles$text$font.family$data, "Times")
})

test_that("A listing is correctly styled for a presentation", {
      
	ftPres <- getListing(data = data, style = "presentation")
	expect_is(ftPres, "flextable")
	expect_setequal(ftPres$body$styles$text$font.size$data, 10)
	expect_setequal(ftPres$body$styles$text$font.family$data, "Tahoma")
      
})

test_that("A warning is generated if columns should be highlighted but no color is specified for the listing", {

	idxHighlight <- c(2, 3)
	expect_warning(
		ftHighlight <- getListing(
			data = data, 
			highlight = idxHighlight, 
			style = "presentation",
			colorTable = NULL
		)
	)
	
})

test_that("Specified columns are correctly highlighted in a listing", {
	
	idxHighlight <- c(2, 3)
	ftHighlight <- getListing(
		data = data, 
		highlight = idxHighlight, 
		style = "presentation",
		includeRownames = FALSE
	)
	# check if background colors in highlighted columns are different in non highlighted columns
	for(el in c("header", "body")){		
        
		bgColors <- ftHighlight[[el]]$styles$cells$background.color$data
		tmp <- apply(bgColors, 1, function(row){
			expect_false(unique(row[idxHighlight]) == unique(row[-idxHighlight]))
		})
        
	}
	
})

test_that("A warning is generated if rownames should be highlighted but rownames should not be included in a listing", {
      
	dataNoRn <- data
	rownames(dataNoRn) <- NULL
	expect_warning(
		ftHighlight <- getListing(
			data = dataNoRn, 
			highlight = 0, 
			style = "presentation",
			includeRownames = FALSE
		)
	)
      
})

test_that("Rownames are correctly highlighted in a listing", {

	dataWithRn <- data
	rownames(dataWithRn) <- seq_len(nrow(dataWithRn))
	ftHighlight <- getListing(
		data = dataWithRn, 
		highlight = 0, 
		style = "presentation",
		includeRownames = TRUE
	)
      
	for(el in c("header", "body")){		
        
		bgColors <- ftHighlight[[el]]$styles$cells$background.color$data
        
		# are rows included?
        expect_equal(ncol(bgColors), ncol(dataWithRn) + 1)
        tmp <- apply(bgColors, 1, function(row){
			expect_false(unique(row[1]) == unique(row[-1]))
		})
        
	}
      
})

test_that("A listing is correctly created in landscape format", {
      
	ftPortrait <- getListing(data = data, landscape = FALSE)
	ftLandscape <- getListing(data = data, landscape = TRUE)
      
	expect_gte(
		object = sum(ftLandscape$body$colwidths), 
		expected = sum(ftPortrait$body$colwidths)
	)
	expect_gte(
		object = sum(ftLandscape$header$colwidths), 
		expected = sum(ftPortrait$header$colwidths)
	)
      
})

test_that("Column widths are properly adjusted depending on page dimensions in a listing", {
      
	widths <- c(2, 50)
	tableWidths <- numeric()
	for(wid in widths){
        
		ft <- getListing(
			data = data, 
 			pageDim = c(wid, 50), margin = 0,
			adjustWidth = TRUE
		)
		expect_equal(sum(ft$body$colwidths), wid)
        
	}
      
})

test_that("A title is correctly included in a listing", {
      
	title <- "Subset of the cars dataset"
	ft <- getListing(data = data, title = title)
	expect_setequal(
		ft$header$dataset[1, ],
		title
	)
	# a border
	expect_setequal(
		ft$header$styles$cells$border.color.bottom$data[1, ],
		c("#000000", "black")
	)
      
})


test_that("A character vector is correctly converted to a binary vector", {
      
	x <- c("group1", "group1", "group1", "group2", "group2", "group3", "group4", "group4")			
	expect_equivalent(
		convertVectToBinary(x = x),
		c(0, 0, 0, 1, 1, 0, 1, 1)
	)
      
})

test_that("Background colors are correctly set based on a variable in a listing", {
      
	data <- data.frame(
		x = c("A", "B", "C", "C"),
		y = seq.int(4)
	)
	colorTable <- getColorPaletteTable(style = "presentation")
	colorTable["bodyBackground1"] <- "#FFFFFF"
	colorTable["bodyBackground2"] <- "#D9D9D9"
	ft <- getListing(
		data = data, bgVar = "x",
		colorTable = colorTable
	)
	
	expect_setequal(
		object = ft$body$styles$cells$background.color$data[c(1, 3, 4), ],
		expected = "#FFFFFF"
	)
	
	expect_setequal(
		object = ft$body$styles$cells$background.color$data[2, ],
		expected = "#D9D9D9"
	)
      
})
