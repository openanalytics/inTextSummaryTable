context("Create a flextable with header")

library(flextable)

test_that("A header is correctly set by default from the data header to a flextable", {		
			
	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2)
	)
	res <- createFlextableWithHeader(data = data)
	expect_is(res, "list")
	expect_is(res$ft, "flextable")
	# table header
	expect_equal(
		object = as.character(as.vector(res$ft$header$dataset[1, ])),
		expected = c("", colnames(data))
	)
	# original column names
	expect_equal(
		object = unname(res$colsData),
		expected = c("", colnames(data))
	)
	
})

test_that("An error is generated if the flextable header is not specified for the rows when rownames are requested", {		
			
	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2)
	)
	
	# in case ncol(headerDf) != ncol(data)
	headerDf <- data.frame(
		V1 = "subject ID", 
		V2 = "x variable"
	)
	expect_error(
		ft <- createFlextableWithHeader(
			data = data, header = headerDf, 
			includeRownames = TRUE
		)
	)
	
})

test_that("A header is correctly set from a specified dataset to a flextable", {		
			
	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2)
	)
	headerDf <- data.frame(	
		V1 = "subject ID", 
		V2 = "x variable",  
		stringsAsFactors = FALSE
	)
	res <- createFlextableWithHeader(
		data = data, header = headerDf, 
		includeRownames = FALSE
	)
	# table header
	expect_equal(
		object = as.character(as.vector(res$ft$header$dataset[1, ])),
		expected = as.character(as.vector(headerDf[1, ]))
	)
	# original column names
	expect_equal(
		object = unname(res$colsData),
		expected = colnames(data)
	)

})

test_that("A header is correctly set from a specified dataset to a flextable with row names", {		
			
	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2)
	)
	headerDf <- data.frame(
		V0 = "rownames", 
		V1 = "subject ID", 
		V2 = "x variable", 
		stringsAsFactors = FALSE
	)
	res <- createFlextableWithHeader(
		data = data, header = headerDf, 
		includeRownames = TRUE
	)
	# table header
	expect_equal(
		object = as.character(as.vector(res$ft$header$dataset[1, ])),
		expected = as.character(as.vector(headerDf[1, ]))
	)
	# original column names
	expect_equal(
		object = unname(res$colsData),
		expected = c("", colnames(data))
	)
			
})

test_that("A header and title are correctly set to a flextable", {

	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2)
	)
	headerDf <- data.frame(
		V1 = "subject ID", 
		V2 = "x variable", 
		stringsAsFactors = FALSE
	)	
	titles <- c(
		"Table: content of the iris dataset", 
		"This is an informative subtitle"
	)
	res <- createFlextableWithHeader(
		data = data, header = headerDf, 
		title = titles,
		includeRownames = FALSE
	)
	expect_is(res$ft, "flextable")
	
	## table header
	
	# titles are inserted first in the header data
	for(i in seq_along(titles)){
		expect_setequal(
			object = res$ft$header$dataset[i, ], 
			expected = titles[i]
		)
	}
	
	# then header:
	ftHeader <- res$ft$header$dataset[length(titles)+1, ]
	expect_equal(
		as.character(as.vector(ftHeader)), 
		as.character(as.vector(headerDf)), 
		check.attributes = FALSE
	)

	## original column names
	expect_equal(
		object = unname(res$colsData),
		expected = colnames(data)
	)
	
})

test_that("A header spanning multiple rows and columns is correctly set to a flextable", {
	
	data <- data.frame(
		USUBJID = seq.int(2),
		x = c(1, 2),
		y = c(4, 5),
		z = c(0, 1)
	)
			
	# example table with multi-column and multi-row
	headerDf <- rbind.data.frame(
		c("subject ID", rep("Analysis values", 3)),
		c("subject ID", "x analyte", "y analyte", "z analyte"),
		stringsAsFactors = FALSE
	)
	colnames(headerDf) <- c("V1", "V2", "V3", "V4")
	
	res <- createFlextableWithHeader(
		data = data, 
		headerDf = headerDf, 
		includeRownames = FALSE
	)
	
	## table header
	expect_equal(
		object = unname(res$ft$header$dataset),
		expected = unname(headerDf)
	)
	
	## spanning header
	headerSpan <- res$ft$header$spans
	
	# (multi-row) first column span 2 rows
	expect_equal(
		object = headerSpan$columns[, 1],
		expected = c(2, 0)
	)
	expect_setequal(
		object = headerSpan$columns[, -1],
		expected = 1
	) # others
	# (multi-column) first row of second column span 3 columns
	expect_equal(
		object = headerSpan$rows[1, 2:4],
		expected = c(3, 0, 0)
	)
	expect_setequal(
		object = c(headerSpan$rows[-1, ], headerSpan$rows[1, 1]),
		expected = 1
	)# others
	
	## original column names
	expect_equal(
		object = unname(res$colsData),
		expected = colnames(data)
	)
	
})
