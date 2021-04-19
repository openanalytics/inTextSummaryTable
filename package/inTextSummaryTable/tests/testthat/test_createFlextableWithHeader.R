context("Create a flextable with header")

library(flextable)

data <- iris

test_that("Create a flextable simple header", {		
			
	# no header
	expect_is(res <- createFlextableWithHeader(data = data), "list")
	expect_is(res$ft, "flextable")
	expect_named(res$colsData)
	expect_identical(setdiff(res$colsData, ""), colnames(data))
	
	# in case ncol(headerDf) != ncol(data)
	headerDf <- as.data.frame(t(colnames(data)))
	expect_error(
		ft <- createFlextableWithHeader(
			data = iris, header = headerDf, 
			includeRownames = TRUE
		)
	)
	
	# simple header without rownames
	expect_is(
		ft <- createFlextableWithHeader(
			data = iris, header = headerDf, 
			includeRownames = FALSE
		)$ft,
		"flextable"
	)
	
	# simple header with rownames
	headerDfWithRownames <- cbind("rownames", headerDf)
	expect_is(
		ft <- createFlextableWithHeader(
			data = iris, header = headerDfWithRownames, 
			includeRownames = TRUE
		)$ft,
		"flextable"
	)
			
})

test_that("Create a flextable with header and title", {

	titles <- c("Table: content of the iris dataset", "This is an informative subtitle")
	headerDf <- as.data.frame(t(colnames(data)))
	expect_is(
		ftWithTitle <- createFlextableWithHeader(
			data = iris, header = headerDf, 
			title = titles,
			includeRownames = FALSE
		)$ft,
		"flextable"
	)
	
	# titles are inserted first in the header data
	for(i in seq_along(titles)){
		expect_setequal(ftWithTitle$header$dataset[i, ], titles[i])
	}
	
	# then header:
	ftHeader <- ftWithTitle$header$dataset[length(titles)+1, ]
	expect_equal(
		as.character(unlist(ftHeader)), 
		as.character(unlist(headerDf)), 
		check.attributes = FALSE
	)

})

test_that("Create a flextable with multiple row/column header", {
			
	# example table with multi-column and multi-row
	multiColTitle <- "Flower characteristics"
	multiRowTitle <- "Species"
	headerDf <- as.data.frame(rbind(
		c(rep(multiColTitle, 4), multiRowTitle),
		colnames(iris)
	), stringsAsFactors = FALSE)
	
	expect_silent(
		ft <- createFlextableWithHeader(
			data = iris, 
			headerDf = headerDf, 
			includeRownames = FALSE
		)$ft
	)
	
	ftHeader <- as.data.frame(ft$header$dataset, stringsAsFactors = FALSE)
	expect_equal(ftHeader, headerDf, check.attributes = FALSE)
	
})
