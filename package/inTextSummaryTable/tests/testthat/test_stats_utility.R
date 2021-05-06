context("Utility stats functionalities")

set.seed(123)

test_that("Compute standard error", {
      
	x <- c(1, 2, 3)	
	expect_equal(se(x = x), 1/sqrt(3))
      
})

test_that("Compute standard error with missing value", {
		
	x <- c(1, 2, 3, NA)	
	expect_equal(
		se(x = x, na.rm = FALSE), 
		NA_real_
	)
	
	expect_equal(
		se(x = x, na.rm = TRUE), 
		1/sqrt(3)
	)
			
})

test_that("Compute coefficient of variation", {
			
	x <- c(1, 2, 3)	
	expect_equal(cv(x = x), 50)
			
})

test_that("Compute coefficient of variation with missing value", {
			
	x <- c(1, 2, 3, NA)	
	expect_equal(
		cv(x = x, na.rm = FALSE), 
		NA_real_
	)
			
	expect_equal(
		cv(x = x, na.rm = TRUE), 
		50
	)
			
})


test_that("Compute geometric mean", {
      
	x <- exp(c(1, 2, 3))
	expect_equal(geomMean(x = x), exp(2))
      
})

test_that("Compute geometric mean with missing value", {
	
	x <- exp(c(1, 2, 3, NA))
	expect_equal(
		geomMean(x = x, na.rm = FALSE), 
		NA_real_
	)
			
	expect_equal(
		geomMean(x = x, na.rm = TRUE), 
		exp(2)
	)
			
})

test_that("Compute geometric mean with negative value", {
			
	x <- c(-3, 2)
	expect_warning(gM <- geomMean(x = x))
	expect_true(is.nan(gM))
			
})


test_that("Compute geometric standard deviation", {
			
	x <- exp(c(1, 2, 3))
	expect_equal(geomSD(x = x), exp(1))
			
})

test_that("Compute geometric standard deviation with missing value", {
			
	x <- exp(c(1, 2, 3, NA))
	expect_equal(
		geomSD(x = x, na.rm = FALSE), 
		NA_real_
	)
			
	expect_equal(
		geomSD(x = x, na.rm = TRUE), 
		exp(1)
	)
			
})

test_that("Compute geometric standard deviation with negative value", {
			
	x <- c(-3, -2)
	expect_warning(gSD <- geomSD(x = x))
	expect_equal(gSD, NA_real_)
			
})

test_that("Compute geometric coefficient of variation", {
			
	x <- exp(c(1, 2, 3))
	expect_equal(geomCV(x = x), sqrt(exp(sd(log(x))^2)-1)*100)
	
})

test_that("Compute geometric coefficient of variation with missing value", {
			
	x <- c(1, 2, 3, NA)	
	expect_equal(
		geomCV(x = x, na.rm = FALSE), 
		NA_real_
	)
			
	expect_equal(
		geomCV(x = x, na.rm = TRUE), 
		geomCV(x = c(1, 2, 3))
	)
			
})

test_that("Compute geometric coefficient of variation with negative value", {
			
	x <- c(-3, -2)
	expect_warning(gCV <- geomCV(x = x))
	expect_equal(gCV, NA_real_)
			
})

test_that("Compute geometric standard error of the mean", {
			
	x <- exp(c(1, 2, 3))
	# geomSE is exp of SE of log (x)
	expect_equal(geomSE(x = x), exp(1/sqrt(3)))
			
})


test_that("Compute geometric standard error of the mean with missing value", {
			
	x <- exp(c(1, 2, 3, NA_real_))
	expect_equal(
		geomSE(x = x, na.rm = FALSE), 
		NA_real_
	)
			
	expect_equal(
		geomSE(x = x, na.rm = TRUE), 
		exp(1/sqrt(3))
	)
			
})

