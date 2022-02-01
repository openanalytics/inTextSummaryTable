context("Test utility statistical functionalities")

test_that("The standard error is correctly computed", {
      
	x <- c(1, 2, 3)	
	expect_equal(se(x = x), 1/sqrt(3))
      
})

test_that("The standard error of data with missing values is correctly computed", {
		
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

test_that("The coefficient of variation is correctly computed", {
			
	x <- c(1, 2, 3)	
	expect_equal(cv(x = x), 50)
			
})

test_that("The coefficient of variation of data with missing values is correctly computed", {
			
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


test_that("The geometric mean is correctly computed", {
      
	x <- exp(c(1, 2, 3))
	expect_equal(geomMean(x = x), exp(2))
      
})

test_that("The geometric mean of data with missing values is correctly computed", {
	
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

test_that("The geometric mean of data with with negative values is correctly computed", {
			
	x <- c(-3, 2)
	expect_warning(gM <- geomMean(x = x))
	expect_true(is.nan(gM))
			
})


test_that("The geometric standard deviation is correctly computed", {
			
	x <- exp(c(1, 2, 3))
	expect_equal(geomSD(x = x), exp(1))
			
})

test_that("The geometric standard deviation of data with missing values is correctly computed", {
			
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

test_that("The geometric standard deviation of data with negative values is correctly computed", {
			
	x <- c(-3, -2)
	expect_warning(gSD <- geomSD(x = x))
	expect_equal(gSD, NA_real_)
			
})

test_that("The geometric coefficient of variation is correctly computed", {
			
	x <- exp(c(1, 2, 3))
	expect_equal(geomCV(x = x), sqrt(exp(sd(log(x))^2)-1)*100)
	
})

test_that("The geometric coefficient of variation of data with missing values is correctly computed", {
			
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

test_that("The geometric coefficient of variation of data with negative values is correctly computed", {
			
	x <- c(-3, -2)
	expect_warning(gCV <- geomCV(x = x))
	expect_equal(gCV, NA_real_)
			
})

test_that("The geometric standard error of the mean is correctly computed", {
			
	x <- exp(c(1, 2, 3))
	# geomSE is exp of SE of log (x)
	expect_equal(geomSE(x = x), exp(1/sqrt(3)))
			
})


test_that("The geometric standard error of the mean of data with missing values is correctly computed", {
			
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

