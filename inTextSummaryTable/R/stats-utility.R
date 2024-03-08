#' Common arguments for the for the statistics utility functions
#' of the inTextSummaryTable package.
#' @param x Numeric vector.
#' @param na.rm Logical, should NA value(s) be removed (FALSE by default)?
#' @name inTextSummaryTable-stats-utility
#' @return No return value, used for the documentation of 
#' stat utility R functions
NULL

#' Compute standard error of the mean.
#' 
#' The standard error of the mean is computed as:
#' \eqn{\frac{\sigma(x)}{\sqrt{length(x)}}}, with:\cr
#' \eqn{\sigma(x)}: standard deviation of \code{x}
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector with standard error of the mean
#' @author Laure Cougnaud
#' @importFrom stats na.omit sd
#' @examples
#' se(rnorm(1000))
#' @family stats utility functions
#' @export
se <- function(x, na.rm = FALSE){
	if(na.rm)	 x <- na.omit(x)
	res <- sd(x)/sqrt(length(x))
	return(res)
}

#' Compute the percentage coefficient of variation,
#' (in a scale from 0 to 100).
#' 
#' The coefficient of variation is computed as:
#' \eqn{\frac{\sigma(x)}{\bar{x}}*100}, with:
#' \itemize{
#' \item{\eqn{\sigma(x)}: standard deviation of \code{x}}
#' \item{\eqn{\bar{x}}: arithmetic mean of \code{x}}
#' }
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector of length 1 with coefficient of variation.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @examples
#' # coefficient of variation of normal distribution tends to 100%
#' cv(rnorm(n = 1000, mean = 1, sd = 1))
#' @family stats utility functions
#' @export
cv <- function(x, na.rm = FALSE){
	
	res <- sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)*100
	return(res)
	
}

#' Compute geometric mean.
#' 
#' The geometric mean is computed as:
#' \eqn{\exp(\bar{log(x)})}, with:
#' \itemize{
#' \item{log: natural logarithm}
#' \item{\eqn{\bar{log(x)}}: arithmetic mean of log(x)}
#' }
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector of length 1 with geometric mean.
#' @author Laure Cougnaud
#' @examples
#' # geometric mean of a big sample from log normal distribution
#' # tends to the mean of the distribution:
#' geomMean(rlnorm(n = 1000, meanlog = 0, sdlog = 1))
#' @family stats utility functions
#' @export
geomMean <- function(x, na.rm = FALSE){
	res <- exp(mean(log(x), na.rm = na.rm))
	return(res)
}

#' Compute geometric standard deviation
#' 
#' The geometric standard deviation is computed as:
#' \eqn{\exp(\sigma(log(x)))}, with:
#' \itemize{
#' \item{log: natural logarithm}
#' \item{\eqn{\sigma}: standard deviation}
#' }
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector of length 1 with geometric mean.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @examples
#' # geometric standard deviation of a sample from a log normal distribution:
#' geomSD(rlnorm(n = 1000, meanlog = 0, sdlog = 1))
#' @family stats utility functions
#' @export
geomSD <- function(x, na.rm = FALSE){
	res <- exp(sd(log(x), na.rm = na.rm))
	return(res)
}

#' Compute geometric coefficient of variation 
#' (in a scale from 0 to 100).
#' 
#' The geometric coefficient of variation is computed as:
#' \eqn{\sqrt{\exp(\sigma(log(x))^2)-1}*100}, with:
#' \itemize{
#' \item{log: natural logarithm}
#' \item{\eqn{\sigma}: standard deviation}
#' }
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector of length 1 with 
#' geometric coefficient of variation.
#' @author Laure Cougnaud
#' @importFrom stats sd
#' @examples
#' # Geometric coefficient of variation of a sample from a log normal distribution:
#' geomCV(rlnorm(n = 1000, meanlog = 0, sdlog = 1))
#' @family stats utility functions
#' @export
geomCV <- function(x, na.rm = FALSE){
	res <- sqrt(exp(sd(log(x), na.rm = na.rm)^2)-1)*100
	return(res)
}

#' Compute geometric standard error of the mean.
#' 
#' The geometric standard error of the mean is computed as:
#' \eqn{\exp(se(log(x)}, with:
#' \itemize{
#' \item{log: natural logarithm}
#' \item{\eqn{se}: standard error of the mean, as computed with \code{\link{se}}}
#' }
#' @inheritParams inTextSummaryTable-stats-utility
#' @return Numeric vector of length 1 with 
#' geometric standard error of the mean.
#' @author Laure Cougnaud
#' @examples
#' # Geometric standard error of the mean of a sample from a log normal distribution:
#' geomSE(rlnorm(n = 1000, meanlog = 0, sdlog = 1))
#' @family stats utility functions
#' @export
geomSE <- function(x, na.rm = FALSE){
	exp(se(log(x), na.rm = na.rm))
}