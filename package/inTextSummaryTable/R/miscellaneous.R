#' Custom round function, rounding to the closest digits
#' (instead of rounding to the even number in case of 0.5)
#' @param x numeric vector to round
#' @param digits number of digits to consider
#' @return rounded vector
#' @author stackoverflow question 6461209
#' @export
roundCustom <- function(x, digits) {
	posneg <- sign(x)
	z <- abs(x)*10^digits
	z <- z + 0.5
	z <- trunc(z)
	z <- z/10^digits
	z*posneg
}

