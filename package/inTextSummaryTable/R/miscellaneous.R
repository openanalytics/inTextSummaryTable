#' Custom round function, rounding to the closest digits
#' (instead of rounding to the even number in case of 0.5)
#' @param x numeric vector to round
#' @param digits number of digits to consider, 0 by default
#' @return rounded vector
#' @author stackoverflow question 6461209
#' @examples
#' # numbers are rounded to the closest even number in case of .5 
#' # with the round 'base' function
#' round(0.45, 1)
#' # 'roundCustom' round to the closest number
#' roundCustom(0.45, 1)
#' # rounding is the same for uneven number:
#' round(0.55, 1) == roundCustom(0.55, 1)
#' @export
roundCustom <- function(x, digits = 0) {
	posneg <- sign(x)
	z <- abs(x)*10^digits
	z <- z + 0.5
	z <- trunc(z)
	z <- z/10^digits
	z*posneg
}

