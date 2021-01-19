#' @section Statistics formatting:
#' \itemize{
#'
#' \item{In general, all rounding is handled with \code{\link{roundCustomText}}.}
#'
#' \item{statistics for continuous variable: }{
#' \itemize{
#' \item{if the number of decimals is specified (via \code{nDecCont}): }{
#' \cr statistics are rounded with the following number of decimals:
#' \itemize{
#' \item{'Min', 'Max': }{\code{nDecCont}}
#' \item{'Mean', 'SD', 'Median': }{\code{nDecCont} + 1}
#' \item{'SE': }{\code{nDecCont} + 2}
#' }}
#' \item{if the number of decimals is not specified: }{
#' \cr a default format is set via the \code{\link{formatC}} function.}
#' }}
#' 
#' \item{statistics for counts:}{
#' \itemize{
#' 
#' \item{number of subjects, records are rounded with the number of decimals
#' specified via \code{nDecN} or \code{nDecm} (0 by default)}
#' 
#' \item{percentages are formatted by default with \code{\link{formatPercentage}}.}
#' 
#' \item{'n (\%)' and 'm (\%)': }{ 
#' \itemize{
#' \item{if the percentage of subjects/records is missing, '-' is reported}
#' \item{if the number of subjects/records is 0, '0' is reported instead of '0 (0\%)'}
#' \item{otherwise the number and percentage of subjects/records are formatted as specified}
#' }}
#' 
#' \item{'n/N (\%)': }{
#' \itemize{
#' \item{if the percentage of subjects is missing, '-' is reported}
#' \item{if the number of subjects is 0, '0' is reported instead of '0/... (0\%)'}
#' \item{otherwise the number and percentage of subjects and total are formatted as specified}
#' }}
#' 
#' }}
#' 
#' }
