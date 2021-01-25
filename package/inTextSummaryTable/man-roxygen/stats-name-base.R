#' @section Base statistics:
#' 
#' In the in-text package, the following 'base
#' statistics' are reported in the summary table:
#' \itemize{
#' \item{for a continuous variable: }{
#' \itemize{
#' \item{'statMean': }{variable mean}
#' \item{'statSD': }{variable standard deviation}
#' \item{'statSE': }{variable standard error}
#' \item{'statMedian': }{variable median}
#' \item{'statMin': }{variable minimum}
#' \item{'statMax': }{variable maximum}
#' }
#' During the computation of the statistics,
#' if multiple and different values are available for a specific variable
#' and subject ID (by row/column): an error is triggered.
#' }
#' \item{for a categorical and continuous variable (or the full table): }{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statm': }{number of records}
#' \item{'statPercN' (or 'statPercm'): }{percentage of subjects
#' (or records) for the specific group}
#' \item{'statPercTotalN' (or 'statPercTotalm'): }{number of subjects 
#' (or records) considered 
#' for the total (denominator) of the percentage}
#' }
#' The percentage and denominator of the percentage are based
#' on the number of subjects or records depending on
#' the \code{statsPerc} parameter.
#' }
#' }
#' These statistics are reported as numeric and
#' non rounded in the summary table, and are
#' typically used as input for the formatted statistics,
#' or for plots.

