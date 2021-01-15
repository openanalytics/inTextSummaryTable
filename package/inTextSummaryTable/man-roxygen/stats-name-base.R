#' @section Base statistics:
#' 
#' In the in-text package, the following 'base
#' statistics' can reported in the output table.\cr
#' These statistics are reported as numeric and
#' non rounded in the output table, and are
#' typically as input for the formatted statistics,
#' or for plots.
#' 
#' Depending on the type of the variable:
#' \itemize{
#' \item{specific for a continuous variable: }{
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
#' \item{for a categorical variable or for a group/full table}{
#' \itemize{
#' \item{'statN': }{number of subjects}
#' \item{'statPercN' (or 'statPercm'): }{percentage of subjects
#' (or records depending on \code{statsPerc})}
#' \item{'statm': }{number of records}
#' }
#' }
#' }

