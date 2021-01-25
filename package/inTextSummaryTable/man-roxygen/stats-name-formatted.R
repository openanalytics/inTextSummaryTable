#' @section Formatted statistics:
#' 
#' In the in-text package, the following formatted
#' statistics can be reported in the final output table.
#' \itemize{
#' 
#' \item{for a continuous variable: }{
#' \itemize{
#' 
#' \item{base statistics: }{
#' \itemize{
#' \item{'Mean': }{formatted mean}
#' \item{'Median': }{formatted median}
#' \item{'SE': }{formatted standard error}
#' \item{'SD': }{formatted standard deviation}
#' \item{'Min': }{formatted minimum}
#' \item{'Max': }{formatted maximum}
#' }}
#' 
#' \item{multiple: }{
#' \itemize{
#' \item{'summary-default': }{default set of statistics for a continuous variable:
#'  'n', 'Mean', 'SD', 'SE', 'Median', 'Min', 'Max'}
#' \item{'summary': }{all statistics available for a continuous variable:
#' 'n', 'Mean', 'SD', 'SE', 'Median', 'Min', 'Max', '\%', 'm'}
#' }}
#' 
#' \item{combined statistics: }{
#' \itemize{
#' \item{'median (range)': }{median (minimum,maximum)}
#' \item{'median\\n(range)': }{median and (minimum, maximum) below (linebreak)}
#' \item{'mean (se)': }{mean and standard error}
#' \item{'mean (range)': }{mean and (minimum, maximum)}
#' }}
#' 
#' }}
#' 
#' \item{for a categorical or continuous variable (or the full table): }{
#' \itemize{
#' 
#' \item{base statistics: }{
#' \itemize{
#' \item{'n': }{formatted number of subjects}
#' \item{'m': }{formatted number of records}
#' \item{'\%': }{formatted percentage of subjects}
#' \item{'\%m': }{formatted percentage of records.\cr
#' Note: this is only available if the percentage of records is reported
#' (\code{statsPerc} set to 'statm').}
#' }}
#' 
#' \item{multiple: }{
#' \itemize{
#' \item{'count-default': }{default set of statistics for a categorical variable:
#'  'n', '\%'}
#' \item{'count': }{all statistics available for a categorical variable: 'n', '\%', 'm'}
#' }}
#' 
#' \item{combined statistics: }{
#' \itemize{
#' \item{'n (\%)': }{number of subjects (and associated percentage)}
#' \item{'n/N (\%)': }{number of subjects/total number of subjects (percentage)}
#' \item{'m (\%)': }{number of records (and associated percentage).\cr
#' Note: this is only available if the percentage of records is reported
#' (\code{statsPerc} set to 'statm').}
#' }}
#' 
#' }}
#' #' These statistics are specified via the \code{stats} parameter 
#' (or \code{type} parameter of \code{\link{getStats}}).\cr
#' These statistics are reported as text variables
#' in the summary table (as data.frame),
#' and typically displayed inside the final table.
#' }
