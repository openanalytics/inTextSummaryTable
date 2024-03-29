#' Arguments used across the functions of the inTextSummaryTable package.
#' @param data Data.frame with dataset to consider for the summary table.
#' @param summaryTable A \code{\link{summaryTable}} object.
#' @param var Character vector with variable(s) of \code{data}, 
#' to compute statistics on.\cr
#' If NULL (by default), counts by row/column variable(s) are computed.\cr
#' To also return counts of the \code{rowVar} in case other \code{var}
#' are specified, you can include: 'all' in the \code{var}.\cr
#' Missing values, if present, are filtered 
#' (also for the report of number of subjects/records).
#' @param varFlag Character vector, subset of \code{var} with variable(s) 
#' of type 'flag' (with 'Y', 'N' or '' for empty/non specified value).
#' Only the counts for records flagged (with 'Y') are retained.
#' @param varLabInclude Logical, if TRUE
#' the name of the summary statistic variable(s) (\code{var})
#' are included in the table.
#' This is automatically set to TRUE if more than one variable(s) 
#' and is specified, and FALSE if only one variable is specified.
#' @param rowVar Character vector with variable(s)
#' to be included in the rows.
#' If multiple variables are specified, the variables should 
#' be sorted in hierarchical order
#' (e.g. body system class before adverse event term)
#' and are nested in the table.
#' @param rowVarInSepCol Character vector with \code{rowVar}
#' that should be included in separated columns.
#' By default (NULL), all row variables are nested in the first column 
#' of the table.\cr
#' To include the groups within a \code{var} variable in a separated column, 
#' set: \code{rowVarInSepCol == 'variableGroup'}.
#' @param rowVarLab Named character vector with 
#' label for the \code{rowVar} variable(s).
#' @param statsVar Character vector with columns of \code{summaryTable} with
#' statistic variables. For the export: if not specified, all columns of \code{data}
#' besides row, column variables, 'variable', 'variableGroup'
#' and 'isTotal' are considered.
#' @param rowVarTotalInclude Character vector with \code{rowVar}
#' for which the total should be reported.\cr
#' If the higher row variable is specified, the total across all rows
#' is reported. \cr
#' For the export, these variable(s) are formatted as factor with 
#' \strong{'Total' as the first level}.
#' @param rowVarTotalInSepRow Character vector with \code{rowVarTotalInclude}
#' (not in \code{rowVarInSepCol}) for which the total should be included in a separated row labelled 'Total'.
#' Otherwise (by default) the total is included in the header row of each category.
#' @param colVar Character vector with variable(s) to be included in columns.
#' If multiple variables are specified, the variables should 
#' be sorted in hierarchical order,
#' and are included in multi-columns layout.\cr
#' Use: 'variable' to include the variables to summarize: \code{var}
#'  (if multiple) in different columns.
#' @param colTotalInclude Logical, if TRUE (FALSE by default) include the summary 
#' statistics across columns in a separated column.
#' @param colTotalLab String, label for the total column  'Total' by default.\cr
#' @param subjectVar String, variable of \code{data} with subject ID,
#' 'USUBJID' by default.
#' @param statsLayout String with layout for the statistics names 
#' (in case more than one statistic is included), among:
#' \itemize{
#' \item{row (by default for 'flextable' output): \cr All statistics are
#'  included in different rows in the first column of the table (after the row variable(s))}
#' \item{'col' (by default for 'DT' output): \cr Statistics are included 
#' in separated columns (last row of the header).\cr
#' This option is not compatible with categorical variable(s).}
#' \item{'rowInSepCol': \cr Statistics are included in different rows, 
#' but in a separated column than the \code{rowVar} variable(s)}
#' }
#' @param statsValueLab String with label for the statistic value, 
#' 'StatisticValue' by default.\cr
#' This is only included in the table if the statistics provided in 
#' \code{stats} are not named and if no \code{colVar} is specified.
#' @param statsExtra (optional) Named list with functions for additional custom
#' statistics to be computed.\cr
#' Each function:
#' \itemize{
#' \item{has as parameter, either: 'x': the variable (\code{var}) to compute
#' the summary statistic on or 'data': the entire dataset}
#' \item{returns the corresponding summary statistic as a numeric vector}
#' }
#' For example, to additionally compute the coefficient of variation, this can be set to:
#' \code{list(statCVPerc = function(x) sd(x)/mean(x)*100)} (or \code{\link{cv}}).
#' @param type String with type of table: 
#' \itemize{
#' \item{'summaryTable': summary table with statistics for numeric variable}
#' \item{'countTable': count table}
#' \item{'auto' (by default): 'summaryTable' if the variable is numeric,
#' 'countTable' otherwise}
#' }
#' @param statsLabInclude Logical, if TRUE include the statistic label
#' in the table. \cr By default only included if more than
#' one statistic variables are available in the table.
#' @param title Character vector with title(s) for the table.
#' Set to NULL (by default) if no title should be included.
#' If multiple are specified, specified for each element of \code{byVar} (in order of the levels).
#' @param pageDim Numeric vector of length 2 with page width and height.\cr
#' Depending on \code{outputType}:
#' \itemize{
#' \item{'flextable': in inches}
#' \item{'DT': in number of rows in the table.\cr
#' Currently only the height is used (e.g. \code{c(NA, 4)})
#' }}
#' @param columnsWidth (expert mode) Column widths of the table. This is only used 
#' for flextable and DT tables.\cr
#' For flextable, note that the widths should be set to fit into the document page
#' (see \code{\link{getDimPage}}).
#' @param labelVars (optional) Named character vector with label for
#' the row, column variable(s) or variable(s) to summarize. \cr
#' Labels specified via dedicated parameter: e.g. 
#' \code{rowVarLab}, \code{colVarLab}, \code{varLab}
#' have priority on this parameter.
#' @name inTextSummaryTable-common-args
#' @return No return value, used for the documentation of R functions
NULL

#' Common arguments for the functionalities of the inTextSummaryTable package
#' for flextable export.
#' @param style (flextable output) String with table style,
#'  either 'report' or 'presentation'.
#' This parameter affects the fontsize, font family, color of the text and background, 
#' and table dimensions of the table.
#' @param rowTotalLab (flextable output) string with label for the row with total.
#' @param rowAutoMerge (flextable output) Logical, if TRUE (by default) automatically merge rows,
#' e.g. in case there is only one sub-category (e.g. categorical variable with only one group)
#' or only one statistic per category.
#' @param rowVarFormat (flextable output) Named list with special formatting for the \code{rowVar}.
#' Currently, only possibility is to set the variable elements in bold, with:
#' list(var1 = "bold").
#' (Use 'variable' for \code{var} or 'variableGroup' for group within categorical variables.)
#' @param rowPadBase (flextable output) Base padding for row (in points), 
#' 14.4 by default (corresponds to 0.2 inches)
#' @param fontname (flextable output) String with font name, by default:
#' 'Times' if \code{style} is 'report' and 'Tahoma' if \code{style} is 'presentation'.
#' @param fontsize (flextable output) Integer with font size, by default:
#' 8 if \code{style} is 'report' and 10 if \code{style} is 'presentation'.
#' @param margin (flextable output) Margin in the document in inches
#' (1 by default).
#' This is used to specify the width of the table, from:
#' [\code{pageDim[1]} - 2 * margin].
#' @param colorTable (flextable output) Named character vector with color for the table background/body/text/line,
#' e.g. created with the \code{\link{getColorPaletteTable}} function.
#' @param landscape (flextable output) Logical, if TRUE the file is in landscape format.\cr
#' By default: FALSE if \code{style} is 'report' and TRUE if \code{style} is 'presentation'.
#' @param footer (flextable output) Character vector with footer(s) for the table.
#' Set to NULL (by default) if no footer should be included.
#' @param vline (flextable output) String mentioning how vertical lines 
#' should be included in the body of the table, either: 
#' \itemize{
#' \item{'none' (default): no vertical lines included}
#' \item{'auto': vertical lines included between sub-groups}
#' }
#' @param hline (flextable output) String mentioning how horizontal lines 
#' should be included in the body of the table, either: 
#' \itemize{
#' \item{'none': no horizontal lines included}
#' \item{'auto' (default): horizontal lines included between sub-groups}
#' }
#' @param file String with path of the file where the table should be exported.
#' The file should have the extension: '.html'.
#' If NULL, the summary table is not exported but only returned as output.
#' If \code{byVar} is specified, each table is exported to a separated
#' file with the suffix: 'file_[i].html' with i the index of the file.
#' @param colHeaderMerge (flextable output) Logical, if TRUE (FALSE by default) 
#' the column header is merged.
#' @name inTextSummaryTable-flextable-args
#' @return No return value, used for the documentation of 
#' R functions for 'flextable' output
NULL

#' Common arguments for the functionalities of the inTextSummaryTable package
#' for DT export.
#' @param expandVar (DT output) Character vector with variables of the summary table which
#' should be expanded in the data.
#' @param pageDim (DT output) Numeric vector of length 2 with page width and height,
#' in number of rows (currently only
#' the height is used (e.g. \code{c(NA, 4)})
#' @param noEscapeVar (DT output) Character vector with variables of \code{summaryTable}
#' which shouldn't be escaped in the table (e.g. containing URLs).
#' @param barVar (DT output) Character vector with variables of \code{summaryTable}
#' that should be represented as a bar.
#' @param file String with path of the file where the table should be exported.
#' The file should have the extension: '.docx'.
#' If NULL, the summary table is not exported but only returned as output.
#' If \code{byVar} is specified, each table is exported to a separated
#' file with the suffix: 'file_[i].docx' with i the index of the file.
#' @name inTextSummaryTable-DT-args
#' @return No return value, used for the documentation of 
#' R functions for 'DT' output
NULL
