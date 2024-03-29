% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/flextable-formatSummaryStatisticsTableFlextable.R
\name{formatSummaryStatisticsTableFlextable}
\alias{formatSummaryStatisticsTableFlextable}
\title{Merge nested rows of a summary table
for a format compatible with \code{flextable}}
\usage{
formatSummaryStatisticsTableFlextable(
  summaryTable,
  rowVar = getAttribute(summaryTable, "rowVar"),
  rowVarInSepCol = NULL,
  rowVarTotalInclude = getAttribute(summaryTable, "rowVarTotalInclude"),
  statsLayout = "row",
  statsVar = getAttribute(summaryTable, "statsVar"),
  statsLabInclude = getAttribute(summaryTable, "statsLabInclude", default =
    length(statsVar) > 1),
  rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar,
    labelVars = labelVars)),
  rowVarTotalInSepRow = NULL,
  vline = c("none", "auto"),
  hline = c("none", "auto"),
  rowAutoMerge = TRUE,
  rowVarFormat = NULL,
  rowTotalLab = NULL,
  labelVars = NULL
)
}
\arguments{
\item{summaryTable}{A \code{\link{summaryTable}} object.}

\item{rowVar}{Character vector with variable(s)
to be included in the rows.
If multiple variables are specified, the variables should 
be sorted in hierarchical order
(e.g. body system class before adverse event term)
and are nested in the table.}

\item{rowVarInSepCol}{Character vector with \code{rowVar}
that should be included in separated columns.
By default (NULL), all row variables are nested in the first column 
of the table.\cr
To include the groups within a \code{var} variable in a separated column, 
set: \code{rowVarInSepCol == 'variableGroup'}.}

\item{rowVarTotalInclude}{Character vector with \code{rowVar}
for which the total should be reported.\cr
If the higher row variable is specified, the total across all rows
is reported. \cr
For the export, these variable(s) are formatted as factor with 
\strong{'Total' as the first level}.}

\item{statsLayout}{String with layout for the statistics names 
(in case more than one statistic is included), among:
\itemize{
\item{row (by default for 'flextable' output): \cr All statistics are
 included in different rows in the first column of the table (after the row variable(s))}
\item{'col' (by default for 'DT' output): \cr Statistics are included 
in separated columns (last row of the header).\cr
This option is not compatible with categorical variable(s).}
\item{'rowInSepCol': \cr Statistics are included in different rows, 
but in a separated column than the \code{rowVar} variable(s)}
}}

\item{statsVar}{Character vector with columns of \code{summaryTable} with
statistic variables. For the export: if not specified, all columns of \code{data}
besides row, column variables, 'variable', 'variableGroup'
and 'isTotal' are considered.}

\item{statsLabInclude}{Logical, if TRUE include the statistic label
in the table. \cr By default only included if more than
one statistic variables are available in the table.}

\item{rowVarLab}{Named character vector with 
label for the \code{rowVar} variable(s).}

\item{rowVarTotalInSepRow}{Character vector with \code{rowVarTotalInclude}
(not in \code{rowVarInSepCol}) for which the total should be included in a separated row labelled 'Total'.
Otherwise (by default) the total is included in the header row of each category.}

\item{vline}{(flextable output) String mentioning how vertical lines 
should be included in the body of the table, either: 
\itemize{
\item{'none' (default): no vertical lines included}
\item{'auto': vertical lines included between sub-groups}
}}

\item{hline}{(flextable output) String mentioning how horizontal lines 
should be included in the body of the table, either: 
\itemize{
\item{'none': no horizontal lines included}
\item{'auto' (default): horizontal lines included between sub-groups}
}}

\item{rowAutoMerge}{(flextable output) Logical, if TRUE (by default) automatically merge rows,
e.g. in case there is only one sub-category (e.g. categorical variable with only one group)
or only one statistic per category.}

\item{rowVarFormat}{(flextable output) Named list with special formatting for the \code{rowVar}.
Currently, only possibility is to set the variable elements in bold, with:
list(var1 = "bold").
(Use 'variable' for \code{var} or 'variableGroup' for group within categorical variables.)}

\item{rowTotalLab}{(flextable output) string with label for the row with total.}

\item{labelVars}{(optional) Named character vector with label for
the row, column variable(s) or variable(s) to summarize. \cr
Labels specified via dedicated parameter: e.g. 
\code{rowVarLab}, \code{colVarLab}, \code{varLab}
have priority on this parameter.}
}
\value{
summaryTable reformatted in long format, with extra attributes:
\itemize{
\item{'header': data.frame with header for each column}
\item{'padParams': list of list of parameters to be passed to the 
\code{\link[flextable]{padding}} function}
\item{'rowVar': column of output with row variable}
\item{'rowVarInSepCol': column(s) of output with row variable in separated column(s)}
\item{'vlineParams' and 'hlineParams': 
list of list with correspondingly parameters for
vertical and horizontal lines
}
\item{'vline': \code{vline} parameter}
\item{'formatParams': list of list with special formatting for the table,
currently only used if \code{rowVarFormat} if specified.}
}
If \code{summaryTable} is a list of summary tables,
returns a list of corresponding summary tables in long format.
}
\description{
Merge nested rows of a summary table
for a format compatible with \code{flextable}
}
\author{
Laure Cougnaud
}
