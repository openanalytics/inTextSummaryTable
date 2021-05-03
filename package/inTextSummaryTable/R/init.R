#' @importFrom clinUtils clinColors clinShapes clinLinetypes
.onAttach <- function(libname, pkgname) {
  
  # color scheme for tables
  options(inTextSummaryTable.colors.table.presentation = tableColorsPresentation)
  
  # color scheme for plots
  options(inTextSummaryTable.colors.plot = clinColors)
  options(inTextSummaryTable.shapes.plot = clinShapes)
  options(inTextSummaryTable.linetypes.plot = clinLinetypes)
  
}