#' @importFrom clinUtils clinColors clinShapes clinLinetypes
.onAttach <- function(libname, pkgname) {
  
  # options for tables in presentation mode
  options(inTextSummaryTable.pageDim.presentation = pageDimPresentation)
  options(inTextSummaryTable.colors.table.presentation = tableColorsPresentation)
  
  # palettes for plots
  options(inTextSummaryTable.colors.plot = clinColors)
  options(inTextSummaryTable.shapes.plot = clinShapes)
  options(inTextSummaryTable.linetypes.plot = clinLinetypes)
  
}