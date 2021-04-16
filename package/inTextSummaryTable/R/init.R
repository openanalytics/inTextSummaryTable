#' @importFrom clinUtils getColorPalette getShapePalette getLinetypePalette
.onAttach <- function(libname, pkgname) {
  
  # color scheme for tables
  options(inTextSummaryTable.colors.table.report = tableColorsReport)
  options(inTextSummaryTable.colors.table.presentations = tableColorsPresentation)
  
  # color scheme for plots
  options(inTextSummaryTable.colors.plot = getColorPalette)
  options(inTextSummaryTable.shapes.plot = getShapePalette)
  options(inTextSummaryTable.linetypes.plot = getLinetypePalette)
  
}