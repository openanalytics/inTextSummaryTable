#' @importFrom clinUtils getColorPalette getShapePalette getLinetypePalette
.onAttach <- function(libname, pkgname) {
  
  # color scheme for tables
  options(inTextSummaryTable.reportColors = getDefaultTableColors("report"))
  options(inTextSummaryTable.presentationColors = getDefaultTableColors("presentation"))
  
  # color scheme for plots
  options(inTextSummaryTable.plotColors = clinUtils::getColorPalette)
  options(inTextSummaryTable.plotShapes = clinUtils::getShapePalette)
  options(inTextSummaryTable.plotLinetypes = getLinetypePalette)
  
}