.onAttach <- function(libname, pkgname) {
  options(inTextSummaryTable.reportColors = getDefaultTableColors("report"))
  options(inTextSummaryTable.presentationColors = getDefaultTableColors("presentation"))
}