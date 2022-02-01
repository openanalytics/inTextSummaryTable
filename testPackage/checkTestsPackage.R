# Run tests and check tests coverage for the 'patientProfileVis' package
# 
# Author: Laure Cougnaud
###############################################################################

packagePath <- "../inTextSummaryTable/"

## create reference figures for vdiffr::expect_doppelganger

library(vdiffr)

# The same collate statement should be used than the R CMD check
# at the creation of the reference figures with 'manage_cases'
# this affect the order of the elements with 'reorder'
# if different, the order of the parameters in the y-axis of the plot might differ
tmp <- Sys.setlocale(category = "LC_COLLATE", locale = "C")

# create reference figures in 'tests/fig' package
validate_cases(collect_cases(package = packagePath, filter = "subjectProfileSummaryPlot"))

## create the package
library(devtools)
pkgTarballPath <- build(pkg = packagePath, args = "--resave-data")

## check the package
check_built(path = pkgTarballPath)

## check the package coverage:

library(covr)

# test coverage all functions
pc <- package_coverage(
	path = packagePath, 
	type = "tests", 
	quiet = FALSE, clean = FALSE
)
report(
	x = pc, 
	file = paste0("testCoverage-inTextSummaryTable-", packageVersion("inTextSummaryTable"), ".html")
)

