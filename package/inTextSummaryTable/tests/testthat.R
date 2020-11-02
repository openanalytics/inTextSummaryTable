library(testthat)
library(inTextSummaryTable)

# Note: tests are run in alphabetical order of the scripts in the R folder of the package

# load data required for tests in the package
library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAll <- ADaMDataPelican
labelVars <- labelVarsADaMPelican

dataLB <- ADaMDataPelican$ADLB

if (Sys.getenv("TESTTHAT_OUTPUT_FILE") != "") {
	options(testthat.output_file = Sys.getenv("TESTTHAT_OUTPUT_FILE", stdout()))
	#options(VDIFFR_RUN_TESTS = Sys.setenv("VDIFFR_RUN_TESTS" = TRUE))
}
test_check(
		"inTextSummaryTable",
		reporter = Sys.getenv("TESTTHAT_DEFAULT_CHECK_REPORTER", "check")
)


