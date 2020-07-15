library(testthat)
library(inTextSummaryTable)

# load data required for tests in the package
library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAll <- ADaMDataPelican
labelVars <- labelVarsADaMPelican

dataLB <- ADaMDataPelican$ADLB

if (Sys.getenv("TESTTHAT_OUTPUT_FILE") != "") {
	options(testthat.output_file = Sys.getenv("TESTTHAT_OUTPUT_FILE", stdout()))
	options(VDIFFR_RUN_TESTS = Sys.setenv("VDIFFR_RUN_TESTS" = "true"))
}
test_check(
		"inTextSummaryTable",
		reporter = Sys.getenv("TESTTHAT_DEFAULT_CHECK_REPORTER", "check")
)


