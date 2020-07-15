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
	Sys.setenv("CI" = "testMe")
}
test_check(
		"inTextSummaryTable",
		reporter = Sys.getenv("TESTTHAT_DEFAULT_CHECK_REPORTER", "check")
)


