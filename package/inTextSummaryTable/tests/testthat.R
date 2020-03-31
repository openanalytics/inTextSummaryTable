library(testthat)
library(inTextSummaryTable)

# load data required for tests in the package
library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAll <- ADaMDataPelican
labelVars <- labelVarsADaMPelican

dataLB <- ADaMDataPelican$ADLB

test_check("inTextSummaryTable")



