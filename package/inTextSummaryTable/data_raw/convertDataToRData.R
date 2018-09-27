# Create example dataset for the 'inTextSummaryTable' R package
# 
# Author: Laure Cougnaud
###############################################################################

library(glpgUtilityFct)

dataFiles <- list.files(
	path = "data/Pelican/ADAM",
	pattern = "*.sas7bdat",
	full.names = TRUE
)

dataADaM <- loadDataADaMSDTM(files = dataFiles)

ADaMDataPelicanInText <- dataADaM
save(ADaMDataPelicanInText, file = "../data/ADaMDataPelicanInText.RData")

labelVarsADaMPelicanInText <- attr(dataADaM, "labelVars")
save(labelVarsADaMPelicanInText, file = "../data/labelVarsADaMPelicanInText.RData")
