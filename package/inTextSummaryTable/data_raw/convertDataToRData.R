# Project: GLPGPatientProfiles
# 
# Author: Laure Cougnaud
###############################################################################

library(glpgUtilityFct)

dataFiles <- "../../../../../GLPGSAPCysticFibrosis/data/Pelican/ADAM/20180525 study/adre.sas7bdat"

dataADaM <- loadDataADaMSDTM(files = dataFiles)

#res <- getTimeVsReference(dataList)

ADaMDataPelican <- dataADaM$ADRE
save(ADaMDataPelican, file = "../data/ADaMDataPelican.RData")

labelVarsADaMPelican <- attr(dataADaM, "labelVars")
save(labelVarsADaMPelican, file = "../data/labelVarsADaMPelican.RData")
