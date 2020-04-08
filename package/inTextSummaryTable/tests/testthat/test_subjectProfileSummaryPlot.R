library(vdiffr)

context("Compare 'subjectProfileSummaryPlot' with previous version")

# load data required for tests in the package
library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

dataAll <- ADaMDataPelican
labelVars <- labelVarsADaMPelican

test_that("subjectProfileSummaryPlot - basic plot", {
					
	summaryTableDf <- computeSummaryStatisticsTable(
		data = dataAll$ADPP,
		var = "AVAL",
		rowVar = c("PARCAT1", "PARAM"),
		colVar = c("TRTP", "AVISIT")
	)
			
	# create the plot
	gg <- subjectProfileSummaryPlot(
		data = subset(summaryTableDf, !isTotal),
		xVar = "AVISIT",
		colorVar = "TRTP",
		labelVars = labelVars,
		useLinetype = TRUE,
		facetVar = c("PARCAT1", "PARAM")
	)
			
	vdiffr::expect_doppelganger(
		title = "basic", 
		fig = gg,
		path = "subjectProfileSummaryPlot",
		verbose = TRUE
	)
	
})