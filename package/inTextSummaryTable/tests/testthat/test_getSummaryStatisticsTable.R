context("Test 'getSummaryTable' with multiple scenarios")

# load data required for tests in the package
library(glpgUtilityFct)
data(ADaMDataPelican)
data(labelVarsADaMPelican)

# dataset used for the example
dataLB <- ADaMDataPelican$ADLB

# dataset used for the computation of the total (in one of the scenario)
dataSL <- ADaMDataPelican$ADSL
dataSL$TRTP <- dataSL$TRT01P 

dataVS <- ADaMDataPelican$ADVS

dataAE <- ADaMDataPelican$ADAE

## summary statistics table

for(type in c("summaryTable", "countTable")){
	
	for(colInclude0 in c(FALSE, TRUE)){
		
		for(rowInclude0 in c(FALSE, TRUE)){
			
			for(dataTotalSpecified in c(FALSE, TRUE)){

				label <- paste0(
					"type: ", type,
					", include empty rows: ", colInclude0,
					", include empty columns: ", rowInclude0,
					", data total specified: ", dataTotalSpecified
				)
#				print(label)
				
				test_that(label, expect_silent(object = {
						
					getSummaryStatisticsTableCustom <- function(
						data = dataLB, 
						dataTotal = if(dataTotalSpecified)	dataSL, ...){
						argsFct <- c(
							list(
								data = data, labelVars = labelVarsADaMPelican, 
								type = type, 
								rowInclude0 = rowInclude0,
								colInclude0 = colInclude0,
								file = NULL, 
								dataTotal = dataTotal, ...
							),
							if(type == "summaryTable")	list(var = "AVAL")
						)
						do.call(getSummaryStatisticsTable, argsFct)
					}
				
					# base table
					getSummaryStatisticsTableCustom()		
					
					# row variable
					getSummaryStatisticsTableCustom(rowVar = "PARAM") # one
					getSummaryStatisticsTableCustom(rowVar = c("PARCAT1", "PARCAT2", "PARAM"), rowPadBase = 30) # multiple
					
					# column variable
					getSummaryStatisticsTableCustom(colVar = "TRTP") # one
					getSummaryStatisticsTableCustom(colVar = c("TRTP", "AVISIT"), 
						dataTotal = if(dataTotalSpecified)	dataVS
					) # multiple
					
					# row and column
					getSummaryStatisticsTableCustom(rowVar = "PARAM", colVar = "TRTP") # one
					getSummaryStatisticsTableCustom(rowVar = c("PARCAT2", "PARCAT1", "PARAM"), 
						colVar = c("TRTP", "AVISIT"),
						dataTotal = if(dataTotalSpecified)	dataVS) # multiple
					
					# custom stats
					getSummaryStatisticsTableCustom(
						rowVar = c("PARCAT1", "PARCAT2", "PARAM"), 
						colVar = c("TRTP", "AVISIT"), stats = list(expression(paste0(statN, " (", round(statPercN), ")"))),
						rowPadBase = 30,
						dataTotal = if(dataTotalSpecified)	dataVS
					)
					
					# include total row
					getSummaryStatisticsTableCustom(
						rowVar = c("PARCAT1", "PARCAT2", "PARAM"), 
						colVar = "TRTP", stats = list(expression(paste0(statN, " (", round(statPercN), ")"))),
						rowPadBase = 30,
						rowVarTotalInclude = "PARCAT1"
					)
					
					# include subtotal row
					getSummaryStatisticsTableCustom(
						rowVar = c("PARCAT1", "PARCAT2", "PARAM"), 
						colVar = "TRTP", stats = list(expression(paste0(statN, " (", round(statPercN), ")"))),
						rowPadBase = 30,
						rowVarTotalInclude = c("PARCAT2", "PARAM")
					)
					
					# in separated row
					getSummaryStatisticsTableCustom(
						rowVar = c("PARCAT1", "PARCAT2", "PARAM"), 
						colVar = "TRTP", stats = list(expression(paste0(statN, " (", round(statPercN), ")"))),
						rowPadBase = 30,
						rowVarTotalInclude = c("PARCAT2", "PARAM"),
						rowVarTotalInSepRow = c("PARCAT2", "PARAM")
					)
					
				}))
					
			}
			
		}
	
	}
	
}

## test simple table

test_that("simple table var", {
			
	expect_error(
		getSummaryStatisticsTable(
			data = dataSL, 
			rowVar = "AGE", 
			stats = c(getStats("summary")),
			output = "DT"
		),
		pattern = "object 'statMean' not found"
	)
			
	getSummaryStatisticsTable(data = dataSL, var = "AGE", output = "DT")
	# TODO: remove column: '.id'
	getSummaryStatisticsTable(data = dataSL, rowVar = "AGE", output = "DT")
			
})


## tests warnings/errors conditions

test_that("'getSummaryStatisticsTable' contain error/warning tracking", {

	expect_warning(
		getSummaryStatisticsTable(data = dataLB, colTotalInclude = TRUE),
		regexp = "Column 'total' is not included"
	)

})

test_that("'getSummaryStatisticsTable' with only one statistic and statistics in columns work", {
			
	expect_silent(
		getSummaryStatisticsTable(
			data = dataAE, 
			rowVar = c("AESOC", "AEDECOD"),
			stats = getStats("n (%)"), 
			statsLayout = "col"
		)
	)
			
})

test_that("filters work for numeric summary statistic", {
			
	dt <- getSummaryStatisticsTable(
		data = dataAE, 
		rowVar = c("AESOC", "AEDECOD"),
		stats = getStats("n"), 
		statsLayout = "col",
		outputType = "DT"
	)
	
})

test_that("'statsLayout' is: 'col' for categorical variables", {
			
	vars <- c("SEX", "RACE", "AGE")
	stats <- getStatsData(
		data = dataSL,
		type = "default",
		var = vars
	)
#	expect_silent({
				
		# without treatment:
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "DT",
			file = "testTable_statsLayout-col_var-cat.docx"
		)
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "flextable"
		)
		
		# with treatment in column:
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "DT",
			colVar = "TRT01P"
		)
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "flextable",
			colVar = "TRT01P",
			file = "testTable_statsLayout-col_var-cat_colVar.docx",
			landscape = TRUE
		)
		
		# with treatment in row:
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "DT",
			rowVar = "TRT01P"
		)
		dt <- getSummaryStatisticsTable(
			data = dataSL, 
			var = vars,
			stats = stats, 
			statsLayout = "col",
			outputType = "flextable",
			rowVar = "TRT01P"
		)
#	})
			
})


