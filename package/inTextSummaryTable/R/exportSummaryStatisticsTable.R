#' Export a summary table in \code{docx} format.
#' @param file String with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @param outputType String with output type, 'data.frame' or 'flextable'.
#' @inheritParams formatSummaryStatisticsTable
#' @inheritParams convertSummaryStatisticsTableToFlextable
#' @return Depending on the \code{outputType}:
#' \itemize{
#' \item{'flextable': }{\code{\link[flextable]{flextable}} object with summary table}
#' \item{'data.frame': }{data.frame with summary table}
#' }
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @import officer
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(summaryTable, 
	rowVar = NULL, rowVarLab = getLabelVar(rowVar, labelVars = labelVars),
	rowVarInSepCol = NULL, 
	rowTotalInclude = FALSE, rowTotalLab = NULL,
	rowSubtotalInclude = FALSE,
	colVar = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = FALSE, 
	margin = 1, rowPadBase = 2,
	title = "Table: Descriptive statistics",
	footer = NULL,
	outputType = c("flextable", "data.frame")){

	outputType  <- outputType(outputType)
	
	## format table
	summaryTableLong <- formatSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		rowSubtotalInclude = rowSubtotalInclude,
		colVar = colVar
	)
	
	if(outputType == "flextable" | !is.null(file)){
		
		# create flextable only with header to extract dimensions header
		summaryTableFt <- convertSummaryStatisticsTableToFlextable(
			summaryTable = summaryTableLong,
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer
		)
		
		# include the tables in a Word document
		if(!is.null(file)){	
			
			doc <- read_docx()
			if(landscape)	doc <- doc %>% body_end_section_landscape()
			
			doc <- doc %>% body_add_flextable(value = summaryTableFt)
			
			if(landscape){
				doc <- doc %>%
					# a paragraph needs to be included after the table otherwise the layout is not landscape
					body_add_par(value = "", style = "Normal") %>%
					body_end_section_landscape()
			}
			print(doc, target = file)
			
		}
		
		
	}
	result <- switch(outputType,
		'data.frame' = summaryTableLong,	
		'flextable' = summaryTableFt
	)
	
	return(result)
	
}