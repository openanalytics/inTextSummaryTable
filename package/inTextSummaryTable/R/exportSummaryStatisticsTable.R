#' Export a summary table in \code{docx} format.
#' @param file String with path of the file where the table should be exported.
#' If NULL, the summary table is not exported but only returned as output.
#' @inheritParams formatSummaryStatisticsForExport
#' @inheritParams convertSummaryStatisticsTableToFlextable
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
	colVar = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = FALSE, 
	margin = 1, rowPadBase = 2,
	title = "Table: Descriptive statistics",
	footer = NULL){
	
	## format table
	summaryTableLong <- formatSummaryStatisticsForExport(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		colVar = colVar
	)
	
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
	
	return(summaryTableFt)
	
}