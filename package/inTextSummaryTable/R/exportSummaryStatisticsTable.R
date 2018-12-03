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
#' If \code{summaryTable} is a list of summary tables,
#' returns a list of corresponding summary tables in long format.
#' @inherit convertSummaryStatisticsTableToFlextable return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getLabelVar
#' @import officer
#' @importFrom magrittr "%>%"
#' @export
exportSummaryStatisticsTable <- function(
	summaryTable, 
	rowVar = getAttribute(summaryTable, "rowVar"), 
	rowVarLab = getAttribute(summaryTable, "rowVarLab", default = getLabelVar(rowVar, labelVars = labelVars)),
	rowVarInSepCol = NULL, 
	rowTotalInclude = FALSE, rowTotalLab = NULL,
	rowSubtotalInclude = getAttribute(summaryTable, "rowSubtotalInclude", FALSE),
	colVar = NULL, 
	labelVars = NULL, 
	file = NULL, landscape = FALSE, 
	margin = 1, rowPadBase = 2,
	title = NULL,
	footer = NULL,
	outputType = c("flextable", "data.frame"),
	statsLayout = c("row", "col", "rowInSepCol"),
	byVar = NULL){

	outputType  <- match.arg(outputType)
	
	statsLayout <- match.arg(statsLayout)
	
	isListTables <- !is.data.frame(summaryTable)
	
	## format table
	summaryTableLong <- formatSummaryStatisticsTable(
		summaryTable = summaryTable,
		rowVar = rowVar, rowVarLab = rowVarLab,
		rowVarInSepCol = rowVarInSepCol,
		rowTotalInclude = rowTotalInclude, rowTotalLab = rowTotalLab,
		rowSubtotalInclude = rowSubtotalInclude,
		colVar = colVar,
		statsLayout = statsLayout
	)
	
	if(outputType == "flextable" | !is.null(file)){
		
		# create flextable only with header to extract dimensions header
		summaryTableFt <- convertSummaryStatisticsTableToFlextable(
			summaryTable = summaryTableLong,
			landscape = landscape, margin = margin, rowPadBase = rowPadBase,
			title = title, footer = footer
		)
		
		# include the table(s) in a Word document
		if(!is.null(file)){	
			
			doc <- read_docx()
			if(landscape)	doc <- doc %>% body_end_section_landscape()
			
			if(isListTables){
				for(summaryTableFtI in summaryTableFt){
					doc <- doc %>% body_add_flextable(value = summaryTableFtI) %>% body_add_break()
				}
			}else	doc <- doc %>% body_add_flextable(value = summaryTableFt)
			
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