#' Get color palette for the tables
#' 
#' This function gets the color palettes for the tables 
#' specified as global options.
#' 
#' By default, the function returns the palette of the package. 
#' The user can specify a custom palette by setting the global options.
#' @param style String with style of report. Either 'report' or
#' 'presentation'. By default, the style is 'report'.
#' @return A named vector with hex colors.
#' @examples 
#' # report style (the default)
#' getColorPaletteTable()
#' # presentation style
#' getColorPaletteTable(style = "presentation")
#' # custom palette
#' customColorTable <- c('header' = "#FFFFFF",'headerBackground' = "#3F4788FF", 
#' 'body' = "#000000", 'bodyBackground1' = "#D9D9D9", 'bodyBackground2' = "#D9D9D9", 
#' 'footer' = "#000000", 'footerBackground' = "#FFFFFF",'line' = "#FFFFFF")
#' options(inTextSummaryTable.colors.table.presentation = customColorTable)
#' getColorPaletteTable("presentation")
#' @export 
getColorPaletteTable <- function(style = c("report", "presentation")) {
  
  style <- match.arg(style)
  
  switch(style,
      'report' = tableColorsReport,
      'presentation' = getOption("inTextSummaryTable.colors.table.presentation")
  )
  
  # old 'report' = getOption("inTextSummaryTable.colors.table.report")
  
}


#' Colors for tables in a report style
#' 
#' Default colors are black text on a white background.
#' @export
tableColorsReport <- c(    
    'header'           = "#000000", # black      
    'headerBackground' = "#FFFFFF", # white 
    'body' 			   = "#000000", 
    'bodyBackground'   = "#FFFFFF",
    'footer' 		   = "#000000",
    'footerBackground' = "#FFFFFF",
    'line' 			   = "#000000"
)

#' Colors for tables in a presentation style
#' 
#' Default colors are
#' \itemize{
#' \item{header: white text on a blue background}
#' \item{body: black text on a grey background}
#' \item{footer: black text on a white background.}
#' }
#' @export
tableColorsPresentation <- c(     
    # header:
    'header' 					= "#FFFFFF",    # white text
    'headerBackground' 		    = "#32648EFF",  # lightblue
    'headerBackgroundHighlight' = "#287D8EFF",  # lighter blue
    
    # body black with alternated background color
    'body' 					 	= "#000000",   # black text
    'bodyBackground1' 		 	= "#D9D9D9",   # grey
    'bodyBackground2' 		 	= "#D9D9D9",   # grey
    'bodyBackgroundHighlight1' 	= "#DCE318FF", # yellowish
    'bodyBackgroundHighlight2' 	= "#DCE318FF", # yellowish
    
    # footer: black on white
    'footer' 					= "#000000",   # black text
    'footerBackground' 		 	= "#FFFFFF",   # white background
    # line color
    'line' 					 	= "#FFFFFF"    # white
)
