
#' Get the default colors of the table
#' 
#' @param style style
#' @export 
getDefaultTableColors <- function(style = c("report", "presentation")){
  
  style <- match.arg(style)
    
  colorTable <- switch(style,
      'report' = getTableColorsReport(),
      'presentation' = getTableColorsPresentation()
  )
  
  return(colorTable)
  
}

getTableColorsReport <- function() {
  
  c(    
      'header'           = "#000000", # black      
      'headerBackground' = "#FFFFFF", # white 
      'body' 			 = "#000000", 
      'bodyBackground'   = "#FFFFFF",
      'footer' 			 = "#000000",
      'footerBackground' = "#FFFFFF",
      'line' 			 = "#000000"
  )
  
}

getTableColorsPresentation <- function() {
  
  c(     
      # header:
      'header' 					  = "#FFFFFF",    # white text
      'headerBackground' 		  = "#32648EFF",  # lightblue
      'headerBackgroundHighlight' = "#287D8EFF",  # lighter blue
      
      # body black with alternated background color
      'body' 					 = "#000000",   # black text
      'bodyBackground1' 		 = "#D9D9D9",   # grey
      'bodyBackground2' 		 = "#D9D9D9",   # grey
      'bodyBackgroundHighlight1' = "#DCE318FF", # yellowish
      'bodyBackgroundHighlight2' = "#DCE318FF", # yellowish
      
      # footer: black on white
      'footer' 					 = "#000000",   # black text
      'footerBackground' 		 = "#FFFFFF",   # white background
      # line color
      'line' 					 = "#FFFFFF"    # white
  )
  
}