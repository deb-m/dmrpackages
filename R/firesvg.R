#' The firesvg function
#'
#' Print a R Graphics object to a file.
#'
#' @param path file path in quotes.
#' @param y the R graphics object, which needs to be saved as a .svg file.
#' @param w width in inches, defaults to 6
#' @param h height in inches, defaults to 6
#' @keywords firesvg
#' @export
firesvg <- function(path,y,w=6,h=6){
  svg(filename=path,
      width=w,
      height=h,
      pointsize=12
  )
  print(y)
  dev.off()
}
