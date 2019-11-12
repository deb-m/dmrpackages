#' The firepdf function
#'
#' Print a R Graphics object to a file.
#'
#' @param path file path in quotes.
#' @param y the R graphics object, which needs to be saved as a .svg file.
#' @param w width in inches, defaults to 6
#' @param h height in inches, defaults to 4
#' @keywords firepdf
#' @export
firepdf <- function(path,y,w=6,h=4){
  pdf(file=path,
      width=w,
      height=h,
      pointsize=10
      )
  print(y)
  dev.off()
}
