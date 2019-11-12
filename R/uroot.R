#' The uroot function
#'
#' Calculates back x from a given y for a polynomial model defined as M1.
#' @param x The numeric ordinate value (absorbance at 595 nm) for which the unknown numeric abcissa value (protein oncentration in mg/ml) is being sought.
#' @keywords uroot
#' @export
# @examples
# uroot()

uroot <- function(x){
  p <- function(M, y) {
    function(x) {predict(M, data.frame(Conc=x)) - y}
  }
  f <- p(M1,x)
  uniroot(f,c(0,2), extendInt = "yes")$root
}
