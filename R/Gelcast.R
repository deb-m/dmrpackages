#' Gelcast function for calculating required amounts of Lamelli SDS-PAGE components
#'
#' Lamelli SDS PAGE is cast using 5 components (30\% Acrylamide solution, Tris-HCl Buffer, Water, TEMED and 10\% APS). The Gelcast function calculates and prints the volumes of these components to be mixed, to cast a particular volume and percentage of running and stacking gels.
#'
#'
#' @param runvol a numeric value representing the final volume of running gel mix required.
#' @param runpc a numeric value representing the running gel percentage required.
#' @param stkvol a numeric value representing the final volume of stacking gel mix required.
#' @param stkpc a numeric value representing the stacking gel percentage required.
#'
#' @keywords Gelcast
#' @export
# @examples
# Gelcast(7,12.5,2,5)


Gelcast <- function(runvol,runpc,stkvol,stkpc){
  running <- data.frame(PC=c(1:5))
  rownames(running) <- c("30%.Acrylamide","1.5M Tris pH8.8","Water","TEMED","10% APS")
  a <- round((runvol*runpc)/30,2)
  b <- round((runvol*0.38)/1.5,2)
  c <- runvol-(a+b)
  d <- runvol
  e <- runvol*10
  a <- paste0(as.character(a)," ml ")
  b <- paste0(as.character(b)," ml ")
  c <- paste0(as.character(c)," ml ")
  d <- paste0(as.character(d)," mcl")
  e <- paste0(as.character(e)," mcl")
  running$PC <-c(a,b,c,d,e)
  colnames(running) <- c(paste0(as.character(runpc),"% Running Gel"))
  running <<- running
  print(running)

  stacking <- data.frame(PC=c(1:5))
  rownames(stacking) <- c("30%.Acrylamide","1M Tris pH6.8","Water","TEMED","10% APS")
  a <- round((stkvol*stkpc)/30,2)
  b <- round((stkvol*0.125)/1,2)
  c <- stkvol-(a+b)
  d <- stkvol
  e <- stkvol*10
  a <- paste0(as.character(a)," ml ")
  b <- paste0(as.character(b)," ml ")
  c <- paste0(as.character(c)," ml ")
  d <- paste0(as.character(d)," mcl")
  e <- paste0(as.character(e)," mcl")
  stacking$PC <-c(a,b,c,d,e)
  colnames(stacking) <- c(paste0(as.character(stkpc),"% Stacking Gel"))
  stacking <<- stacking
  print(stacking)
}


