#' The prest function, which is short for protein estimation.
#'
#' This function takes in a dataframe of "Sample", "Conc" and "A595".
#' The first 12 rows are for 6 standards in duplicates.
#' Depends on BCAplot(), summarySE().
#'
#'
#'
#' @param x row number in the dataframe where the unknown samples start, default is 13.
#' @param y row number in the dataframe where the unknown samples end.
#' @keywords BCAplot, prest, uroot, summarySE
#' @export
# @examples
# prest()

prest <- function(x=13,y){
  m0 <- matrix(d0$A595[1:12],nrow=2)
  d0$A595_bc <- NA
  d0$A595_bc[1:12] <- as.vector(m0-m0[,1])
  d0$A595_bc[x:y] <- d0$A595[x:y] - mean(m0[,1])
  e0 <- summarySE(d0, measurevar = "A595_bc", groupvars =c("Conc", "Sample"))
  e <- e0[grep("STD", e0$Sample), ]
  M1 <<- lm(e, formula = A595_bc ~ poly(Conc,2))
  d0$Conc[x:y]<- vapply( d0$A595_bc[x:y], FUN.VALUE = numeric(1), uroot) *5
  e1 <- summarySE(d0[x:y, ], measurevar = "Conc", groupvars = c("Sample"))
  p1 <- BCAplot(e)
  grid.arrange(p1, ncol=1, top = "", bottom = "")
  e1$Conc <- round(e1$Conc,3)
  e1
}
