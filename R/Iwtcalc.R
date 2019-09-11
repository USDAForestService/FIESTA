wtcalc <- function(wtLUT, strvar, acrevar){
  ## DESCRIPTION: Internal function to calculate weights (proportions) by strata.

  strnames <- unique(wtLUT[,strvar])
  stracres <- tapply(wtLUT[,acrevar], wtLUT[,strvar], sum)
  stracres[!(levels(strnames) %in% strnames)] <- 0
  stracres/sum(stracres)
  }
