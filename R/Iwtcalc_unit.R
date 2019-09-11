
wtcalc.unit <- function(wtLUT, strvar, unit, unitvar, acrevar){
  ## DESCRIPTION: Calculates weights (proportions) of strata by estimation unit

  wtUnit <- wtLUT[wtLUT[,unitvar] %in% unit,]
  strnamesUnit <- unique(wtUnit[,strvar])
  stracres <- tapply(wtUnit[,acrevar], wtUnit[,strvar], sum)
  stracres[!(levels(strnamesUnit) %in% strnamesUnit)] <- 0
  stracres/sum(stracres)
}
