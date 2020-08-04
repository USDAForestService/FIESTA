getrhat <- function(x){
  ## DESCRIPTION: Internal function to calculate rhat and variance or rhat.

  ########################################################################################
  ## DESCRIPTION: Calculates the following variables
  ## rhat		- ratio of estimates (numerator/denominator)
  ## rhat.var	- variance of ratio of estimates
  ## rhat.se	- standard error of ratio of estimates { sqrt(rhat.var) }
  ## rhat.cv	- coefficient of variation of ratio of estimates { rhat.se/rhat }
  ## rhat.pse	- percent sampling error of estimates { rhat.cv*100 }
  ########################################################################################


  ## Set global variables
  rhat=rhat.var=rhat.se=rhat.cv=rhat.pse=estn=estn.var=estd=estd.var=est.covar=pse=note <- NULL

  if (!"data.table" %in% class(x))
    x <- data.table(x)

  ## GET RATIO ESTIMATES (rhat) AND VARIANCE OF RATIO ESTIMATES (rhat.var)
  ## Variance (EQ 4.17)
  x[,	rhat := estn / estd][, 
	rhat.var := (estn.var + rhat^2 * estd.var - 2*rhat * est.covar) / estd^2] 	


  ## SET NEW VARIABLE TO FOR RECORDING NOTES
#  x[, note := "ok"]


  x[(estn == 0 & estd == 0), rhat:= 0]
  x[(estn == 0 & estd == 0), rhat.var:= 0]
#  x[(estn == 0 & estd == 0), note:= "Undefined"]

#  x[(estn > 0 & estd > 0 & rhat.var < 0), note := "Negative"]
  x[(estn > 0 & estd > 0 & rhat.var < 0), rhat.var := 0]

  x[,	rhat.se := sqrt(rhat.var)][, 
	rhat.cv := rhat.se / rhat][,
  	pse := rhat.cv * 100]


  ## CHANGE NA VALUES
#  if (sum(is.na(x$pse)) > 0 | any(x$note == "Undefined")) {
#    x[, pse := as.character(pse)]
#    x[is.na(as.numeric(pse)), pse := "--"]
#    #x[note == "Undefined", pse := "*--"]
#  }
#  x[, note:= NULL]

  x[is.na(rhat.cv), rhat.cv := 0]
  x[is.na(pse), pse := 0]

  return(data.table(x))
}

