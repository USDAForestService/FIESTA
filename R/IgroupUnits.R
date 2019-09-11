groupUnits <- function(tabest, domain, esttype="AREA", estn="estn", 
	estn.var="estn.var", estd="estd", estd.var="estd.var", 
	covar="covar", grpfun=sum, domvar2=NULL, estround=3, pseround=2, 
	dividebynum=NULL, rowgrpnm=NULL, unitvar=NULL, areavar=NULL, 
	phototype="PCT", photoratio=FALSE, keepvars=NULL){

  ## DESCRIPTION: Internal function to group estimation unit estimates together 
  ## (FOR FIA ESTIMATION)

  ## Set global variables
  pse=rhat=TOTAREA=WEIGHT=rhat.var=NBRPNTS=phat=phat.var=est=est.var <- NULL

  if (!"data.table" %in% class(tabest))
    tabest <- setDT(tabest)

  ## Domain variables
  domvargrp <- domain
  if (!is.null(domvar2)) domvargrp <- c(domvargrp, domvar2)
  if (!is.null(rowgrpnm) && rowgrpnm %in% names(tabest)) 
		domvargrp <- c(domvargrp, rowgrpnm)


  if (esttype == "PHOTO") {
    tabest <- tabest[, TOTAREA := sum(unique(tabest[, c(unitvar, areavar), 
		with=FALSE])[[areavar]])][, WEIGHT := get(areavar) / TOTAREA]

    if (photoratio) {
      tabgrp <- tabest[, list(rhat=sum(rhat * WEIGHT), 
			rhat.var=sum(rhat.var * WEIGHT^2), 
			NBRPNTS=sum(NBRPNTS)), by=c(domvargrp, keepvars)]
      tabgrp <- FIESTA::PBgetest(tabgrp, areavar=areavar, tabtype=phototype, 
			phat="rhat", phat.var="rhat.var")
    } else {
      tabgrp <- tabest[, list(phat=sum(phat * WEIGHT), 
			phat.var=sum(phat.var * WEIGHT^2), 
			NBRPNTS=sum(NBRPNTS)), by=c(domvargrp, keepvars, "TOTAREA")]

      setcolorder(tabgrp, c(domvargrp, keepvars, "phat", "phat.var", "NBRPNTS", "TOTAREA"))
      setnames(tabgrp, "TOTAREA", areavar)
      tabgrp <- FIESTA::PBgetest(tabgrp, areavar=areavar, tabtype=phototype, 
			phat="phat", phat.var="phat.var")
    }

  } else {
  
    ## Aggregation variables
    agvars <- c(estn, estn.var, "NBRPLT", "NBRPLT.gt0")
    agvars <- agvars[which(agvars %in% names(tabest))]

    if (esttype == "RATIO") {
      agvars <- c(agvars, estd, estd.var, "est.covar")
      agvars <- agvars[which(agvars %in% names(tabest))]
    }
    tabgrp <- tabest[, lapply(.SD, sum, na.rm=TRUE), by=c(domvargrp, keepvars), 
		.SDcols=agvars]
    
    if (esttype == "RATIO") {
      tabgrp <- FIESTA::getrhat(tabgrp, estround=estround, pseround=pseround)

      ## Round estimate
      tabgrp[, rhat := round(rhat, estround)]
      tabgrp[, pse := round(pse, pseround)]

    } else {
      if (!is.null(estn.var) && estn.var %in% names(tabgrp)) {

        ## standard error of the estimated area
        ## the coefficient of variation - percent sampling error of the estimate
        sevar <- paste0(estn, ".se")
        cvvar <- paste0(estn, ".cv")
        suppressWarnings(tabgrp[, 
		(sevar) := sqrt(get(estn.var))][,
		(cvvar) := get(sevar) / get(estn)][,
  		pse := get(cvvar) * 100]) 
      }
    }
  }

  ## Divide by
  estn.temp <- estn
  if (!is.null(dividebynum)) 
    tabgrp[, (estn.temp) := get(estn.temp) / dividebynum]

  ## Round estimate
  tabgrp[, (estn.temp) := round(get(estn.temp), estround)]
  tabgrp[, pse := round(pse, pseround)]

  tabgrp[is.na(tabgrp)] <- 0
  setkeyv(tabgrp, domain)
      
  return(tabgrp)
}
