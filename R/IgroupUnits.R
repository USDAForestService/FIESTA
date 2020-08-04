groupUnits <- function(tabest, domain, esttype="AREA", estncol="estn", 
	estncol.var="estn.var", estdcol="estd", estdcol.var="estd.var", 
	covarcol="covar", grpfun=sum, domvar2=NULL, rowgrpnm=NULL, unitvar=NULL, 
	areavar=NULL, phototype="PCT", photoratio=FALSE, keepvars=NULL){

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
			phatcol="rhat", phatcol.var="rhat.var")
    } else {
      tabgrp <- tabest[, list(phat=sum(get(estncol) * WEIGHT), 
			phat.var=sum(get(estncol.var) * WEIGHT^2), 
			NBRPNTS=sum(NBRPNTS)), by=c(domvargrp, keepvars, "TOTAREA")]

      setcolorder(tabgrp, c(domvargrp, keepvars, "phat", "phat.var", "NBRPNTS", "TOTAREA"))
      setnames(tabgrp, "TOTAREA", areavar)
      tabgrp <- FIESTA::PBgetest(tabgrp, areavar=areavar, tabtype=phototype, 
			phatcol="phat", phatcol.var="phat.var")
    }

  } else {
  
    ## Aggregation variables
    agvars <- c(estncol, estncol.var, "NBRPLT", "NBRPLT.gt0")
    agvars <- agvars[which(agvars %in% names(tabest))]

    if (esttype == "RATIO") {
      agvars <- c(agvars, estdcol, estdcol.var, "est.covar")
      agvars <- agvars[which(agvars %in% names(tabest))]
    }
    tabgrp <- tabest[, lapply(.SD, sum, na.rm=TRUE), by=c(domvargrp, keepvars), 
		.SDcols=agvars]
    
    if (esttype == "RATIO") {
      tabgrp <- getrhat(tabgrp)

#      ## Round estimate
#      tabgrp[, rhat := round(rhat, estround)]
#      tabgrp[, pse := round(pse, pseround)]

    } else {
      if (!is.null(estncol.var) && estncol.var %in% names(tabgrp)) {

        ## standard error of the estimated area
        ## the coefficient of variation - percent sampling error of the estimate
        sevar <- paste0(estncol, ".se")
        cvvar <- paste0(estncol, ".cv")
        suppressWarnings(tabgrp[, 
		(sevar) := sqrt(get(estncol.var))][,
		(cvvar) := get(sevar) / get(estncol)][,
  		pse := get(cvvar) * 100]) 
      }
    }
  }

  ## Divide by
#  estn.temp <- paste(estncol, divideby, sep=".")
#  if (!is.null(dividebynum)) 
#    tabgrp[, (estn.temp) := get(estn.temp) / dividebynum]

#  ## Round estimate
#  tabgrp[, (estn.temp) := round(get(estn.temp), estround)]
#  tabgrp[, pse := round(pse, pseround)]

#  tabgrp[is.na(tabgrp)] <- 0
#  setkeyv(tabgrp, domain)
      
  return(tabgrp[order(tabgrp[[domain]]),])
}

