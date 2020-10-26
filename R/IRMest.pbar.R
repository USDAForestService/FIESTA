RMest.pbar <- function(sumyn="CONDPROP_ADJ", ysum, sumyd, bytdom=FALSE, uniqueid, 
 	strlut, unitvar, strvar, domain){
  ########################################################################################
  ## DESCRIPTION: Ratio-to-size estimators
  ## ARGUMENTS:
  ## sumyn		- string. estimation response
  ## ysum		- data frame. domain-level aggregated dataset
  ## sumyd 		- string. estimation response (denominator) - for ratio of means estimates
  ## bytdom		- logical. if TRUE, estimates are by tree domains (e.g., species)
  ## uniqueid 	- unique plot identifier in ysum
  ## strlut		- data.frame. estimation unit/strat -level information
  ## unitvar 	- name of variable defining estimation unit
  ## domain		- name of variable defining domain (e.g., forest type)
  ## areavar 	- name of variable defining area
  ##
  ## VALUE:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## rhat		- ratio domain filter
  ## rhat.var	- variance of ratio domain filter
  ########################################################################################
  strunitvars <- c(unitvar, strvar)

  ## Set global variables
  sumyn.pltdom=sumyd.pltdom=nhat=dhat=sumyn.dom=sumyd.dom=nhat.var=dhat.var=
	n.total=n.strata=prop.total=prop.plt=value=rhat <- NULL

  ## Set key for est.unit (note: to run separate)
  est.unit <- setDT(est.unit)
  ysum <- setDT(ysum)
  setkeyv(est.unit, unitvar)


  if (!"n.strata" %in% names(strlut)) stop("need n.strata in strlut")
  if (!"n.total" %in% names(strlut)) stop("need n.total in strlut")


  ## STRATA/PLOT/DOMAIN LEVEL: Change name and square sum and multiply sum for denominator 
  ysum[, ':=' (sumyn.pltdom = get(sumyn), sumyd.pltdom = get(sumyd))]

  ## STRATA/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/domain
  ysum.strata <- ysum[, list(sumyn.dom=sum(sumyn.pltdom, na.rm=TRUE),
		sumyd.dom=sum(sumyd.pltdom, na.rm=TRUE)), by=c(strunitvars, domain)]
  setkeyv(ysum.strata, strunitvars)
 

  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  setkeyv(strlut, strunitvars)
  ybardat <- strlut[ysum.strata]


  ## Calculate mean for numerator and denominator
  #########################################################
  #ybardat[, nhat.strwt := sumyn.dom/n.strata * strwt]
  #ybardat[, dhat.strwt := sumyd.dom/n.strata * strwt]
  ybardat[, nhat := sumyn.dom/n.strata]
  ybardat[, dhat := sumyd.dom/n.strata]


  ## Define names for calculations
  #########################################################
  ybardat[[domain]] <- as.character(ybardat[[domain]])
  domnames <- ybardat[[domain]]
  domnames.yn <- paste0("yn_", domnames)
  domnames.yd <- paste0("yd_", domnames)
  domnames.n <- paste0("n_", domnames)
  domnames.d <- paste0("d_", domnames)

  

  ## Calculate mean and variance by domain - numerator
  #########################################################
  ## Close, but not correct - need to consult with Paul
  # ybardat[, nhat.var.strwt := 
  #	( strwt * prop.strata / prop.total + (1 - strwt) * prop.strata / prop.total^2 ) * 
  #	( sumynsq.dom - 1 / prop.strata * sumyn.dom^2 ) / ( prop.strata * (prop.strata - 1) ) ]
  # ybardat


  ## Calculate mean and variance by domain - numerator
  #########################################################

  ## Calculate sum of area proportions by plot
  ysum.plt <- ysum[, sum(.SD), by=c(unitvar, uniqueid), .SDcols=sumyn]
  setnames(ysum.plt, "V1", "prop.plt")
  setkeyv(ysum.plt, c(uniqueid))

  ## Transpose numerator and denominator values
  plt.sumyn = transpose2col(ysum, uniqueid, domain, "sumyn.pltdom")
  plt.sumyd = transpose2col(ysum, uniqueid, domain, "sumyd.pltdom")
  setnames(plt.sumyn, c(uniqueid, domnames.yn))
  setnames(plt.sumyd, c(uniqueid, domnames.yd))
  setkeyv(plt.sumyn, c(uniqueid))
  setkeyv(plt.sumyd, c(uniqueid))

  ## Subtract mean from all all values (for variance)
  plt.sumyn[, (domnames.n) := Map("-", .SD, ybardat[["nhat"]]), .SDcols=domnames.yn]
  plt.sumyd[, (domnames.d) := Map("-", .SD, ybardat[["dhat"]]), .SDcols=domnames.yd]

  
  ## Calculate variance by domain - numerator
  ###################################################################
  ysum.plt <- ysum.plt[plt.sumyn]
  ysum.plt[, (domnames.n) := .SD * prop.plt, .SDcols=domnames.n]
  ysum.plt.unit <- ysum.plt[, lapply(.SD, function(x) sum(x^2)), by=unitvar, .SDcols=domnames.n]
  var.part2 <- transpose2row(ysum.plt.unit, unitvar, domnames.n, tnewname=domain, tvalue="value")
  var.part2[[domain]] <- sub("n_", "", var.part2[[domain]])
  setkeyv(ybardat, c(unitvar, domain))
  setkeyv(var.part2, c(unitvar, domain))

  ybardat <- ybardat[var.part2][, 
	nhat.var := ( n.total / (n.total - 1) ) * value / prop.total^2][, 
	value := NULL]


  ## Calculate variance by domain - denominator
  ###################################################################
  ysum.plt <- ysum.plt[plt.sumyd]
  ysum.plt[, (domnames.d) := .SD * prop.plt, .SDcols=domnames.d]
  ysum.plt.unit <- ysum.plt[, lapply(.SD, function(x) sum(x^2)), by=unitvar, .SDcols=domnames.d]
  var.part2 <- transpose2row(ysum.plt.unit, unitvar, domnames.d, tnewname=domain, tvalue="value")
  var.part2[[domain]] <- sub("d_", "", var.part2[[domain]])  
  setkeyv(ybardat, c(unitvar, domain))
  setkeyv(var.part2, c(unitvar, domain))

  ybardat <- ybardat[var.part2][, 
	dhat.var := ( n.total / (n.total - 1) ) * value / prop.total^2][, 
	value := NULL]


  ## Calculate rhat and rhat.var
  ###################################################################
  ybardat[, rhat := nhat / dhat]
  rhat.dom = transpose2col(ybardat, unitvar, domain, "rhat")
  rnames <- paste0("rhat_", domnames)
  setnames(rhat.dom, c(unitvar, rnames))
  ysum.plt <- merge(ysum.plt, rhat.dom, by=unitvar)

  rhat.var.part2 <- colSums((ysum.plt[, domnames.yn, with=FALSE] - 
					ysum.plt[, rnames, with=FALSE] * 
					ysum.plt[, domnames.yd, with=FALSE])^2)
  ybardat$rhat.var <- with(ybardat, (n.total / (n.total-1)) * rhat.var.part2 / sumyd.dom^2)



  ## Aggregate strata-level weights to estimation unit (after adding strata-level
  ################################################################################
  unit.agvars <- c("nhat", "nhat.var", "dhat", "dhat.var", "rhat", "rhat.var")
  est.unit <- ybardat[, c(unitvar, domain, "n.total", unit.agvars), with=FALSE]



  ## Add number of non-zero plots and merge to est.unit
  ###################################################################
  NBRPLT.gt0 <- ysum[, sum(sumyn.pltdom != 0, na.rm=TRUE), by=c(unitvar, domain)] 
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
  setkeyv(NBRPLT.gt0, unitvar)
  NBRPLT.gt0[[domain]] <- as.character(NBRPLT.gt0[[domain]])
  est.unit <- merge(est.unit, NBRPLT.gt0, by=c(unitvar, domain))


  if (bytdom) est.unit$tdom <- sumyn

  return(est.unit)
}


