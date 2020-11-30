RMest.pbar <- function(sumyn, ysum, sumyd, bytdom=FALSE, uniqueid, strlut,
 	unitvar, strvar, domain){
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
  sumyn.pltdom=sumyd.pltdom=nhat.strwt=dhat.strwt=sumyn.dom=sumyd.dom=
	sumynsq.pltdom=sumydsq.pltdom=sumnd.pltdom=sumnp.pltdom=sumdp.pltdom=
	strwt=n.strata=sumynsq.dom=sumydsq.dom=sumnp.dom=sumdp.dom=sumnd.dom=
	propsq.total=nhat.var.strwt=dhat.var.strwt=rhat.strwt=rhat.var.strwt=
	n.total=prop.total=prop.plt=value=rhat <- NULL

  ## Set key for est.unit (note: to run separate)
  #require(data.table)

  tdomdatsum <- fread("tdomdatsum.csv")
  ysum <- tdomdatsum
  ysum <- setDT(ysum)
  setkeyv(ysum, c(strunitvars, uniqueid))


  if (!"n.strata" %in% names(strlut)) stop("need n.strata in strlut")
  if (!"n.total" %in% names(strlut)) stop("need n.total in strlut")


  ## STRATA/PLOT LEVEL: Sum area by plot
  sumyd.plt <- ysum[,  list(sumyd.plt = sum(get(sumyd), na.rm=TRUE)), 
						by=c(strunitvars, uniqueid)]
  setkeyv(sumyd.plt, c(strunitvars, uniqueid))
  ysum <- ysum[sumyd.plt]

  ## STRATA/PLOT/DOMAIN LEVEL: Change name and square sum and multiply sum for denominator
  ysum[, ':=' (sumyn.pltdom = get(sumyn), sumyd.pltdom = get(sumyd))][, 
    ':=' (sumynsq.pltdom = sumyn.pltdom^2, sumydsq.pltdom = sumyd.pltdom^2,
    sumnd.pltdom = sumyn.pltdom * sumyd.pltdom,
    sumnp.pltdom = sumyn.pltdom * sumyd.plt, sumdp.pltdom = sumyd.pltdom * sumyd.plt)]
  
  
  ## STRATA/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/domain
  ysum.strata <- ysum[, list(sumyn.dom=sum(sumyn.pltdom, na.rm=TRUE), 
    sumynsq.dom=sum(sumynsq.pltdom, na.rm=TRUE),
    sumyd.dom=sum(sumyd.pltdom, na.rm=TRUE), sumydsq.dom=sum(sumydsq.pltdom, na.rm=TRUE),
    sumnd.dom=sum(sumnd.pltdom, na.rm=TRUE), 
    sumnp.dom=sum(sumnp.pltdom, na.rm=TRUE), sumdp.dom=sum(sumdp.pltdom, na.rm=TRUE)), 
    by=c(strunitvars, domain)]
  setkeyv(ysum.strata, strunitvars)
 

  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  setkeyv(strlut, strunitvars)
  ybardat <- strlut[ysum.strata]
 

  ## Calculate mean for numerator and denominator
  #########################################################
  ## using ratio to size estimators with assumption that Andy's plot are 1-unit in size 
  ybardat[, nhat.strwt := sumyn.dom/prop.total * strwt]
  ybardat[, dhat.strwt := sumyd.dom/prop.total * strwt]

  ## Calculate variance of the mean for numerator and denominator
  ######################################################### 
  ybardat[, nhat.var.strwt :=
    		n.strata / (n.strata-1) * (sumynsq.dom-2 * nhat.strwt * 
			sumnp.dom + nhat.strwt^2 * propsq.total) / (prop.total^2)]       
  ybardat[, dhat.var.strwt :=
            n.strata / (n.strata-1) * (sumydsq.dom-2 * dhat.strwt * 
			sumdp.dom + dhat.strwt^2 * propsq.total) / (prop.total^2)]
  
  ## Calculate rhat and rhat.var
  ###################################################################
  ybardat[, rhat.strwt := nhat.strwt / dhat.strwt]
  
  ybardat[, rhat.var.strwt :=
            n.strata / (n.strata-1) * (sumynsq.dom-2 * rhat.strwt * 
			sumnd.dom + rhat.strwt^2 * sumydsq.dom) / (sumyd.dom^2)]

  unit.agvars <- unique(c("nhat.strwt", "nhat.var.strwt", 
				"dhat.strwt", "dhat.var.strwt", "rhat.strwt", "rhat.var.strwt"))


  ## Aggregate strata-level weights to estimation unit
  est.unit <- ybardat[, lapply(.SD, sum, na.rm=TRUE), by=c(unitvar, domain), 
		.SDcols=unit.agvars]
  setkeyv(est.unit, unitvar)


  names(est.unit) <- sub(".strwt", "", names(est.unit))
  #setnames(est.unit, "n.strata", "NBRPLT")

  ## Add number of non-zero plots and merge to est.unit
  NBRPLT.gt0 <- ysum[, sum(sumyn.pltdom != 0, na.rm=TRUE), by=c(unitvar, domain)] 
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
  setkeyv(NBRPLT.gt0, unitvar)

  est.unit <- merge(est.unit, NBRPLT.gt0, by=c(unitvar, domain))
  
  #if (bytdom) est.unit$tdom <- sumyn

  return(est.unit)
}


