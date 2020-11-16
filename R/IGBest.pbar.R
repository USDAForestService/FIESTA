GBest.pbar <- function(sumyn="CONDPROP_ADJ", ysum, sumyd=NULL, esttype="AREA",
 	ratiotype="PERACRE", bytdom=FALSE, strlut, uniqueid, unitvar, strvar=NULL, domain){
  ########################################################################################
  ## DESCRIPTION: Calculates the following variables using Green-book estimators
  ## ARGUMENTS:
  ## sumyn		- string. estimation response
  ## ysum		- data frame. domain-level aggregated dataset
  ## sumyd 		- string. estimation response (denominator) - for ratio of means estimates
  ## esttype 	- string. type of estimate ('AREA', 'TREE', 'RATIO')
  ## ratiotype	- string. type of ratio ('PERACRE', 'PERTREE')
  ## bytdom		- logical. if TRUE, estimates are by tree domains (e.g., species)
  ## strlut		- data.frame. strata-level information
  ## uniqueid 	- unique plot identifier in ysum
  ## unitvar 	- name of variable defining estimation unit
  ## strvar		- name of variable defining strata 
  ## domain		- name of variable defining domain (e.g., forest type)
  ##
  ## VALUE:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## covar		- covariance of numerator and denominator
  ########################################################################################
  strunitvars <- c(unitvar, strvar)

  ## Set global variables
  sumyn.pltdom=sumyd.pltdom=sumynsq.pltdom=sumydsq.pltdom=sumnd.pltdom=
	nhat.strwt=sumyn.dom=n.strata=strwt=nhat.var.strwt=n.total=
	sumynsq.dom=dhat.strwt=sumyd.dom=dhat.var.strwt=sumydsq.dom=
	covar.strwt=sumnd.dom <- NULL

  if (!"n.strata" %in% names(strlut)) stop("need n.strata in strlut")
  if (!"n.total" %in% names(strlut)) stop("need n.total in strlut")


  if (esttype == "RATIO") {
    if (ratiotype == "PERTREE") {
      sumyd <- paste(sumyn, "d", sep=".")
      if (!sumyd %in% names(ysum)) {
        message(sumyd, " not in denominator")
        return(NULL)
      }
    }

    ## STRATA/PLOT/DOMAIN LEVEL: Change name and square sum and multiply sum for denominator 
    ysum[, ':=' (sumyn.pltdom = get(sumyn), sumyd.pltdom = get(sumyd))][, 
		':=' (sumynsq.pltdom = sumyn.pltdom^2, sumydsq.pltdom = sumyd.pltdom^2,
			sumnd.pltdom = sumyn.pltdom * sumyd.pltdom)]

    ## STRATA/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/domain
    ysum.strata <- ysum[, list(sumyn.dom=sum(sumyn.pltdom, na.rm=TRUE),
 		sumynsq.dom=sum(sumynsq.pltdom, na.rm=TRUE),
		sumyd.dom=sum(sumyd.pltdom, na.rm=TRUE), sumydsq.dom=sum(sumydsq.pltdom, na.rm=TRUE),
		sumnd.dom=sum(sumnd.pltdom, na.rm=TRUE)), by=c(strunitvars, domain)]
    setkeyv(ysum.strata, strunitvars)
 
  } else {
    ## STRATA/PLOT/DOMAIN LEVEL: Change name and square sum 
    ysum[, sumyn.pltdom := get(sumyn)][, sumynsq.pltdom := sumyn.pltdom^2]

    ## STRATA/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/domain
    ysum.strata <- ysum[, list(sumyn.dom=sum(sumyn.pltdom, na.rm=TRUE),
 		sumynsq.dom=sum(sumynsq.pltdom, na.rm=TRUE)),
		by=c(strunitvars, domain)]
    setkeyv(ysum.strata, strunitvars)
  }

  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  setkeyv(strlut, strunitvars)
  ybardat <- strlut[ysum.strata]


  ## Calculate estimate weights by estimation unit and strata for numerator
  ybardat[, nhat.strwt := sumyn.dom/n.strata * strwt]

  ## Calculate estimated variance weights by estimation unit and strata for numerator 
  ## (based on Equation 4.6, with the variance per strata equation 4.4 factored out)
  ybardat[, nhat.var.strwt := 
	( strwt * n.strata / n.total + (1 - strwt) * n.strata / n.total^2 ) * 
	( sumynsq.dom - 1 / n.strata * sumyn.dom^2 ) / ( n.strata * (n.strata - 1) ) ]

  unit.agvars <- c("nhat.strwt", "nhat.var.strwt")
 
  if (esttype == "RATIO") {
    ## Calculate estimate weights by estimation unit and strata for denominator
    ybardat[, dhat.strwt := sumyd.dom/n.strata * strwt]

    ## Calculate estimated variance weights by estimation unit and strata for denominator
    ybardat[, dhat.var.strwt := 
		( strwt * n.strata / n.total + (1 - strwt) * n.strata / n.total^2 ) * 
		( sumydsq.dom - 1 / n.strata * sumyd.dom^2 ) / ( n.strata * (n.strata - 1) ) ]

    ## Calculate covariance weights by estimation unit and strata for ratio
    ybardat[, covar.strwt :=  
		( strwt * n.strata / n.total + (1 - strwt) * n.strata / n.total^2 ) * 
		( sumnd.dom - 1 / n.strata * sumyn.dom * sumyd.dom ) / ( n.strata * (n.strata - 1) ) ]

    unit.agvars <- unique(c(unit.agvars, "dhat.strwt", "dhat.var.strwt", "covar.strwt"))
  }

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
