PBest.pbar <- function(dom.prop, uniqueid, domain, strtype="post", stratalut, strunitvars, 
	unitvars, strvar){

  ########################################################################################
  ## DESCRIPTION: Calculates the following variables
  ## phat		- estimated proportion of cover
  ## phat.var	- variance of estimated proportion of cover
  ## phat.se	- standard error of estimated proportion of cover { sqrt(phat.var) }
  ## phat.cv	- coefficient of variance of estimated proportion of cover { phat.se/phat }
  ########################################################################################


  ## Set global variables
  psq.pltdom=sump.dom=n.strata=strwt=sumpsq.dom=phat.se=phat.var=phat.cv=
	ese.pct=pse=phat=p.pltdom=nbrpts.pltdom=phat.strwt=phat.var.strwt=n.total <- NULL

  ## Check that stratalut is a data.table
  if (!"data.table" %in% class(stratalut))
    stratalut <- setDT(stratalut)
  setkeyv(stratalut, strunitvars)

  ## Proportion of points per plot, squared (intermediate variable)
  dom.prop[, psq.pltdom := p.pltdom^2]


  ## STRATA/DOMAIN LEVEL: 
  ## Aggregate plot-level proportions and squared proportions by unit/strata/domain
  #ysum.strata <- dom.prop[, list(sump.dom=sum(p.pltdom), sumpsq.dom=sum(psq.pltdom),
#		nbrpts=sum(nbrpts.pltdom)), by=c(strunitvars, domain)]
  ysum.strata <- dom.prop[, list(sump.dom=sum(p.pltdom), sumpsq.dom=sum(psq.pltdom)),
		by=c(strunitvars, domain)]
  setkeyv(ysum.strata, strunitvars)

  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  #setkeyv(stratalut, strunitvars)
  ybardat <- stratalut[ysum.strata]

  if (strtype == "pre") {

    ## Calculate estimate weights by estimation unit and strata
    ybardat[, phat.strwt := sump.dom/n.strata * strwt]

    ## Calculate estimated variance weights by estimation unit 
    ybardat[, phat.var.strwt := strwt^2 * 	
	  ( sumpsq.dom - 1 / n.strata * sump.dom^2 ) / ( n.strata * (n.strata - 1) ) ]
  }

  if (strtype == "post") {  

    ## Calculate estimate weights by estimation unit and strata
    ybardat[, phat.strwt := sump.dom/n.strata * strwt]

    ## Calculate estimated variance weights by estimation unit and strata 
    ## (based on Equation 4.6, with the variance per strata equation 4.4 factored out)
    ybardat[, phat.var.strwt := 
	( strwt * n.strata / n.total + (1 - strwt) * n.strata / n.total^2 ) * 
	( sumpsq.dom - 1 / n.strata * sump.dom^2 ) / ( n.strata * (n.strata - 1) ) ]
  }
		
  #unit.agvars <- c("phat.strwt", "phat.var.strwt", "nbrpts")
  unit.agvars <- c("phat.strwt", "phat.var.strwt")


  ## Aggregate strata-level weights to estimation unit
  est.unit <- ybardat[, lapply(.SD, sum), by=c(unitvars, domain), .SDcols=unit.agvars]

  unit.vars <- sapply(unit.agvars, function(x) sub(".strwt", "", x) )
  setnames(est.unit, unit.agvars, unit.vars)

  ## Calculate standard error (se), coefficient of variation (cv) 
  #suppressWarnings(
  #est.unit[, phat.se := sqrt(phat.var)][, phat.cv := phat.se / phat] )

  ## Convert unitvars to character
  est.unit[, (unitvars) := as.character(get(unitvars))]

  ## Change NA values to 0
  est.unit[is.na(est.unit)] <- 0
  setkeyv(est.unit, unitvars)
     
  # return data.table (est.unit) and proportion of points by domain (dom.prop)
  returnlst <- list(est.unit=data.table(est.unit))
  if (length(unique(est.unit[[strvar]])) > 1)
    returnlst$ybardat <- ybardat
  
  return(returnlst) 
}


