PBest.pbarRatio <- function(dom.prop.n, dom.prop.d, uniqueid, domain, attribute, strlut,  
	strunitvars, unitvars, strvar){

  ########################################################################################
  ## DESCRIPTION: Calculates the following variables
  ## phat.n	- estimated proportion of covered land for numerator
  ## phat.var.n	- variance of estimated proportion of covered land for numerator
  ## phat.d	- estimated proportion of covered land for denominator
  ## phat.var.d	- variance of estimated proportion of covered land for denominator
  ## covar		- covariance of numerator and denominator
  ## rhat		- ratio of estimated proportions (numerator/denominator)
  ## rhat.var	- variance of ratio of estimated proportions
  ## rhat.se	- standard error of ratio of estimated proportions { sqrt(rhat.var) }
  ## rhat.cv	- coefficient of variation of ratio of estimated proportions { rhat.se/rhat }
  ########################################################################################


  ## Set global variables
  psq.pltdom.n=p.pltdom.n=pnpd=p.pltdom.d=psq.pltdom.d=sumpn=n.strata=strwt=sumpnsq=
	sumpd=sumpdsq=covar.strwt=sumpnpd=phat.n=phat.d=phat.var.n=phat.var.d=covar=
	rhat.se=rhat.var=rhat.cv=rhat <- NULL

  ## Merge pd to ysum.n
  dom.prop.n <- merge(dom.prop.n, dom.prop.d, by=c(strunitvars, uniqueid, domain, "PtsPerPlot"))

  ## Proportion of points per plot, squared (intermediate variable) for num and den
  dom.prop.n[, psq.pltdom.n := p.pltdom.n^2][, pnpd := p.pltdom.n * p.pltdom.d]
  dom.prop.d[, psq.pltdom.d := p.pltdom.d^2]


  SDcols <- c("p.pltdom.n", "p.pltdom.d", "psq.pltdom.n", "pnpd",
		"nbrpts.pltdom.n", "nbrpts.pltdom.d")
  ## STRATA/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/domain
  ysum.n.strata <- dom.prop.n[, lapply(.SD, sum), by=c(strunitvars, domain, attribute),
		.SDcols=SDcols]
  setnames(ysum.n.strata, SDcols, c("sumpn", "sumpd", "sumpnsq", "sumpnpd", "nbrpts.n", "nbrpts.d"))
  setkeyv(ysum.n.strata, strunitvars)

  ysum.d.strata <- dom.prop.d[, list(sumpd=sum(p.pltdom.d), sumpdsq=sum(psq.pltdom.d)), 
		by=c(strunitvars, domain)]
  setkeyv(ysum.d.strata, strunitvars)
 


  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  ybardat.n <- strlut[ysum.n.strata]
  ybardat.d <- strlut[ysum.d.strata]


  ## Calculate estimate and estimated variance by estimation unit and strata for numerator
  ybardat.n[, ':=' (
	phat.n.strwt = sumpn / n.strata * strwt,
	phat.var.n.strwt = strwt^2 * 	
	( sumpnsq - 1 / n.strata * sumpn^2 ) / ( n.strata * (n.strata - 1) )) ]

  ## Calculate estimate and estimated variance by estimation unit and strata for denominator
  ybardat.d[, ':=' (
	phat.d.strwt = sumpd / n.strata * strwt,
	phat.var.d.strwt = strwt^2 * 	
	( sumpdsq - 1 / n.strata * sumpd^2 ) / ( n.strata * (n.strata - 1) )) ]

  ## Calculate covariance weights by estimation unit and strata for ratio
  ybardat.n[, covar.strwt := strwt^2 *
		( sumpnpd - 1 / n.strata * sumpn * sumpd ) / ( n.strata * (n.strata - 1) ) ]

  unit.agvars.n <- c("phat.n.strwt", "phat.var.n.strwt", "covar.strwt", "nbrpts.n", "nbrpts.d")
  unit.agvars.d <- c("phat.d.strwt", "phat.var.d.strwt")


  ## Aggregate strata-level weights to estimation unit - Numerator
  est.unit.n <- ybardat.n[, lapply(.SD, sum), by=c(unitvars, domain, attribute), 
		.SDcols=unit.agvars.n]
  setkeyv(est.unit.n, c(unitvars, domain))
  setnames(est.unit.n, unit.agvars.n, sapply(unit.agvars.n, function(x) sub(".strwt", "", x) ))

  ## Aggregate strata-level weights to estimation unit - Denominator
  est.unit.d <- ybardat.d[, lapply(.SD, sum), by=c(unitvars, domain), 
		.SDcols=unit.agvars.d]
  setkeyv(est.unit.d, c(unitvars, domain))
  setnames(est.unit.d, unit.agvars.d, sapply(unit.agvars.d, function(x) sub(".strwt", "", x) ))


  ## Calculate rhat, variance (var), standard error (se), coefficient of variation (cv)
  est.unit <- est.unit.n[est.unit.d]
  suppressWarnings(
  est.unit[, ':=' (
	rhat = phat.n / phat.d, 
	rhat.var = ( phat.var.n + phat.n^2 * phat.var.d - 2 * phat.n * covar ) / phat.d^2 )][, 
	rhat.se := sqrt(rhat.var)][,
	rhat.cv := rhat.se / rhat] )

  est.unit <- est.unit[get(domain) != "NOTinDOMAIN" & get(attribute) != "NOTinDOMAIN", ]
  setkeyv(est.unit, unitvars)

  ## Convert unitvars to character
  est.unit[, (unitvars) := as.character(get(unitvars))]


  # return dataframe
  return(data.table(est.unit))
  if (length(unique(est.unit[[strvar]])) > 1) {
    returnlst$ybardat.n <- ybardat.n
    returnlst$ybardat.d <- ybardat.d
  }
}
 


