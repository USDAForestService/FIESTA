MAest.ht <- function(y, N, FIA=TRUE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "lin_HTSRS"
  estht <- mase::horvitzThompson(y, pi = NULL, N = N, pi2 = NULL, 
						var_est = TRUE, var_method = var_method, 
						B = 1000, strata = NULL)

  estht <- data.table(estht$pop_mean, estht$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estht, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estht[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estht)
}


MAest.ps <- function(y, N, x_sample, x_pop, FIA=TRUE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "lin_HTSRS"
  estps <- mase::postStrat(	  y = y, 
					   x_sample = x_sample, 
					   x_pop = x_pop, 
					   pi = NULL, N = N, pi2 = NULL, 
					   var_est = TRUE, var_method = var_method,
					   data_type = "means", 
					   B = 1000, strata = NULL)

  estps <- data.table(estps$pop_mean, estps$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estps, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estps[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estps)
}


MAest.greg <- function(y, N, x_sample, x_pop, FIA=TRUE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "lin_HTSRS"
  estgreg <- mase::greg(	y = y, 
					x_sample = x_sample, 
					x_pop = x_pop, 
					pi = NULL, N = N, pi2 = NULL,
					model = "linear", 
  					var_est = TRUE, var_method = var_method, 
					data_type = "means", 
  					model_select = FALSE, 
					lambda = "lambda.min", 
					B = 1000, strata = NULL)

  estgreg <- data.table(estgreg$pop_mean, estgreg$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgreg, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
 
  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estgreg[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estgreg)
}



########################################################################
## Get estimates
########################################################################
MAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, unitlut=NULL, 
	pltassgn, esttype="ACRES", MAmethod, PSstrvar=NULL, prednames=NULL, 
	yd=NULL, ratiotype="PERACRE", N, FIA=TRUE) {

  ########################################################################################
  ## DESCRIPTION: Gets estimates from mase::horvitzThompson
  ## PARAMETERS:
  ## yn 		- response (numerator)
  ## dat 		- domain-level data set
  ## yd		- response (denominator)
  ## N		- Number of pixels in estimation unit
  ## n		- Number of total plots in estimation unit
  ## 
  ## VALUES:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## covar		- covariance of numerator and denominator
  ########################################################################################

  ## Merge dat.dom to pltassgn
  pltdat.dom <- dat.dom[pltassgn]

  ## Subset response vector and change NA values of response to 0
  yn.vect <- pltdat.dom[[yn]]
  yn.vect[is.na(yn.vect)] <- 0

  if (MAmethod == "HT") {
    est <- MAest.ht(yn.vect, N)

  } else if (MAmethod == "PS") {

    x_sample <- pltdat.dom[, PSstrvar, with=FALSE][[1]]
    x_pop <- unitlut[, c(PSstrvar, "Prop"), with=FALSE]
    est <- MAest.ps(yn.vect, N, x_sample, x_pop, FIA=FIA)

  } else if (MAmethod == "GREG") {

    x_sample <- setDF(pltdat.dom[, prednames, with=FALSE])
    x_pop <- setDF(unitlut[, prednames, with=FALSE])
    est <- MAest.greg(yn.vect, N, x_sample, x_pop, FIA=FIA)
  }
  return(est)
}


########################################################################
## By domain
########################################################################
MAest.dom <- function(dom, dat, cuniqueid, unitlut, pltassgn, esttype, MAmethod, 
		PSstrvar=NULL, prednames=NULL, domain, N, response=NULL, FIA=TRUE) {
#dom <- doms[1]
  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

#yn=response
#dat=dat.dom

  ## Apply function to each dom
  domest <- data.table(dom, MAest(yn=response, dat.dom=dat.dom, 
		cuniqueid=cuniqueid, esttype=esttype, unitlut=unitlut, 
		pltassgn=pltassgn, PSstrvar=PSstrvar, prednames=prednames, 
		MAmethod=MAmethod, N=N, FIA=FIA))
  setnames(domest, "dom", domain)
  return(domest)
}



########################################################################
## By estimation unit
########################################################################
MAest.unit <- function(unit, dat, cuniqueid, unitlut, unitvar, 
		esttype, MAmethod="HT", PSstrvar=NULL, prednames=NULL, 
		domain, response, npixels, FIA=TRUE) {
## testing
#unit=1
#domain="TOTAL"
#print(unit)

  dat.unit <- dat[dat[[unitvar]] == unit, c(cuniqueid, domain, response),
			with=FALSE]
  setkeyv(dat.unit, cuniqueid)
  pltassgn.unit <- unique(dat[dat[[unitvar]] == unit, c(cuniqueid, PSstrvar, prednames),
			with=FALSE])
  setkeyv(pltassgn.unit, cuniqueid)

  if (nrow(dat.unit) == 0) {
    if (domain == "TOTAL") {
      unitest <- data.table(unit=unit, domain=1, nhat=0, nhat.var=0, 
		NBRPLT=0, NBRPLT.gt0=0)
    } else {
      unitest <- data.table(unit=unit, domain="TOTAL", nhat=0, nhat.var=0, 
		NBRPLT=0, NBRPLT.gt0=0)
    }  
    setnames(unitest, c("unit", "domain"), c(unitvar, domain)) 
    return(unitest)
  }

  unitlut.unit <- unitlut[unitlut[[unitvar]] == unit, ]
  N.unit <-  npixels[["npixels"]][npixels[[unitvar]] == unit]

  if (!MAmethod %in% c("HT", "PS")) {
    if (any(pltassgn.unit[, colSums(.SD), .SDcols=prednames] < 3)) {
      prednames <- prednames[pltassgn.unit[, colSums(.SD), .SDcols=prednames] >= 3]
      if (length(prednames) == 0) MAmethod <- "HT"
    }
  }

  doms <- as.character(unique(dat.unit[[domain]]))

#dat=dat.unit
#unitlut=unitlut.unit
#pltassgn=pltassgn.unit
#N=N.unit
#prednames="fnf"

  unitest <- do.call(rbind, lapply(doms, MAest.dom, 
			dat=dat.unit, cuniqueid=cuniqueid, 
			unitlut=unitlut.unit, pltassgn=pltassgn.unit,
			esttype=esttype, MAmethod=MAmethod, 
			PSstrvar=PSstrvar, prednames=prednames, 
			domain=domain, N=N.unit, response=response,
			FIA=FIA))        
  unitest <- data.table(unit=unit, unitest)
  setnames(unitest, "unit", unitvar)
  return(unitest)
}


