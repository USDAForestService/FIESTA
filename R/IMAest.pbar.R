MAest.ht <- function(y, N) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "lin_HTSRS"
  estht <- mase::horvitzThompson(y, pi = NULL, N = N, pi2 = NULL, 
						var_est = TRUE, var_method = var_method, 
						B = 1000, strata = NULL)

  estht <- data.table(estht$pop_mean, estht$pop_mean_var, NBRPLT=NBRPLT, NBRPLT.gt0=NBRPLT.gt0)
  setnames(estht, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  ## This takes out the finite population correction term (to estimated variance from FIA)
  estht[, nhat.var := nhat.var / (1 - length(y) / N)]
}


MAest.ps <- function(y, N, x_sample, x_pop) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- sum(y > 0)
  var_method <- "lin_HTSRS"
  estps <- mase::postStrat(	  y = y, 
					   x_sample = x_sample, 
					   x_pop = x_pop, 
					   pi = NULL, N = N, pi2 = NULL, 
					   var_est = TRUE, var_method = var_method,
					   data_type = "means", 
					   B = 1000, strata = NULL)

  estps <- data.table(estps$pop_mean, estps$pop_mean_var, NBRPLT=NBRPLT)
  setnames(estps, c("nhat", "nhat.var", "NBRPLT"))


  ## This takes out the finite population correction term (to estimated variance from FIA)
  #estps[, nhat.var := nhat.var / (1 - length(y) / N)]
}


MAest.greg <- function(y, N, x_sample, x_pop) {

  ## Set global variables
  #nhat.var <- NULL

  NBRPLT <- sum(y > 0)
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

  estgreg <- data.table(estgreg$pop_mean, estgreg$pop_mean_var, NBRPLT=NBRPLT)
  setnames(estgreg, c("nhat", "nhat.var", "NBRPLT"))


  ## This takes out the finite population correction term (to estimated variance from FIA)
  #estgreg[, nhat.var := nhat.var / (1 - length(y) / N)]
}


MAest <- function(yn="CONDPROP_ADJ", dat, cuniqueid=cuniqueid, pltmodelx, 
	puniqueid=puniqueid, unitlut=NULL, esttype="ACRES", bytdom=FALSE, MAmethod, 
	PSstrvar=NULL, prednames=NULL, yd=NULL, ratiotype="PERACRE", N) {

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

  ## Sum response to domain and merge to pltmodelx
  dat.dom <- dat[, sum(get(yn)), by=cuniqueid]
  setnames(dat.dom, "V1", yn)
  dat.dom <- merge(pltmodelx[, c(puniqueid, PSstrvar, prednames), with=FALSE], 
				dat.dom, by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
  dat.dom <- DT_NAto0(dat.dom, yn, 0)
  yn.vect <- dat.dom[[yn]]

  if (MAmethod == "HT") {
    estht <- MAest.ht(yn.vect, N)
    if (bytdom)
      estht <- data.table(tdom=yn, estht)
    est <- estht

  } else if (MAmethod == "PS") {
    x_sample <- dat.dom[, PSstrvar, with=FALSE][[1]]
    x_pop <- unitlut[, c(PSstrvar, "Prop"), with=FALSE]

    estps <- MAest.ps(yn.vect, N, x_sample, x_pop)
    if (bytdom)
      estps <- data.table(tdom=yn, estps)
    est <- estps

  } else if (MAmethod == "GREG") {

    x_sample <- setDF(dat.dom[, prednames, with=FALSE])
    x_pop <- setDF(unitlut[, prednames, with=FALSE])

    estgreg <- MAest.greg(yn.vect, N, x_sample, x_pop)
    if (bytdom)
      estgreg <- data.table(tdom=yn, estgreg)
    est <- estgreg
  }
  return(est)
}


## Define functions
MAest.dom <- function(dom, dat, cuniqueid, pltmodelx, puniqueid, 
		unitlut, esttype, bytdom=FALSE, MAmethod, PSstrvar=NULL, prednames=NULL, 
		domain, N, response=NULL, tdomvarlst=NULL) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

#yn=response
#dat=dat.dom

  ## If tdom (e.g., SPCD), apply function to each tdom in tdomvarlst
  if (bytdom) {
    domest <- data.table(dom, 
		do.call(rbind, lapply(tdomvarlst, MAest, dat=dat.dom, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx, puniqueid=puniqueid, esttype=esttype, bytdom=bytdom, 
			unitlut=unitlut, PSstrvar=PSstrvar, prednames=prednames, 
 			MAmethod=MAmethod, N=N)))
  } else {
    domest <- data.table(dom, MAest(yn=response, dat=dat.dom, cuniqueid=cuniqueid, 
 		pltmodelx=pltmodelx, puniqueid=puniqueid, esttype=esttype, bytdom=bytdom, 
		unitlut=unitlut, PSstrvar=PSstrvar, prednames=prednames, 
		MAmethod=MAmethod, N=N))
  }
  setnames(domest, "dom", domain)
  return(domest)
}

MAest.unit <- function(unit, dat, cuniqueid, pltmodelx, puniqueid, 
		unitlut, unitvar, esttype, bytdom=FALSE, MAmethod="HT",  
		PSstrvar=NULL, prednames=NULL, domain, response, npixels,
		tdomvarlst=NULL) {
  dat.unit <- dat[dat[[unitvar]] == unit, ]
  unitlut.unit <- unitlut[unitlut[[unitvar]] == unit, ]
  pltmodelx.unit <- pltmodelx[pltmodelx[[unitvar]] == unit, ]
  N.unit <-  npixels[["npixels"]][npixels[[unitvar]] == unit]

  doms <- as.character(unique(dat.unit[[domain]]))


#dat=dat.unit
#pltmodelx=pltmodelx.unit
#unitlut=unitlut.unit
#N=N.unit

  unitest <- do.call(rbind, lapply(doms, MAest.dom, 
			dat=dat.unit, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx.unit, puniqueid=puniqueid,
     			unitlut=unitlut.unit,
			esttype=esttype, bytdom=bytdom,
			MAmethod=MAmethod, PSstrvar=PSstrvar, 
			prednames=prednames, 
			domain=domain, N=N.unit, 
			response=response, tdomvarlst=tdomvarlst))        
  unitest <- data.table(unit=unit, unitest)
  setnames(unitest, "unit", unitvar)
  return(unitest)
}


