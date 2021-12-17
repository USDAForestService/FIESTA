
MAest.ht <- function(y, N, FIA=TRUE, getweights=FALSE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "LinHTSRS"
  estht <- mase::horvitzThompson(y, pi = NULL, N = N, pi2 = NULL, 
						var_est = TRUE, var_method = var_method, 
						B = 1000)

  esthtdt <- data.table(estht$pop_mean, estht$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(esthtdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=esthtdt)

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    esthtdt[, nhat.var := nhat.var / (1 - length(y) / N)]
  }

  ## Return survey weights
  if (getweights) {
    if (any(estht$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estht$weights / N
  }
  return(returnlst)
}


MAest.ps <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE, getweights=FALSE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "SRSunconditional"

  estps <- tryCatch(mase::postStrat(	  y = y, 
					   xsample = x_sample, 
					   xpop = x_pop, 
					   pi = NULL, N = N, pi2 = NULL, 
					   var_est = TRUE, var_method = var_method,
					   datatype = "means", 
					   B = 1000),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  if (is.null(estps)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("error in mase::postStrat function... returning NA")
    estps <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estps, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est=estps)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  estpsdt <- data.table(estps$pop_mean, estps$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estpsdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estpsdt)

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estpsdt[, nhat.var := nhat.var / (1 - length(y) / N)]
  }

  ## Return survey weights
  if (getweights) {
    if (any(estps$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estps$weights / N
  }
  return(returnlst)
}


MAest.greg <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE,
			modelselect=FALSE, getweights=FALSE) {

  #y <- yn.vect

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)

  ## define empty data frame for storing selected predictors
  predselect <- x_pop[FALSE, ]


  if (modelselect) {
    ## Get model-selected variables using mase::gregElasticNet()
    preds.selected <- gregEN.select(y, x_sample, x_pop, N, alpha=0.5)
  } else {
    preds.selected <- names(predselect)
  }

  ## Note: GREG cannot have more predictors than plots
  if (length(y) <= length(names(predselect))) {
    preds.selected <- na.omit(preds.selected[1:(length(y)-1)])
  }

  if (!is.null(preds.selected) && length(preds.selected) > 0) {
    xsample <- x_sample[, preds.selected, drop=FALSE]
    xpop <- x_pop[, preds.selected, drop=FALSE]

    var_method <- "LinHTSRS"
    estgreg <- tryCatch(mase::greg(	y = y, 
					xsample = xsample, 
					xpop = x_pop, 
					pi = NULL, N = N, pi2 = NULL,
					model = "linear", 
  					var_est = TRUE, var_method = var_method, 
					datatype = "means", 
  					modelselect = FALSE, 
					lambda = "lambda.min", 
					B = 1000),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  } else {
    estgreg <- NULL
  }
  if (is.null(estgreg)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("multicolinearity exists in predictor data set...  try MAmethod = gregEN")
    estgreg <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estgreg, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    predselect[1,] <- NA
    returnlst <- list(est=estgreg, predselect=predselect)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  selected <- data.frame(t(estgreg$coefficients))[,-1]
  predselect <- rbindlist(list(predselect, selected), fill=TRUE)

  estgregdt <- data.table(estgreg$pop_mean, estgreg$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgregdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estgregdt, predselect=predselect)

  ## This takes out the finite population correction term (to estimated variance from FIA)
  if (FIA) {
    estgregdt[, nhat.var := nhat.var / (1 - length(y) / N)]
  }

  ## Return survey weights
  if (getweights) {
    if (any(estgreg$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estgreg$weights / N
  }
  return(returnlst)
}


MAest.ratio <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE, getweights=FALSE) {

#y <- yn.vect

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "LinHTSRS"

  estratio <- tryCatch(mase::ratioEstimator(	y = y, 
					xsample = x_sample, 
					xpop = x_pop, 
					pi = NULL, N = N, pi2 = NULL,
					var_est = TRUE, var_method = var_method, 
					datatype = "means", 
					B = 1000),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
  if (is.null(estratio)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("error in mase::ratioEstimator function... returning NA")
    estratio <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estratio, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    returnlst <- list(est=estratio)

#    if (getweights) {
#      weights <- rep(NA, length(y))
#      returnlst$weights <- weights
#    }
    return(returnlst)
  }

  estratiodt <- data.table(estratio$pop_mean, estratio$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estratiodt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
  returnlst <- list(est=estratiodt)
 
  ## This takes out the finite population correction term (to estimated variance from FIA)
  if (FIA) {
    estratiodt[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  ## Return survey weights
  if (getweights) {
    if (any(estratio$weights < 0)) {
      message("model resulted in negatives... indicating model instability")
    }
    returnlst$weights <- estratio$weights / N
  }
  return(returnlst)
}



MAest.gregEN <- function(y, N, x_sample, x_pop, FIA=TRUE, model="linear", 
		save4testing=TRUE) {

#y <- yn.vect

  ## define empty data frame for storing selected predictors
  predselect <- x_pop[FALSE, ]


  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "LinHTSRS"
  estgregEN <- tryCatch(mase::gregElasticNet(	y = y, 
					xsample = x_sample, 
					xpop = x_pop, 
					pi = NULL, N = N, pi2 = NULL,
					model = model, 
  					var_est = TRUE, var_method = var_method, 
					datatype = "means", 
					lambda = "lambda.min", 
					B = 1000),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )

  if (is.null(estgregEN)) {
    if (save4testing) {
      message("saving objects to working directory for testing: y, x_sample, x_pop, N")

      save(y, file=file.path(getwd(), "y.rda"))
      save(x_sample, file=file.path(getwd(), "x_sample.rda"))
      save(x_pop, file=file.path(getwd(), "x_pop.rda"))
      save(N, file=file.path(getwd(), "N.rda"))
    }
    message("error in mase::gregElasticNet function... returning NA")

    estgregEN <- data.table(matrix(c(NA, NA, NBRPLT, NBRPLT.gt0), 1,4))
    setnames(estgregEN, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    predselect[1,] <- NA
    returnlst <- list(est=estgregEN, predselect=predselect)

    if (getweights) {
      weights <- rep(NA, length(y))
      returnlst$weights <- weights
    }
    return(returnlst)
  }

  selected <- data.frame(t(estgregEN$coefficients))[,-1]
  predselect <- rbindlist(list(predselect, selected), fill=TRUE)

  estgregENdt <- data.table(estgregEN$pop_mean, estgregEN$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgregENdt, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
 
  returnlst <- list(est=estgregENdt, predselect=predselect)

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estgregENdt[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  ## Return survey weights
#  if (getweights) {
#    if (any(estgregEN$weights < 0)) {
#      message("model resulted in negatives... indicating model instability")
#    }
#    returnlst$weights <- estgregEN$weights / N
#  }
  return(returnlst)

}


########################################################################
## Get estimates
########################################################################
MAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, unitlut=NULL, 
	pltassgn, esttype="ACRES", MAmethod, strvar=NULL, prednames=NULL, 
	yd=NULL, ratiotype="PERACRE", N, FIA=TRUE, modelselect=FALSE,
	getweights=FALSE) {

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
  pltdat.dom <- as.data.frame(dat.dom[pltassgn])
  pltdat.dom[is.na(pltdat.dom[[yn]]), yn] <- 0

  ## Subset response vector and change NA values of response to 0
  yn.vect <- pltdat.dom[[yn]]

  if (MAmethod == "HT") {
    estlst <- MAest.ht(yn.vect, N, FIA=FIA, 
			getweights=getweights)

  } else if (MAmethod == "PS") {
    x_sample <- pltdat.dom[, strvar][[1]]
    x_pop <- unitlut[, c(strvar, "Prop"), with=FALSE]

    estlst <- MAest.ps(yn.vect, N, x_sample, x_pop, FIA=FIA, 
			getweights=getweights)

  } else {

    x_sample <- setDF(pltdat.dom)[, prednames, drop=FALSE]
    x_pop <- setDF(unitlut)[, prednames, drop=FALSE]

    if (MAmethod == "greg") {
      estlst <- MAest.greg(yn.vect, N, x_sample, x_pop, FIA=FIA, 
		modelselect=modelselect, getweights=getweights)

    } else if (MAmethod == "gregEN") {
      estlst <- MAest.gregEN(yn.vect, N, x_sample, x_pop, FIA=FIA, 
				getweights=getweights)

    } else if (MAmethod == "ratio") {
      if (length(prednames) > 1) {
        stop("only one continuous predictor is allowed")
      } else {
        x_sample <- x_sample[[prednames]]
        x_pop <- x_pop[[prednames]]
      }
      est <- MAest.ratio(yn.vect, N, x_sample, x_pop, FIA=FIA,
			getweights=getweights)
  
    } else {
      stop("invalid MAmethod")
    }
  }

  if (getweights) {
<<<<<<< Updated upstream
    # TESTING 
    print(pltdat.dom[[cuniqueid]])
    print(estlst$weights)
    print(estlst)
    # TESTING
=======
    #print(pltdat.dom[[cuniqueid]])
    #print(estlst$weights)
    #print(estlst)
>>>>>>> Stashed changes
    estlst$weights <- data.frame(pltdat.dom[[cuniqueid]], estlst$weights)
    names(estlst$weights) <- c(cuniqueid, "weights")
  }

  return(estlst)
}


########################################################################
## By domain
########################################################################
MAest.dom <- function(dom, dat, cuniqueid, unitlut, pltassgn, esttype, MAmethod, 
		strvar=NULL, prednames=NULL, domain, N, response=NULL, FIA=TRUE,
		modelselect=FALSE, getweights=FALSE) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {
    domest <- data.table(dom, matrix(c(NA, NA, 0, 0), 1,4))
    setnames(domest, c(domain, "nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

    predselect <- data.table(dom, unitlut[FALSE, 
				unique(c(prednames, strvar), with=FALSE)])
    setnames(predselect, "dom", domain)
    returnlst <- list(est=domest, predselect=predselect)
    if (getweights) {
      weights <- data.frame(dom, id=cuniqueid, weights=NA)
      setnames(weights, "id", cuniqueid)
      setnames(weights, "dom", domain)
      returnlst$weights <- weights
    }
    return(returnlst)
  }

#yn=response

  ## Apply function to each dom
  domestlst <- MAest(yn=response, dat.dom=dat.dom, pltassgn=pltassgn, 
		cuniqueid=cuniqueid, esttype=esttype, unitlut=unitlut, 
		strvar=strvar, prednames=prednames, 
		MAmethod=MAmethod, N=N, FIA=FIA, modelselect=modelselect,
		getweights=getweights)
 
  domestlst <- lapply(domestlst, function(x, dom, domain) {
		dt <- data.table(dom, x) 
           setnames(dt, "dom", domain) }, dom, domain)
  return(domestlst)
}



########################################################################
## By estimation unit
########################################################################
MAest.unit <- function(unit, dat, cuniqueid, unitlut, unitvar, 
		esttype, MAmethod="HT", strvar=NULL, prednames=NULL, 
		domain, response, npixels, FIA=TRUE, modelselect=TRUE,
		getweights=FALSE) {
## testing
#unit = estunits[1]
#domain="TOTAL"

  dat.unit <- dat[dat[[unitvar]] == unit, c(cuniqueid, domain, response),
			with=FALSE]
  if (nrow(dat.unit) == 0 || sum(!is.na(dat.unit[[domain]])) == 0) {
    if (domain == "TOTAL") {
      unitest <- data.table(unit=unit, domain=1, nhat=0, nhat.var=0, 
		NBRPLT=0, NBRPLT.gt0=0)
    } else {
      unitest <- data.table(unit=unit, domain="TOTAL", nhat=0, nhat.var=0, 
		NBRPLT=0, NBRPLT.gt0=0)
    }  
    setnames(unitest, c("unit", "domain"), c(unitvar, domain)) 

    predselect <- data.table(unit=unit, domain=1, 
				unitlut[FALSE, c(prednames,strvar), with=FALSE])
    setnames(predselect, c("unit", "domain"), c(unitvar, domain)) 
    returnlst <- list(unitest=unitest, predselect=predselect)

    if (getweights) {
      weights <- data.frame(unit=unit, domain=1, id=cuniqueid, weights=NA)
      setnames(weights, "id", cuniqueid)
      returnlst$weights <- weights
    }
    return(returnlst)
  }
  setkeyv(dat.unit, cuniqueid)
  pltassgn.unit <- unique(dat[dat[[unitvar]] == unit, c(cuniqueid, strvar, prednames),
			with=FALSE])
  setkeyv(pltassgn.unit, cuniqueid)

  unitlut.unit <- unitlut[unitlut[[unitvar]] == unit, ]
  N.unit <-  npixels[["npixels"]][npixels[[unitvar]] == unit]

#  if (!MAmethod %in% c("HT", "PS")) {
#    if (any(pltassgn.unit[, colSums(.SD), .SDcols=prednames] < 3)) {
#      prednames <- prednames[pltassgn.unit[, colSums(.SD), .SDcols=prednames] >= 3]
#      if (length(prednames) == 0) MAmethod <- "HT"
#    }
#  }

  #doms <- as.character(unique(dat.unit[[domain]]))
  doms <- unique(dat.unit[!is.na(get(domain)) & get(domain) != "NA NA"][[domain]])

#dat=dat.unit
#unitlut=unitlut.unit
#pltassgn=pltassgn.unit
#N=N.unit
#dom=doms[1]

  unitestlst <- lapply(doms, MAest.dom, 
			dat=dat.unit, cuniqueid=cuniqueid, 
			unitlut=unitlut.unit, pltassgn=pltassgn.unit,
			esttype=esttype, MAmethod=MAmethod, 
			strvar=strvar, prednames=prednames, 
			domain=domain, N=N.unit, response=response,
			FIA=FIA, modelselect=modelselect, getweights=getweights)

  unitest <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "est")))
  setnames(unitest, "unit", unitvar)

  returnlst <- list(unitest=unitest)

  if (getweights) {
    weights <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "weights")))
    setnames(weights, "unit", unitvar)
    returnlst$weights <- weights
  }

  if (MAmethod %in% c("greg", "gregEN")) {
    predselect <- data.table(unit=unit, do.call(rbind, sapply(unitestlst, '[', "predselect")))
    setnames(predselect, "unit", unitvar)
    returnlst$predselect <- predselect
  }

  return(returnlst)
}


