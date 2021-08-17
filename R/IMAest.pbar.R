
MAest.ht <- function(y, N, FIA=TRUE) {

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "LinHTSRS"
  estht <- mase::horvitzThompson(y, pi = NULL, N = N, pi2 = NULL, 
						var_est = TRUE, var_method = var_method, 
						B = 1000)

  estht <- data.table(estht$pop_mean, estht$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estht, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estht[, nhat.var := nhat.var / (1 - length(y) / N)]
  }

  return(estht)
}


MAest.ps <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE) {

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
    return(estps)
  }

  estps <- data.table(estps$pop_mean, estps$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estps, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))

  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estps[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estps)
}


MAest.greg <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE) {

#y <- yn.vect

  ## Set global variables
  nhat.var <- NULL

  NBRPLT <- length(y)
  NBRPLT.gt0 <- sum(y > 0)
  var_method <- "LinHTSRS"
  estgreg <- tryCatch(mase::greg(	y = y, 
					xsample = x_sample, 
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
    return(estgreg)
  }
  estgreg <- data.table(estgreg$pop_mean, estgreg$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgreg, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
 
  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estgreg[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estgreg)
}


MAest.ratio <- function(y, N, x_sample, x_pop, FIA=TRUE, save4testing=TRUE) {

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
    return(estratio)
  }

  estratio <- data.table(estratio$pop_mean, estratio$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estratio, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
 
  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estratio[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estratio)
}



MAest.gregEN <- function(y, N, x_sample, x_pop, FIA=TRUE, model="linear", save4testing=TRUE) {

#y <- yn.vect
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
  preds <- names(estgregEN$coefficients[estgregEN$coefficients > 0][-1])
  #message("predictors used for estimate: ", toString(preds))
  #print(paste("predictors used for estimate:", toString(preds)))

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
    return(estgregEN)
  }
  estgregEN <- data.table(estgregEN$pop_mean, estgregEN$pop_mean_var, NBRPLT, NBRPLT.gt0)
  setnames(estgregEN, c("nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
 
  if (FIA) {
    ## This takes out the finite population correction term (to estimated variance from FIA)
    estgregEN[, nhat.var := nhat.var / (1 - length(y) / N)]
  }
  return(estgregEN)
}


########################################################################
## Get estimates
########################################################################
MAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, unitlut=NULL, 
	pltassgn, esttype="ACRES", MAmethod, strvar=NULL, prednames=NULL, 
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
    est <- MAest.ht(yn.vect, N, FIA=FIA)

  } else if (MAmethod == "PS") {
    x_sample <- pltdat.dom[, strvar, with=FALSE][[1]]
    x_pop <- unitlut[, c(strvar, "Prop"), with=FALSE]
    est <- MAest.ps(yn.vect, N, x_sample, x_pop, FIA=FIA)

  } else {

    x_sample <- setDF(pltdat.dom[, prednames, with=FALSE])
    x_pop <- setDF(unitlut[, prednames, with=FALSE])

    if (MAmethod == "greg") {
      est <- MAest.greg(yn.vect, N, x_sample, x_pop, FIA=FIA)

    } else if (MAmethod == "gregEN") {
      est <- MAest.gregEN(yn.vect, N, x_sample, x_pop, FIA=FIA)

    } else if (MAmethod == "ratio") {
      if (length(prednames) > 1) {
        stop("only one continuous predictor is allowed")
      } else {
        x_sample <- x_sample[[prednames]]
      }
      est <- MAest.ratio(yn.vect, N, x_sample, x_pop, FIA=FIA)
  
    } else {
      stop("invalid MAmethod")
    }
  }

  return(est)
}


########################################################################
## By domain
########################################################################
MAest.dom <- function(dom, dat, cuniqueid, unitlut, pltassgn, esttype, MAmethod, 
		strvar=NULL, prednames=NULL, domain, N, response=NULL, FIA=TRUE) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {
    domest <- data.table(dom, matrix(c(NA, NA, 0, 0), 1,4))
    setnames(domest, c(domain, "nhat", "nhat.var", "NBRPLT", "NBRPLT.gt0"))
    return(domest)
  }

#yn=response

  ## Apply function to each dom
  domest <- data.table(dom, suppressMessages(MAest(yn=response, 
		dat.dom=dat.dom, pltassgn=pltassgn, 
		cuniqueid=cuniqueid, esttype=esttype, unitlut=unitlut, 
		strvar=strvar, prednames=prednames, 
		MAmethod=MAmethod, N=N, FIA=FIA)))
  setnames(domest, "dom", domain)
  return(domest)
}



########################################################################
## By estimation unit
########################################################################
MAest.unit <- function(unit, dat, cuniqueid, unitlut, unitvar, 
		esttype, MAmethod="HT", strvar=NULL, prednames=NULL, 
		domain, response, npixels, FIA=TRUE) {
## testing
#unit=1
#unit="1701020805"
#unit="0904000104"
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
    return(unitest)
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
  unitest <- do.call(rbind, lapply(doms, MAest.dom, 
			dat=dat.unit, cuniqueid=cuniqueid, 
			unitlut=unitlut.unit, pltassgn=pltassgn.unit,
			esttype=esttype, MAmethod=MAmethod, 
			strvar=strvar, prednames=prednames, 
			domain=domain, N=N.unit, response=response,
			FIA=FIA))
  unitest <- data.table(unit=unit, unitest)
  setnames(unitest, "unit", unitvar)
  return(unitest)
}


