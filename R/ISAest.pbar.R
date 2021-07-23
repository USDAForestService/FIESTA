SAest.unit <- function(fmla.dom, pltdat.dom, dunitlut, yn) {

  ## create linear mixed model
  ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
  dom.lme <- eval(bquote( nlme::lme(.(fmla.dom), data=pltdat.dom, random=~1|DOMAIN)))
    
  ## calculate the variance of the EBLUP estimate
  est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut, 
				lme.obj = dom.lme, debug=FALSE)

  ## subset dataframe before returning
  est <- est.unit[,c("DOMAIN.domain", 
					yn, "sample.se", "Synth", 
					"GREG", "GREG.se",
					"EBLUP","EBLUP.se.1", "n.i.sample"), with=FALSE]
  names(est) <- c("DOMAIN", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
			       "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT")
  return(est)

}

SAest.area <- function(fmla.dom, pltdat.dom, dunitlut, cuniqueid, 
	dunitvar, prednames, yn) {
  ## Remove response values equal to 1
  dunitlut.NA <- dunitlut[is.na(dunitlut$mean.var) | dunitlut$mean.var == 0, ]
  dunitNAids <- dunitlut.NA[[dunitvar]]
  dunitids <-  dunitlut[!dunitlut[[dunitvar]] %in% dunitNAids, 
			dunitvar, with=FALSE][[1]]
  dunitlut.area <- dunitlut[dunitlut[[dunitvar]] %in% dunitids, ]
  pltdat.area <- pltdat.dom[pltdat.dom[[dunitvar]] %in% dunitids, ]

  xpop.dom <- paste0(prednames, ".X.pop")
  fmla.dom2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
				paste(xpop.dom, collapse= "+"), sep="~"))
  res <-
    JoSAE::sae.ul.f(samp.data = pltdat.area,
             population.data = dunitlut.area,
             k.ij = rep(1,nrow(pltdat.area)),
             formula = fmla.dom,
             domain.col = "DOMAIN",
             sample.id.col = cuniqueid,
             neg.sfrac = TRUE)

  ## To add space to messages
  cat("\n")

  #building pieces		
  partA <- res$data$samp.agg.X.pop[,c("domain.id","n.i",
				paste0(yn,".ybar.i"),
				xpop.dom)]
  partB <- res$est$se[,c("domain.id","se.srs")]
  dat.al <- merge(partA, partB)

  est.area <- JoSAE::sae.al.f(
    		domain.id=dat.al$domain.id, n.i=dat.al$n.i, psi.i=dat.al$se.srs^2,
    		formula=fmla.dom2, data=dat.al,
    		b.i=rep(1, nrow(dat.al)),
    		type="RE")
  est <- est.area$results[,1:7]
  names(est) <- c("DOMAIN", "DIR", "DIR.se", "JFH", "JFH.se",
			       "JA.synth", "JA.synth.se")
 
  ## To add space to messages
  cat("\n")

  est <- merge(est, dat.al[, c("domain.id", "n.i")], 
		by.x="DOMAIN", by.y="domain.id") 
  names(est)[names(est) == "n.i"] <- "NBRPLT"

  if (nrow(dunitlut.NA) > 0) {
    est.NA <- data.table(dunitlut.NA[[dunitvar]], DIR=NA, DIR.se=NA, 
		JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA, NBRPLT=dunitlut.NA$n.total)
    setnames(est.NA, "V1", dunitvar)
    est <- rbindlist(list(est, est.NA))
    setorderv(est, dunitvar)
  }
}


SAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, pltassgn,
	dunitlut, prednames=NULL, fmla, dunitvar="DOMAIN", 
	SAmethod="unit", SApackage="JoSAE", yd=NULL, ratiotype="PERACRE") {

  ########################################################################################
  ## DESCRIPTION: Gets estimates from JoSAE
  ## PARAMETERS:
  ## yn 		- response (numerator)
  ## pdomdat 	- plt/domain-level data set
  ## dunitlut	- predictor summaries
  ## prednames	- predictor variable names to use in model
  ## 
  ## VALUES:
  ## nhat		- estimate proportion of land covered by condition, for numerator
  ## nhat.var	- variance of estimated proportion, for numerator
  ## dhat		- estimate proportion of land covered by condition, for denominator
  ## dhat.var	- variance of estimated proportion, for denominator
  ## covar		- covariance of numerator and denominator
  ########################################################################################
  #dunitvar <- "DOMAIN"
  standardize <- TRUE
  variable.select <- FALSE
  if (variable.select) {
    if (!"mase" %in% rownames(installed.packages()))
    stop("variable selection requires package mase")
  }

  ## Merge dat.dom to pltassgn
  pltdat.dom <- dat.dom[pltassgn]
  pltdat.dom[is.na(pltdat.dom[[yn]]), (yn) := 0]

  ## Check if all plots are zero
  if (sum(pltdat.dom[[yn]]) == 0) {
    message(yn, " has all 0 values... returning NULL")
    return(NULL)
  }  

  ## Calculate number of non-zero plots
  NBRPLT.gt0 <- pltdat.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
  setkeyv(NBRPLT.gt0, dunitvar)

  if (standardize) {
    ## Standardize predictors at the scale at which borrowing occurs (e.g., PROVINCE/SECTION)
    ## So, not standardized at national level
    pltdat.dom[, (prednames) := lapply(.SD, function(x) x / max(x)), .SDcols=prednames]
    dunitlut[, (prednames) := lapply(.SD, function(x) x / max(x)), .SDcols=prednames]
  }
 
  # variable selection for unit-level model for largebnd value
  #mod.dom <- stats::lm(fmla, data=pltdat.dom)
  #mod.dom.step <- stats::step(mod.dom, trace=FALSE)
  #mod.summary <- summary(mod.dom.step)
  #preds.dom <- names(mod.dom.step$model[-1])

  #if (length(preds.dom) == 0) {
  #  message("no predictors were selected for model")
  #  return(NULL)
  #}

  if (variable.select) {
    #cor(std.plt.dom[, prednames])

    ## select predictor variables from Elastic Net procedure
    mod1 <- mase::gregElasticNet(y=pltdat.dom[[yn]], 
		x_sample=setDF(pltdat.dom[,prednames, with=FALSE]), 
		x_pop=setDF(dunitlut), pi = NULL, alpha = .5,
  		model = "linear", pi2 = NULL, var_est = FALSE,
  		var_method = "lin_HB", data_type = "raw", N = NULL,
  		lambda = "lambda.1se", B = 1000, cvfolds = 10, strata = NULL)
    mod1$coefficients[-1]
    mod1.rank <- rank(-abs(mod1$coefficients[-1]))
    preds.enet <- names(mod1$coefficients[-1])[abs(mod1$coefficients[-1])>0]

    if (length(preds.enet) == 0) {
      message("no predictors were selected for model")
    		return(NULL)
    } else {
      prednames <- preds.enet
    }
  }

  ## create model formula with predictors
  ## note: the variables selected can change depending on the order in original formula (fmla)
  fmla.dom <- stats::as.formula(paste(yn, paste(prednames, collapse= "+"), sep="~"))

  if (SAmethod == "unit") {
    est <- tryCatch(SAest.unit(fmla.dom, pltdat.dom, dunitlut, yn),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(est)) {
      est <- data.table(dunitlut[[dunitvar]],
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut$n.total)
      setnames(est, "V1", dunitvar)
    }
  }


  if (SAmethod == "area") {
    est <- tryCatch(SAest.area(fmla.dom, pltdat.dom, dunitlut, 
				cuniqueid, dunitvar, prednames, yn),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(est)) {
      est <- data.table(dunitlut[[dunitvar]], AOI=dunitlut$AOI,
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut$n.total)
      setnames(est, "V1", dunitvar)
    }
  }

  ## Merge NBRPLT.gt0
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")

  ## Merge AOI
  if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut)) {
    est <- merge(est, dunitlut[, c("DOMAIN", "AOI")], by="DOMAIN")
  }
  return(est)
}


########################################################################
## By domain
########################################################################
SAest.dom <- function(dom, dat, cuniqueid, dunitlut, pltassgn, dunitvar="DOMAIN", 
		SApackage, SAmethod, prednames=NULL, fmla, domain, response=NULL) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {
    if (SAmethod == "unit") {
      domest <- data.table(dom, matrix(c(rep(NA,7), 0, 0), 1,9))
      setnames(domest, c(domain, "DIR", "DIR.se", "JU.Synth", "JU.GREG",
		"JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT", "NBRPLT.gt0"))

    } else if (SAmethod == "area") {
      domest <- data.table(dom, matrix(c(rep(NA,6), 0, 0), 1,8))
      setnames(domest, c(domain, "DIR", "DIR.se", "JFH", "JFH.se",
		"JA.synth", "JA.synth.se", "NBRPLT", "NBRPLT.gt0"))
    } 
    return(domest)
  }

#yn=response

  ## Apply function to each dom
  domest <- data.table(dom, SAest(yn=response, 
			dat.dom=dat.dom, cuniqueid=cuniqueid, pltassgn=pltassgn,
			dunitlut=dunitlut, prednames=prednames, fmla=fmla, 
			SApackage=SApackage, SAmethod=SAmethod))
  setnames(domest, "dom", domain)
  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, dat, cuniqueid, largebnd.att, 
		dunitlut, dunitvar="DOMAIN", SApackage="JoSAE", 
		SAmethod="unit", fmla, domain, response, prednames=NULL) {

  ## subset datasets by largebnd value (e.g., ecosection)
  dat.large <- dat[dat[[largebnd.att]] == largebnd.val, 
		c(dunitvar, cuniqueid, domain, response), with=FALSE]
  if (nrow(dat.large) == 0) stop("invalid largebnd.val")
  setkeyv(dat.large, c(dunitvar, cuniqueid))

  pltassgn.large <- unique(dat[dat[[largebnd.att]] == largebnd.val, 
		c(dunitvar, cuniqueid, prednames), with=FALSE])
  setkeyv(pltassgn.large, c(dunitvar, cuniqueid))

  ## get unique domain units and subset domain lut for largebnd value
  dunits <- sort(unique(dat.large[[dunitvar]]))
  dunitlut.large <- dunitlut[dunitlut[[dunitvar]] %in% dunits,]
     
  ## get unique domains
  doms <- as.character(na.omit(unique(dat.large[[domain]])))

#dat=dat.large
#dunitlut=dunitlut.large
#pltassgn=pltassgn.large
#dom=doms

  domest <- do.call(rbind, lapply(doms, SAest.dom, 
			dat=dat.large, cuniqueid=cuniqueid, pltassgn=pltassgn.large,
     			dunitlut=dunitlut.large, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod=SAmethod, 
			prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 
  setkeyv(domest, dunitvar)
}       

