SAest <- function(yn="CONDPROP_ADJ", pdomdat.dom, cuniqueid, dunitlut.dom, 
	dunitvar="DOMAIN", esttype="ACRES", SAmethod="unit", SApackage="JoSAE", 
	prednames=NULL, fmla, yd=NULL, ratiotype="PERACRE") {

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

  ## get mean response by domain and append to dunitlut
  #yn.dunit <- pdomdat[, lapply(.SD, mean), by=dunitvar, .SDcols=yn]
  #setkeyv(yn.dunit, dunitvar)
  #dunitlut.dom <- merge(dunitlut, yn.dunit, all.x=TRUE)
  #DT_NAto0(dunitlut.dom, yn, 0)

  ## Kick out domains (in dunitlut and pdomdat) that have less than 2 plots
  if (any(dunitlut.dom$n.total < 2)) {
    lt2 <- dunitlut.dom$DOMAIN[dunitlut.dom$n.total < 2]
    dunitlut.dom <- dunitlut.dom[dunitlut.dom$n.total >= 2,]
    message("removing domain(s) with less than 2 plots: ", toString(lt2))

    pdomdat.dom <- pdomdat.dom[!pdomdat.dom[[dunitvar]] %in% lt2,]
  }

  ## Check if all plots are zero
  if (sum(pdomdat.dom[[yn]]) == 0) {
    message(yn, " has all 0 values... returning NULL")
    return(NULL)
  }  

  ## Calculate number of non-zero plots
  NBRPLT.gt0 <- pdomdat.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")


  if (standardize) {
    ## Standardize predictors
    pdomdat.dom[, (prednames) := lapply(.SD, function(x) x / max(x)), .SDcols=prednames]
    dunitlut.dom[, (prednames) := lapply(.SD, function(x) x / max(x)), .SDcols=prednames]
  }
 
  # variable selection for unit-level model for largebnd value
  #mod.dom <- stats::lm(fmla, data=pdomdat.dom)
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
    mod1 <- mase::gregElasticNet(y=pdomdat.dom[[yn]], 
		x_sample=setDF(pdomdat.dom[,prednames, with=FALSE]), 
		x_pop=setDF(dunitlut.dom), pi = NULL, alpha = .5,
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
  fmla.dom <- stats::as.formula(paste(yn, 
		paste(prednames, collapse= "+"), sep="~"))

  if (SAmethod == "unit") {

    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom), data=pdomdat.dom, random=~1|DOMAIN)))
    
    ## calculate the variance of the EBLUP estimate
    est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.dom, 
				lme.obj = dom.lme, debug=FALSE)

    ## subset dataframe before returning
    est <- est.unit[,c("DOMAIN.domain", yn,
					"sample.se", "Synth", 
					"GREG", "GREG.se",
					"EBLUP","EBLUP.se.1", "n.i.sample"), with=FALSE]
    names(est) <- c("DOMAIN", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
			       "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT")

    ## Merge AOI and number of plots greater than 0
    est <- merge(est, NBRPLT.gt0, by="DOMAIN")
  }

  if (SAmethod == "area") {
    xpop.dom <- paste0(prednames, ".X.pop")
    fmla.dom2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
				paste(xpop.dom, collapse= "+"), sep="~"))
    res <-
    JoSAE::sae.ul.f(samp.data = pdomdat.dom,
             population.data = dunitlut.dom,
             k.ij = rep(1,nrow(pdomdat.dom)),
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

    ## eliminating where non-zero plot size < 2
    dat.al <- merge(dat.al, NBRPLT.gt0, by.x="domain.id", by.y="DOMAIN")
    dat.al <- dat.al[dat.al$NBRPLT.gt0 > 1,]

    est.area <- JoSAE::sae.al.f(
    		domain.id=dat.al$domain.id, n.i=dat.al$n.i, psi.i=dat.al$se.srs^2,
    		formula=fmla.dom2, data=dat.al,
    		b.i=rep(1, nrow(dat.al)),
    		type="RE")
    est <- est.area$results[,1:7]
    names(est) <- c("DOMAIN","DIR", "DIR.se", "JFH", "JFH.se", 
				"JA.synth",  "JA.synth.se") 

    est <- merge(est, dat.al[, c("domain.id", "n.i", "NBRPLT.gt0")], 
		by.x="DOMAIN", by.y="domain.id") 
    names(est)[names(est) == "n.i"] <- "NBRPLT"
  }

  ## Merge AOI
  if (!"AOI" %in% names(est)) {
    est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
  }
}


########################################################################
## By domain
########################################################################
SAest.dom <- function(dom, pdomdat, cuniqueid, dunitlut, dunitvar="DOMAIN", 
		esttype, SApackage, SAmethod, prednames=NULL, fmla, 
		domain, response=NULL) {

  ## Subset tomdat to domain=dom
  pdomdat.dom <- pdomdat[pdomdat[[domain]] == dom,] 
#dunitlut.dom=dunitlut

  ## Apply function to each dom
  domest <- data.table(dom, SAest(yn=response, pdomdat.dom=pdomdat.dom, 
			cuniqueid=cuniqueid, esttype=esttype, dunitlut.dom=dunitlut, 
			dunitvar=dunitvar, prednames=prednames, fmla=fmla, 
			SApackage=SApackage, SAmethod=SAmethod))
  setnames(domest, "dom", domain)
  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, pdomdat, cuniqueid, largebnd.att, 
		dunitlut, dunitvar="DOMAIN", esttype, SApackage="JoSAE", 
		SAmethod="unit", fmla, domain, response, prednames=NULL) {
  message("generating estimate for: ", largebnd.val)


  ## subset datasets by largebnd value (e.g., ecosection)
  pdomdat.large <- pdomdat[pdomdat[[largebnd.att]] == largebnd.val, ]
  if (nrow(pdomdat.large) == 0) stop("invalid largebnd.val")
  setkeyv(pdomdat.large, cuniqueid)

  ## get unique domain units and subset domain lut for largebnd value
  dunits <- sort(unique(pdomdat.large[[dunitvar]]))
  dunitlut.large <- dunitlut[dunitlut[[dunitvar]] %in% dunits,]
     
  ## get unique domains
  doms <- as.character(na.omit(unique(pdomdat.large[[domain]])))

#pdomdat=pdomdat.large
#dunitlut=dunitlut.large
#dom=doms

  domest <- do.call(rbind, lapply(doms, SAest.dom, 
			pdomdat=pdomdat.large, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut.large, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod=SAmethod, 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 
  setkeyv(domest, dunitvar)
}       

