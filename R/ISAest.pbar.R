SAest <- function(yn="CONDPROP_ADJ", plt.dom, cuniqueid, dunitlut=NULL, 
	dunitvar="DOMAIN", esttype="ACRES", SAmethod="unit", SApackage="JoSAE", 
	prednames=NULL, fmla, yd=NULL, ratiotype="PERACRE") {

  ########################################################################################
  ## DESCRIPTION: Gets estimates from JoSAE
  ## PARAMETERS:
  ## yn 		- response (numerator)
  ## plt.dom 	- plt/domain-level data set
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

  ## get mean response by domain and append to dunitlut
  yn.dunit <- plt.dom[, lapply(.SD, mean), by=dunitvar, .SDcols=yn]
  setkeyv(yn.dunit, dunitvar)
  dunitlut.dom <- merge(dunitlut, yn.dunit, all.x=TRUE)
  #DT_NAto0(dunitlut.dom, yn, 0)

  ## Kick out domains (in dunitlut and plt.dom) that have less than 2 plots
  if (any(dunitlut.dom$n.total < 2)) {
    lt2 <- dunitlut.dom$DOMAIN[dunitlut.dom$n.total < 2]
    dunitlut.dom <- dunitlut.dom[dunitlut.dom$n.total >= 2,]
    message("removing domain(s) with less than 2 plots: ", toString(lt2))

    plt.dom <- plt.dom[!plt.dom[[dunitvar]] %in% lt2,]
  }

  ## Check if all plots are zero
  if (sum(plt.dom[[yn]]) == 0) {
    message(yn, " has all 0 values... returning NULL")
    return(NULL)
  }  

  ## Calculate number of non-zero plots
  NBRPLT.gt0 <- plt.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
 
  # variable selection for unit-level model for largebnd value
  mod.dom <- stats::lm(fmla, data=plt.dom)
  mod.dom.step <- stats::step(mod.dom)
  mod.summary <- summary(mod.dom.step)
  preds.dom <- names(mod.dom.step$model[-1])

  ## create new model formula with variables selected from step procedure
  ## note: the variables selected can change depending on the order in original formula (fmla)
  fmla.dom <- stats::as.formula(paste(yn, paste(preds.dom, collapse= "+"), sep="~"))
 
  if (SAmethod == "unit") {
    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom), data=plt.dom, random=~1|DOMAIN)))
    
    ## calculate the variance of the EBLUP estimate
    est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.dom, lme.obj = dom.lme, debug=FALSE)

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

    xpop.dom <- paste0(preds.dom, ".X.pop")
    fmla.dom2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
				paste(xpop.dom, collapse= "+"), sep="~"))
#save(plt.dom, file=paste0("outfolder/RAVGyr2018/341/plt.dom_", yn, ".rda"))
#save(dunitlut.dom, file=paste0("outfolder/RAVGyr2018/341/dunitlut.dom_", yn, ".rda"))

    res <-
    JoSAE::sae.ul.f(samp.data = plt.dom,
             population.data = dunitlut.dom,
             k.ij = rep(1,nrow(plt.dom)),
             formula = fmla.dom,
             domain.col = "DOMAIN",
             sample.id.col = cuniqueid,
             neg.sfrac = TRUE)
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
    		domain.id=dat.al$domain.id , n.i=dat.al$n.i , psi.i=dat.al$se.srs^2,
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
  if (!"AOI" %in% names(est))
    est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
 
}


########################################################################
## By domain
########################################################################
SAest.dom <- function(dom, plt.dom, cuniqueid, dunitlut, dunitvar="DOMAIN", 
		esttype, SApackage, SAmethod, prednames=NULL, fmla, 
		domain, response=NULL) {

  ## Subset tomdat to domain=dom
  plt.dom <- plt.dom[plt.dom[[domain]] == dom,] 

#yn=response

  ## Apply function to each dom
  domest <- data.table(dom, SAest(yn=response, plt.dom=plt.dom, 
			cuniqueid=cuniqueid, esttype=esttype, dunitlut=dunitlut, 
			dunitvar=dunitvar, prednames=prednames, fmla=fmla, 
			SApackage=SApackage, SAmethod=SAmethod))
  setnames(domest, "dom", domain)
  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, plt.dom, cuniqueid, largebnd.att, 
		dunitlut, dunitvar="DOMAIN", esttype, SApackage="JoSAE", 
		SAmethod="unit", fmla, domain, response, prednames=NULL) {
  message("generating estimate for: ", largebnd.val)


  ## subset datasets by largebnd value (e.g., ecosection)
  plt.dom.large <- plt.dom[plt.dom[[largebnd.att]] == largebnd.val, ]
  if (nrow(plt.dom.large) == 0) stop("invalid largebnd.val")
  setkeyv(plt.dom.large, cuniqueid)

  ## get unique domain units and subset domain lut for largebnd value
  dunits <- sort(unique(plt.dom.large[[dunitvar]]))
  dunitlut.large <- dunitlut[dunitlut[[dunitvar]] %in% dunits,]
     
  ## get unique domains
  doms <- as.character(unique(plt.dom.large[[domain]]))

#plt.dom=plt.dom.large
#dunitlut=dunitlut.large
#dom=doms

  domest <- do.call(rbind, lapply(doms, SAest.dom, 
			plt.dom=plt.dom.large, cuniqueid=cuniqueid, 
     			dunitlut=dunitlut.large, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod=SAmethod, 
			esttype=esttype, prednames=prednames, fmla=fmla,
			domain=domain, response=response)) 
  setkeyv(domest, dunitvar)
}       

