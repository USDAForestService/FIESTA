SAest <- function(yn="CONDPROP_ADJ", dat, cuniqueid, pltmodelx, puniqueid,
	dunitlut=NULL, esttype="ACRES", bytdom=FALSE, 
	SApackage="JoSAE", SAmethod="unit", 
	prednames=NULL, fmla, yd=NULL, ratiotype="PERACRE") {

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
  dunitvar <- "DOMAIN"

  ## Sum response to domain and merge to pltmodelx
  dat.dom <- dat[, sum(get(yn)), by=cuniqueid]
  setnames(dat.dom, "V1", yn)
  dat.dom <- merge(pltmodelx[, c(puniqueid, dunitvar, prednames), with=FALSE], 
				dat.dom, by.x=puniqueid, by.y=cuniqueid, all.x=TRUE)
  dat.dom <- DT_NAto0(dat.dom, yn, 0)

  ## get mean response by domain and append to dunitlut
  yn.dunit <- dat.dom[, lapply(.SD, mean), by=dunitvar, .SDcols=yn]
  setkey(yn.dunit, "DOMAIN")
  dunitlut.dom <- merge(dunitlut, yn.dunit, all.x=TRUE)
  DT_NAto0(dunitlut.dom, yn, 0)

  NBRPLT.gt0 <- dat.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")

  # variable selection for unit-level model for largebnd value
  mod.dom <- stats::lm(fmla, data=dat.dom)
  mod.dom.step <- stats::step(mod.dom)
  mod.summary <- summary(mod.dom.step)
  preds.dom <- names(mod.dom.step$model[-1])

  ## create new model formula with variables selected from step procedure
  ## note: the variables selected can change depending on the order in original formula (fmla)
  fmla.dom <- stats::as.formula(paste(yn, paste(preds.dom, collapse= "+"), sep="~"))

  if (SAmethod == "unit") {
    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom), data=dat.dom, random=~1|DOMAIN)))
    

    ## calculate the variance of the EBLUP estimate
    est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.dom, lme.obj = dom.lme, debug=FALSE)

    ## subset dataframe before returning
    est <- est.unit[,c("DOMAIN.domain", yn,
					"sample.se", "Synth", 
					"GREG", "GREG.se",
					"EBLUP","EBLUP.se.1", "n.i.sample"), with=FALSE]
    names(est) <- c("DOMAIN", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
			       "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT")
  }

  if (SAmethod == "area") {
    xpop.dom <- paste0(preds.dom, ".X.pop")
    fmla.dom2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
				paste(xpop.dom, collapse= "+"), sep="~"))

    res <-
    JoSAE::sae.ul.f(samp.data = dat.dom,
             population.data = dunitlut.dom,
             k.ij = rep(1,nrow(dat.dom)),
             formula = fmla.dom,
             domain.col = "DOMAIN",
             sample.id.col = puniqueid,
             neg.sfrac = TRUE)

    #building pieces		
    partA <- res$data$samp.agg.X.pop[,c("domain.id","n.i",
				paste0(yn,".ybar.i"),
				xpop.dom)]
    partB <- res$est$se[,c("domain.id","se.srs")]
    dat.al <- merge(partA, partB)

    ## eliminating where plot size < 2
    tmp <- dat.al$n.i
    dat.al <- dat.al[tmp > 1,]

    est.area <- JoSAE::sae.al.f(
    		domain.id=dat.al$domain.id , n.i=dat.al$n.i , psi.i=dat.al$se.srs^2,
    		formula=fmla.dom2, data=dat.al,
    		b.i=rep(1, nrow(dat.al)),
    		type="RE")
    est <- est.area$results[,1:7]
    names(est) <- c("DOMAIN","y.sam", "direct.se", "JFH", "JFH.se", 
				"JA.synth",  "JA.synth.se")  

    ## Get number of plots by domain
    NBRPLT <- dat.dom[, length(get(yn)), by=dunitvar]
    setnames(NBRPLT, "V1", "NBRPLT")

    est <- merge(est, NBRPLT, by="DOMAIN")
  }

  ## Merge AOI and number of plots greater than 0
  if ("AOI" %in% names(est)) 
    est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")])
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")
}


## Define functions
SAest.dom <- function(dom, dat, cuniqueid, pltmodelx, puniqueid, 
		dunitlut, esttype, bytdom=FALSE, SApackage, SAmethod, 
		prednames=NULL, fmla, domain, response=NULL, tdomvarlst=NULL) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 

#yn=response
#dat <- dat.dom

  ## If tdom (e.g., SPCD), apply function to each tdom in tdomvarlst
  if (bytdom) {
    domest <- data.table(dom, 
		do.call(rbind, lapply(tdomvarlst, SAest, 	
			dat=dat.dom, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx, puniqueid=puniqueid, 
			esttype=esttype, bytdom=bytdom, 
			dunitlut=dunitlut, prednames=prednames, fmla=fmla,
 			SApackage=SApackage, SAmethod=SAmethod)))
  } else {
    domest <- data.table(dom, SAest(yn=response, 
			dat=dat.dom, cuniqueid=cuniqueid, 
 			pltmodelx=pltmodelx, puniqueid=puniqueid, 
			esttype=esttype, bytdom=bytdom, 
			dunitlut=dunitlut, prednames=prednames, fmla=fmla,
			SApackage=SApackage, SAmethod=SAmethod))
  }
  setnames(domest, "dom", domain)
  return(domest)
}

SAest.large <- function(largebnd.val, dat, cuniqueid, pltmodelx, puniqueid, 
		largebnd.att, dunitlut, esttype, bytdom=FALSE,
 		SApackage="JoSAE", SAmethod="unit", prednames=NULL, 
		fmla, domain, response, tdomvarlst=NULL) {

  message("generating estimate for: ", largebnd.val)

  ## subset datasets by largebnd value (e.g., ecosection)
  pltmodelx.large <- pltmodelx[pltmodelx[[largebnd.att]] == largebnd.val]
  if (nrow(pltmodelx.large) == 0) stop("invalid largebnd.val")
  dat.large <- dat[dat[[cuniqueid]] %in% pltmodelx.large[[puniqueid]], ]

  ## get unique domain units and subset domain lut for largebnd value
  dunits <- sort(unique(pltmodelx.large[["DOMAIN"]]))
  dunitlut.large <- dunitlut[dunitlut[["DOMAIN"]] %in% dunits,]
     
  ## get unique domains
  doms <- as.character(unique(dat.large[[domain]]))

#dat=dat.large
#pltmodelx=pltmodelx.large
#dunitlut=dunitlut.large
#dom=doms

  domest <- do.call(rbind, lapply(doms, SAest.dom, 
			dat=dat.large, cuniqueid=cuniqueid, 
			pltmodelx=pltmodelx.large, puniqueid=puniqueid,
     			dunitlut=dunitlut.large,
			esttype=esttype, bytdom=bytdom,
			SApackage=SApackage, SAmethod=SAmethod, 
			prednames=prednames, fmla=fmla,
			domain=domain, 
			response=response, tdomvarlst=tdomvarlst)) 
  setkey(domest, "DOMAIN")
}       

