SAest.unit <- function(fmla.dom, pltdat.dom, dunitlut.dom, yn, SApackage, prior = NULL) {
  
  if (SApackage == "JoSAE") {
    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom), data=pltdat.dom, random=~1|DOMAIN)))
    
    ## calculate the variance of the EBLUP estimate
    est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.dom, 
                                        lme.obj = dom.lme, debug=FALSE)
    
    ## subset dataframe before returning
    est <- est.unit[,c("DOMAIN.domain", 
                       yn, "sample.se", "Synth", 
                       "GREG", "GREG.se",
                       "EBLUP","EBLUP.se.1", "n.i.sample")]
    names(est) <- c("DOMAIN", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
                    "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT")
    return(est)
  }
  
  if (SApackage == "sae") {
    xpop <- setDF(dunitlut.dom)[,c('DOMAIN', prednames)]
    popsize <- setDF(dunitlut.dom)[, c("DOMAIN", "npixels")]
    
    est.unit <- sae::pbmseBHF(formula = fmla.dom,
                              dom = DOMAIN,
                              selectdom = unique(xpop$DOMAIN),
                              meanxpop = xpop,
                              popnsize = popsize,
                              method = "REML",
                              data = pltdat.dom,
                              B = 200)
    
    est <- data.frame(
      DOMAIN = est.unit$est$eblup$domain,
      est = est.unit$est$eblup$eblup,
      est.se = sqrt(est.unit$mse[["mse"]])
    )
    
    return(est)
  }
  
  if (SApackage == "hbsae") {
    xpophb <- model.matrix(fmla.dom[-2], dunitlut.dom)
    rownames(xpophb) <- dunitlut.dom$DOMAIN
    
    if (is.null(prior)) {
      est.unit <- hbsae::fSAE.Unit(
        y = pltdat.dom[[yn]],
        X = model.matrix(fmla.dom[-2], pltdat.dom),
        area = pltdat.dom[["DOMAIN"]],
        Narea = dunitlut.dom[, c("npixels")],
        Xpop = xpophb 
      )
    } else {
      est.unit <- hbsae::fSAE.Unit(
        y = pltdat.dom[[yn]],
        X = model.matrix(fmla.dom[-2], pltdat.dom),
        area = pltdat.dom[["DOMAIN"]],
        Narea = dunitlut.dom[, c("npixels")],
        Xpop = xpophb,
        prior = prior
      )
    }
    
    est <- data.frame(
      DOMAIN = est.unit$sampledAreaNames,
      est = est.unit$est,
      est.se = sqrt(est.unit$mse)
    )
    
    return(est)
  }

}

SAest.area <- function(fmla.dom, pltdat.dom, dunitlut.dom, cuniqueid, 
	dunitvar, prednames, yn, SApackage, prior=NULL) {
  
  if (SApackage == "JoSAE") {
    ## Remove response values equal to 1
    #  dunitlut.dom.NA <- dunitlut.dom[is.na(dunitlut.dom$mean.var) | dunitlut.dom$mean.var == 0 |
    #		dunitlut.dom$n.total < 2, ]
    
    dunitlut.dom <- setDT(dunitlut.dom)
    nm.var <- paste0(yn, ".var")
    dunitlut.NA <- dunitlut.dom[is.na(dunitlut.dom[[nm.var]]) | dunitlut.dom[[nm.var]] == 0, ]
    dunitNAids <- dunitlut.NA[[dunitvar]]
    dunitids <-  dunitlut.dom[!dunitlut.dom[[dunitvar]] %in% dunitNAids, 
                              dunitvar, with=FALSE][[1]]
    dunitlut.area <- dunitlut.dom[dunitlut.dom[[dunitvar]] %in% dunitids, ]
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
    
    est.area <- suppressWarnings(JoSAE::sae.al.f(
      domain.id=dat.al$domain.id, n.i=dat.al$n.i, psi.i=dat.al$se.srs^2,
      formula=fmla.dom2, data=dat.al,
      b.i=rep(1, nrow(dat.al)),
      type="RE"))
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
				JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA, 
				NBRPLT=dunitlut.NA$n.total)
      setnames(est.NA, "V1", dunitvar)
      est <- rbindlist(list(est, est.NA))
      setorderv(est, dunitvar)
    }
    return(est)
  }
  
  if (SApackage == "sae") {
    nm.var <- paste0(yn, ".var")
    dunitlut.dom$var <- dunitlut.dom[,..nm.var][[1]] / dunitlut.dom[,n.total][[1]] 
    
    est.area <- sae::mseFH(
      formula = fmla.dom,
      vardir = var,
      method = "FH",
      data = as.data.frame(dunitlut.dom)
    )
    
    est <- data.frame(
      domain = dunitlut.dom$DOMAIN,
      estimate = est.area$est$eblup[,1],
      standard_error = sqrt(est.area$mse)
    )
    
    return(est)
  }
  
  if (SApackage == "hbsae") {
    nm.var <- paste0(yn, ".var")
    dunitlut.dom$var <- dunitlut.dom[,..nm.var][[1]] / dunitlut.dom[,n.total][[1]] 
    
    y <- as.data.frame(dunitlut.dom)[[yn]]
    names(y) <- as.data.frame(dunitlut.dom)[["DOMAIN"]]
    
    X <- model.matrix(fmla.dom[-2], dunitlut.dom)
    rownames(X) <- dunitlut.dom$DOMAIN
    
    if (is.null(prior)) {
      est.area <- hbsae::fSAE.Area(
        est.init = y,
        var.init = as.data.frame(dunitlut.dom)[["var"]],
        X = X
      )
    } else {
      est.area <- hbsae::fSAE.Area(
        est.init = y,
        var.init = as.data.frame(dunitlut.dom)[["var"]],
        X = X,
        prior = prior
      )
    }
    
    est <- data.frame(
      domain = est.area$predAreaNames,
      estimate = est.area$est,
      standard_error = sqrt(est.area$mse)
    )
    
    return(est)
  }
}


SAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, pltassgn,
	dunitlut, prednames=NULL, dunitvar="DOMAIN", 
	SAmethod="unit", SApackage="JoSAE", yd=NULL, ratiotype="PERACRE",
	largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL, prior = NULL) {

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
  variable.select <- TRUE
  if (variable.select) {
    if (!"mase" %in% rownames(installed.packages()))
    stop("variable selection requires package mase")
  }

  ## Merge dat.dom to pltassgn
  pltdat.dom <- dat.dom[pltassgn]
  pltdat.dom[is.na(pltdat.dom[[yn]]), (yn) := 0]

  ## Add mean response to dunitlut for Area-level estimates
  datmean <- pltdat.dom[, list(mean=mean(get(yn), na.rm=TRUE),
			mean.var=var(get(yn), na.rm=TRUE)), by="DOMAIN"]
  setkey(datmean, "DOMAIN")
  dunitlut.dom <- merge(dunitlut, datmean, by=dunitvar)
  setnames(dunitlut.dom, c("mean", "mean.var"), c(yn, paste0(yn, ".var")))

print("TEST")
###################
##### TESTING #####
###################
  if (!"data.table" %in% class(pltdat.dom)) {
    pltdat.dom <- setDT(pltdat.dom)
  } 
  if (!"data.table" %in% class(dunitlut.dom)) {
    dunitlut.dom <- setDT(dunitlut.dom)
  } 

  ## Calculate number of non-zero plots
  NBRPLT.gt0 <- pltdat.dom[, sum(get(yn) > 0), by=dunitvar]
  setnames(NBRPLT.gt0, "V1", "NBRPLT.gt0")
  setkeyv(NBRPLT.gt0, dunitvar)

  ## Check if all plots are zero
  if (sum(pltdat.dom[[yn]]) == 0) {
    message(yn, " has all 0 values... returning NULL")
    if (SAmethod == "unit") {
      est <- data.table(dunitlut.dom[[dunitvar]],
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(est, "V1", dunitvar)
    } else {
      est <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, JA.synth=NA, 
		JA.synth.se=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(est, "V1", dunitvar)
    }
    ## Merge NBRPLT.gt0
    est <- merge(est, NBRPLT.gt0, by="DOMAIN")

    ## Merge AOI
    if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut.dom)) {
      est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
    return(list(est=est, prednames.select=NULL, pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
  }  

  if (standardize) {
    ## Standardize predictors at the scale at which borrowing occurs (e.g., PROVINCE/SECTION)
    ## So, not standardized at national level
    #pltdat.dom[, (prednames) := lapply(.SD, function(x) x / max(abs(x))), .SDcols=prednames]
    #dunitlut.dom[, (prednames) := lapply(.SD, function(x) x / max(abs(x))), .SDcols=prednames]
    pltdat.dom[, (prednames) := lapply(.SD, function(x) (x-min(x)) / (max(x) - min(x))), .SDcols=prednames]
    dunitlut.dom[, (prednames) := lapply(.SD, function(x) (x-min(x)) / (max(x) - min(x))), .SDcols=prednames]
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
 
  ## Variable selection using area-level Elastic net
  ###################################################################
  prednames.select <- prednames
  if (variable.select) {
    pltdat.dom <- setDF(pltdat.dom)
    dunitlut.dom <- setDF(dunitlut.dom)
    #cor(std.plt.dom[, prednames])

    ## select predictor variables from Elastic Net procedure
    ## alpha=1, indicates 
    mod1 <- suppressMessages(mase::gregElasticNet(y=pltdat.dom[[yn]], 
		xsample=pltdat.dom[,prednames], 
		xpop=dunitlut.dom[,prednames], pi = NULL, alpha = 0.5,
  		model = "linear", pi2 = NULL, var_est = FALSE,
  		var_method = "LinHB", datatype = "raw", N = NULL,
  		lambda = "lambda.1se", B = 1000, cvfolds = 10))
    mod1$coefficients[-1]
    mod1.rank <- rank(-abs(mod1$coefficients[-1]))
    preds.enet <- names(mod1$coefficients[-1])[abs(mod1$coefficients[-1])>0]

    if (length(preds.enet) == 0) {
      ## select predictor variables from Elastic Net procedure
      ## alpha=1, indicates 
      mod1 <- suppressMessages(mase::gregElasticNet(y=pltdat.dom[[yn]], 
		xsample=pltdat.dom[,prednames], 
		xpop=dunitlut.dom[,prednames], pi = NULL, alpha = .2,
  		model = "linear", pi2 = NULL, var_est = FALSE,
  		var_method = "LinHB", datatype = "raw", N = NULL,
  		lambda = "lambda.1se", B = 1000, cvfolds = 10))
      mod1$coefficients[-1]
      mod1.rank <- rank(-abs(mod1$coefficients[-1]))
      preds.enet <- names(mod1$coefficients[-1])[abs(mod1$coefficients[-1])>0]
 
      if (length(preds.enet) == 0) {
        message("no predictors were selected for model")
        if (SAmethod == "unit") {
          est <- data.table(dunitlut.dom[[dunitvar]],
			DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
			JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut.dom$n.total)
          setnames(est, "V1", dunitvar)
        } else {
          est <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
			DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, JA.synth=NA, 
			JA.synth.se=NA, NBRPLT=dunitlut.dom$n.total)
          setnames(est, "V1", dunitvar)
        }
        ## Merge NBRPLT.gt0
        est <- merge(est, NBRPLT.gt0, by="DOMAIN")
        ## Merge AOI
        if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut.dom)) {
          est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
        }
        return(list(est=est, prednames.select=preds.enet, 
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
      } else {
        prednames.select <- preds.enet
      }
    } else {
      prednames.select <- preds.enet
    }
  }
 
  if (showsteps || savesteps) {
    ylab <- ifelse(yn == "CONDPROP_ADJ", "FOREST_prob", 
			ifelse(yn == "TPA_UNADJ", "COUNT", yn))
    pheight <- 6
    pwidth <- 6
    rnbr=cbnr <- 1
    if (length(prednames) > 1) {
      nbrpreds <- length(prednames)
      if (nbrpreds == 2) {
        rnbr <- 1
        cnbr <- 2
        pwidth <- 8
      } else if (nbrpreds == 3) {
        rnbr <- 1
        cnbr <- 3
        pwidth <- 10
      } else if (nbrpreds == 4) {
        rnbr <- 2
        cnbr <- 2
        pwidth <- 8
        pheight <- 8
      } else if (nbrpreds %in% 5:6) {
        rnbr <- 2
        cnbr <- 3
        pwidth <- 10
        pheight <- 8
      } else if (nbrpreds %in% 7:8) {
        rnbr <- 2
        cnbr <- 4
        pwidth <- 12
        pheight <- 8
      } else if (nbrpreds %in% 9) {
        rnbr <- 3
        cnbr <- 3
        pwidth <- 10
        pheight <- 8
      } else if (nbrpreds > 9) {
        rnbr <- 4
        cnbr <- 4
        pwidth <- 12
        pheight <- 10
      }        
    }
    if (showsteps) {
      dev.new()
      par(mfrow=c(rnbr,cnbr)) 
      for (pred in prednames) {
        main2 <- ifelse(pred %in% prednames.select, "selected", "not selected")
        if (!is.null(largebnd.val) && largebnd.val != 1) { 
          #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
          main <- paste0(largebnd.val, " - ", main2)
        } else {
          main <- main2
        }
        plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
      }
    }
    if (savesteps) {
      out_layer <- paste0("SApred_", ylab)
      jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
      jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)
    
      par(mfrow=c(rnbr,cnbr)) 
      for (pred in prednames) {
        main2 <- ifelse(pred %in% prednames.select, "selected", "not selected")
        if (!is.null(largebnd.val) && largebnd.val != 1) { 
          #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
          main <- paste0(largebnd.val, " - ", main2)
        } else {
          main <- main2
        }
        plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
      }
      dev.off()
    }
  }

  ## create model formula with predictors
  ## note: the variables selected can change depending on the order in original formula (fmla)
  fmla.dom <- stats::as.formula(paste(yn, paste(prednames.select, collapse= "+"), sep="~"))

  if (SAmethod == "unit") {
    est <- tryCatch(SAest.unit(fmla.dom, pltdat.dom, dunitlut.dom, yn, 
				SApackage = SApackage, prior = prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )

    if (is.null(est)) {
      est <- data.table(dunitlut.dom[[dunitvar]],
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(est, "V1", dunitvar)
    }
  }
 
  if (SAmethod == "area") {
#prednames=prednames.select
print("XXXXX")
    est <- tryCatch(SAest.area(fmla.dom, pltdat.dom, dunitlut.dom, 
				cuniqueid, dunitvar, prednames=prednames.select, yn, 
				SApackage = SApackage, prior = prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(est)) {
      est <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, JA.synth=NA, 
		JA.synth.se=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(est, "V1", dunitvar)
    }
  }

  ## Merge NBRPLT.gt0
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")

  ## Merge AOI
  if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut.dom)) {
    est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
  }
  return(list(est=est, prednames.select=prednames.select, 
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
}


########################################################################
## By domain
########################################################################
SAest.dom <- function(dom, dat, cuniqueid, dunitlut, pltassgn, dunitvar="DOMAIN", 
		SApackage, SAmethod, prednames=NULL, domain, response=NULL,
		largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL) {

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
  domest <- SAest(yn=response, 
			dat.dom=dat.dom, cuniqueid=cuniqueid, pltassgn=pltassgn,
			dunitlut=dunitlut, prednames=prednames, 
			SApackage=SApackage, SAmethod=SAmethod, largebnd.val=largebnd.val,
			showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder)
  domest$est <- data.table(dom, domest$est)
  setnames(domest$est, "dom", domain)
  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, dat, cuniqueid, largebnd.att, 
		dunitlut, dunitvar="DOMAIN", SApackage="JoSAE", 
		SAmethod="unit", domain, response, prednames=NULL,
		showsteps=FALSE, savesteps=FALSE, stepfolder=NULL) {

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
  doms <- sort(as.character(na.omit(unique(dat.large[[domain]]))))

#dat=dat.large
#dunitlut=dunitlut.large
#pltassgn=pltassgn.large
#dom=doms[1]

  estlst <- lapply(doms, SAest.dom, 
			dat=dat.large, cuniqueid=cuniqueid, pltassgn=pltassgn.large,
     			dunitlut=dunitlut.large, dunitvar=dunitvar,
			SApackage=SApackage, SAmethod=SAmethod, prednames=prednames, 
			domain=domain, response=response, largebnd.val=largebnd.val,
			showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder)
  if (length(doms) > 1) {
    est.large <- do.call(rbind, do.call( rbind, estlst)[,1])
    prednames.select <- do.call(rbind, estlst)[,2]
    names(prednames.select) <- doms
    pltdat.dom <- do.call(rbind, do.call(rbind, estlst)[,3])
    dunitlut.dom <- do.call(rbind, do.call(rbind, estlst)[,4])
  } else {
    est.large <- do.call(rbind, estlst)[,1]$est
    prednames.select <- do.call(rbind, estlst)[,2]$prednames.select
    pltdat.dom <- do.call(rbind, estlst)[,3]$pltdat.dom
    dunitlut.dom <- do.call(rbind, estlst)[,4]$dunitlut.dom
  }
  setkeyv(est.large, dunitvar)
  setkeyv(setDT(pltdat.dom), dunitvar)
  setkeyv(setDT(dunitlut.dom), dunitvar)

  return(list(est.large=est.large, prednames.select=prednames.select, 
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
}       

