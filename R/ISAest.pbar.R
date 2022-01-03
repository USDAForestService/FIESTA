SAest.unit <- function(fmla.dom.unit, pltdat.dom, dunitlut.dom, yn, SApackage, 
	dunitvar, predselect.unit, prior = NULL) {

  ## Set global variables
  DOMAIN <- NULL
  
  pltdat.unit <- data.frame(pltdat.dom)
  dunitlut.unit <- data.frame(dunitlut.dom)
  

  if (SApackage == "JoSAE") {

    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom.unit), data=pltdat.unit, random=~1|DOMAIN)))
    
    ## calculate the variance of the EBLUP estimate
    est.unit <- tryCatch(JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.unit, 
                                        lme.obj = dom.lme, debug=FALSE),
                         error=function(err) {
                           #message(err, "\n")
                           return(NULL)
                         } )
    if (is.null(est.unit)) {
      return(NULL)
    }
  
    ## subset dataframe before returning
    est <- est.unit[,c("DOMAIN.domain", "n.i.sample",
                       yn, "sample.se", "Synth", 
                       "GREG", "GREG.se",
                       "EBLUP","EBLUP.se.1")]
    names(est) <- c("DOMAIN", "NBRPLT", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
                    "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1")

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est)
  }
  
  if (SApackage == "sae") {
    xpop <- dunitlut.unit[,c('DOMAIN', predselect.unit)]
    popsize <- dunitlut.unit[, c("DOMAIN", "npixels")]
    
    est.unit <- tryCatch(suppressMessages(sae::pbmseBHF(formula = fmla.dom.unit,
                              dom = DOMAIN,
                              selectdom = unique(xpop$DOMAIN),
                              meanxpop = xpop,
                              popnsize = popsize,
                            method = "REML",
                              data = pltdat.unit,
                              B = 100)),
                         error=function(err) {
                           #message(err, "\n")
                           return(NULL)
                         } )
    if (is.null(est.unit)) {
      return(NULL)
    }
    
    est <- data.frame(
      DOMAIN = est.unit$est$eblup$domain,
      saeU = est.unit$est$eblup$eblup,
      saeU.se = sqrt(est.unit$mse[["mse"]])
    )

    est <- merge(est, dunitlut.unit[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.total"] <- "NBRPLT"
    est <- est[, c("NBRPLT", "saeU", "saeU.se")]

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est)
  }
  
  if (SApackage == "hbsae") {
    prior = function(x) 1 / (sqrt(x) * (1 + x))
    xpophb <- model.matrix(fmla.dom.unit[-2], dunitlut.unit)
    rownames(xpophb) <- dunitlut.unit[[dunitvar]]
    
    if (is.null(prior)) {
      est.unit <- tryCatch(hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        Xpop = xpophb, 
        silent = FALSE 
      ),
      error=function(err) {
        #message(err, "\n")
        return(NULL)
      } )
    } else {
      est.unit <- tryCatch(hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        fpc = FALSE,
        Xpop = xpophb,
        prior = prior, 
        silent = FALSE
      ),
      error=function(err) {
        #message(err, "\n")
        return(NULL)
      } )
    }
    if (is.null(est.unit)) {
      return(NULL)
    }
    
    est <- data.frame(
      DOMAIN = est.unit$sampledAreaNames,
      hbsaeU = est.unit$est,
      hbsaeU.se = sqrt(est.unit$mse)
    )
    est <- merge(est, dunitlut.unit[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.total"] <- "NBRPLT"
    est <- est[, c(dunitvar, "NBRPLT", "hbsaeU", "hbsaeU.se")]

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()
    
    return(est)
  }

}

SAest.area <- function(fmla.dom.area, pltdat.dom, dunitlut.dom, cuniqueid, 
	dunitvar="DOMAIN", predselect.area, yn, SApackage, prior=NULL) {
  
  ## Remove response values equal to 1
  #  dunitlut.dom.NA <- dunitlut.dom[is.na(dunitlut.dom$mean.var) | dunitlut.dom$mean.var == 0 |
  #		dunitlut.dom$n.total < 2, ]

  dunitlut.dom <- data.frame(dunitlut.dom)
  nm.var <- paste0(yn, ".var")
  dunitlut.NA <- dunitlut.dom[is.na(dunitlut.dom[[nm.var]]) | dunitlut.dom[[nm.var]] < 0.001, ]
  #dunitlut.NA <- dunitlut.dom[is.na(dunitlut.dom[[nm.var]]) | dunitlut.dom[[nm.var]] == 0, ]
  dunitNAids <- dunitlut.NA[[dunitvar]]
  dunitids <-  dunitlut.dom[!dunitlut.dom[[dunitvar]] %in% dunitNAids, dunitvar]
  dunitlut.area <- dunitlut.dom[dunitlut.dom[[dunitvar]] %in% dunitids, ]
  pltdat.area <- data.frame(pltdat.dom[pltdat.dom[[dunitvar]] %in% dunitids, ])

  if (SApackage == "JoSAE") {
    xpop.dom <- paste0(predselect.area, ".X.pop")
    fmla.dom.area2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
                                  paste(xpop.dom, collapse= "+"), sep="~"))
    res <-
      tryCatch(JoSAE::sae.ul.f(samp.data = pltdat.area,
                      population.data = dunitlut.area,
                      k.ij = rep(1,nrow(pltdat.area)),
                      formula = fmla.dom.area,
                      domain.col = dunitvar,
                      sample.id.col = cuniqueid,
                      neg.sfrac = TRUE),
               error=function(err) {
                 #message(err, "\n")
                 return(NULL)
               } )
    
    if (is.null(res)) {
      return(NULL)
    }
    
    
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
      formula=fmla.dom.area2, data=dat.al,
      b.i=rep(1, nrow(dat.al)),
      type="RE"))
    est <- est.area$results[,1:7]
    names(est) <- c("DOMAIN", "DIR", "DIR.se", "JFH", "JFH.se",
                    	"JA.synth", "JA.synth.se")
    ## To add space to messages
    cat("\n")
    
    est <- merge(est, dat.al[, c("domain.id", "n.i")], 
                 by.x="DOMAIN", by.y="domain.id") 
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.i"] <- "NBRPLT"
    est <- est[, c(dunitvar, "NBRPLT", "DIR", "DIR.se", "JFH", "JFH.se",
				"JA.synth", "JA.synth.se")]
    
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], NBRPLT=dunitlut.NA$n.total,
				DIR=NA, DIR.se=NA, 
				JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA)
      setnames(est.NA, "V1", dunitvar)
      est <- rbindlist(list(est, est.NA))
      setorderv(est, dunitvar)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()

    return(est)
  }
  
  if (SApackage == "sae") {

    nm.var <- paste0(yn, ".var")
    dunitlut.area$var <- dunitlut.area[[nm.var]] / (dunitlut.area$n.total)

    est.area <- tryCatch(sae::mseFH(
      formula = fmla.dom.area,
      vardir = var,
      #method = "FH",
      method = "REML",
      data = dunitlut.area,
      MAXITER=250
      ),
      error=function(err) {
        message(err, "\n")
        return(NULL)
      } )

    if (is.null(est.area)) {
      return(NULL)
    }
    
    est <- data.frame(
      DOMAIN = dunitlut.area[[dunitvar]],
      saeA = est.area$est$eblup[,1],
      saeA.se = sqrt(est.area$mse)
    )

    est <- merge(est, dunitlut.area[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.total"] <- "NBRPLT"
    est <- est[, c(dunitvar, "NBRPLT", "saeA", "saeA.se")]

       
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], NBRPLT=dunitlut.NA$n.total,
			saeA=NA, saeA.se=NA)
      setnames(est.NA, "V1", dunitvar)
      est <- rbindlist(list(est, est.NA))
      setorderv(est, dunitvar)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()
 
    return(est)
  }
  
  if (SApackage == "hbsae") {

    prior = function(x) 1 / (sqrt(x) * (1 + x))
    nm.var <- paste0(yn, ".var")
    dunitlut.area$var <- dunitlut.area[[nm.var]] / dunitlut.area$n.total
    
    y <- dunitlut.area[[yn]]
    names(y) <- dunitlut.area[[dunitvar]]
    
    X <- model.matrix(fmla.dom.area[-2], dunitlut.area)
    rownames(X) <- dunitlut.area[[dunitvar]]
    
    if (is.null(prior)) {
      est.area <- tryCatch(hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        silent=TRUE
      ),
      error=function(err) {
        #message(err, "\n")
        return(NULL)
      } )
      
    } else {
      est.area <- tryCatch(hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        prior = prior,
        silent=TRUE
      ),
      error=function(err) {
        #message(err, "\n")
        return(NULL)
      } )
    }
   
    if (is.null(est.area)) {
      return(NULL)
    }
    est <- data.frame(
      DOMAIN = est.area$predAreaNames,
      hbsaeA = est.area$est,
      hbsaeA.se = sqrt(est.area$mse)
    )

    est <- merge(est, dunitlut.area[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar)
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.total"] <- "NBRPLT"
    est <- est[, c(dunitvar, "NBRPLT", "hbsaeA", "hbsaeA.se")]
      
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], NBRPLT=dunitlut.NA$n.total,
		hbsaeA=NA, hbsaeA.se=NA)
      setnames(est.NA, "V1", dunitvar)
      est <- rbindlist(list(est, est.NA))
      setorderv(est, dunitvar)
    }

    rm(dunitlut.area)
    rm(fmla.dom.area)
    rm(pltdat.area)
    gc()
    
    return(est)
  }
}


SAest <- function(yn="CONDPROP_ADJ", dat.dom, cuniqueid, pltassgn,
	dunitlut, prednames=NULL, dunitvar="DOMAIN", 
	SAmethod="unit", SApackage="JoSAE", yd=NULL, ratiotype="PERACRE",
	largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL, 
	prior=NULL, variable.select=TRUE) {

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
  getDIR <- FALSE
  
  if (variable.select) {
    if (!"mase" %in% rownames(installed.packages()))
    stop("variable selection requires package mase")
  }

  ## Merge dat.dom to pltassgn
  pltdat.dom <- dat.dom[pltassgn]
  pltdat.dom[is.na(pltdat.dom[[yn]]), (yn) := 0]

  ## Add mean response to dunitlut for Area-level estimates
  datmean <- pltdat.dom[, list(mean=mean(get(yn), na.rm=TRUE),
			mean.var=var(get(yn), na.rm=TRUE)), by=dunitvar]
  setkeyv(datmean, dunitvar)
  dunitlut.dom <- merge(dunitlut, datmean, by=dunitvar)
  setnames(dunitlut.dom, c("mean", "mean.var"), c(yn, paste0(yn, ".var")))


###################
##### TESTING #####
###################
#pltdom <- fread("E:/workspace/FIESTA/FIESTA_SA/ecosubsection_test/pltdom.csv")
#dunitlut <- fread("E:/workspace/FIESTA/FIESTA_SA/ecosubsection_test/dunitlut.csv")
#pltdat.dom <- pltdom[pltdom$PROVINCE == 251, ]
#dunitlut.dom <- dunitlut[dunitlut$PROVINCE == 251, ]
#dunitvar <- "DOMAIN"
#yn <- "DRYBIO_AG_TPA_ADJ"
#prednames <- c("tcc", "elev", "ppt", "tmean", "tmin01", "tnt2")
#standardize <- TRUE
#variable.select <- TRUE
#SApackage <- "JoSAE"
#SAmethod <- "area"
#cuniqueid <- "PLT_CN"

  if (!"data.table" %in% class(pltdat.dom)) {
    pltdat.dom <- setDT(pltdat.dom)
  } 
  if (!"data.table" %in% class(dunitlut.dom)) {
    dunitlut.dom <- setDT(dunitlut.dom)
  } 

  ## define empty data tables for storing selected predictors
  predselect.unitdt <- dunitlut[FALSE, prednames, with=FALSE, drop=FALSE]
  predselect.areadt <- dunitlut[FALSE, prednames, with=FALSE, drop=FALSE]

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
    return(list(est=est, predselect.unit=NULL, predselect.area=NULL, 
		pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
  }  
   
  ## Variable selection for area and unit-level estimators
  ###################################################################
  if (variable.select) {
    predselect.unitlst <- suppressMessages(preds.select(y=yn, 
                            plt=pltdat.dom, aux=dunitlut.dom, prednames=prednames))
    predselect.unit <- predselect.unitlst$preds.enet
    predselect.unit.coef <- predselect.unitlst$preds.coef

    predselect.arealst <- suppressMessages(preds.select(y=yn, 
                            plt=dunitlut.dom, aux=dunitlut.dom, prednames=prednames))
    predselect.area <- predselect.arealst$preds.enet
    predselect.area.coef <- predselect.arealst$preds.coef
    
  } else {
    predselect.area <- prednames
    predselect.unit <- prednames
  } 
 
  if (length(predselect.area) > 0 || length(predselect.unit) > 0) {
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
        if (length(predselect.unit) > 0) {
          ## unit-level selection
          dev.new()
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.unit, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) { 
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
        }

        if (length(predselect.area) > 0) {
          ## area-level selection
          dev.new()
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.area, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) { 
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
        }
      }

      if (savesteps) {
        if (length(predselect.unit) > 0) {
          ## unit-level selection
          out_layer <- paste0("SApred_unit_", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)
    
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.unit, "selected", "not selected")
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

        if (length(predselect.area) > 0) {
     
          ## area-level selection
          out_layer <- paste0("SApred_area", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)
    
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% predselect.area, "selected", "not selected")
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
    }
  }

  if (length(predselect.unit) > 0) {
    message("using following predictors for unit-level models...", toString(predselect.unit))

    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.unit <- stats::as.formula(paste(yn, paste(predselect.unit, collapse= "+"), sep="~"))

    ###  Unit-level estimates
    ############################################
    ## NOTE: changed prednames=prednames.select to prednames
    unit.JoSAE <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, yn=yn, SApackage="JoSAE", 
				dunitvar=dunitvar, predselect.unit=predselect.unit, prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(unit.JoSAE)) {
      unit.JoSAE <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
		NBRPLT=dunitlut.dom$n.total,
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA)
      setnames(unit.JoSAE, "DOMAIN", dunitvar)
    }

    unit.hbsae <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, yn=yn, SApackage="hbsae", 
				dunitvar=dunitvar, predselect.unit=predselect.unit, prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(unit.hbsae)) {
      unit.hbsae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
		NBRPLT=dunitlut.dom$n.total,
		hbsaeU=NA, hbsaeU.se=NA) 
      setnames(unit.hbsae, "DOMAIN", dunitvar)
    }

    ## Merge estimates
    est <- merge(unit.JoSAE, unit.hbsae[, c(dunitvar, "hbsaeU", "hbsaeU.se")], by=dunitvar)

    rm(unit.JoSAE)
    rm(unit.hbsae)

  } else {
    getDIR <- TRUE
    
    message("no predictors were selected for unit-level models... returning NAs")
    est <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]],
			DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
			JU.EBLUP=NA, JU.EBLUP.se.1=NA, 
			hbsaeU=NA, hbsaeU.se=NA, NBRPLT=dunitlut.dom$n.total)
    setnames(est, "DOMAIN", dunitvar)
  }

  if (length(predselect.area) > 0) {
    message("using following predictors for area-level models...", toString(predselect.unit))

    if (getDIR) {
      nm.var <- paste0(yn, ".var")
      dunitlut.dom$DIR <- dunitlut.dom[[yn]]
      dunitlut.dom$DIR.se <- sqrt(dunitlut.dom[[nm.var]] / dunitlut.dom$n.total)
      est[match(est$DOMAIN, dunitlut.dom$DOMAIN), c("DIR", "DIR.se")] <- dunitlut.dom[,c("DIR", "DIR.se")]
      #print(dunitlut.dom)
    }
      
    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.area <- stats::as.formula(paste(yn, paste(predselect.area, collapse= "+"), sep="~"))
 
    ###  Area-level estimates
    ############################################
    area.JoSAE <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, 
                                      pltdat.dom=pltdat.dom, 
                                      dunitlut.dom=dunitlut.dom, 
                                      cuniqueid=cuniqueid, 
                                      dunitvar=dunitvar, 
                                      predselect.area=predselect.area, 
                                      yn=yn, SApackage="JoSAE"), 
                           error=function(err) {
                             message(err, "\n") 
                             return(NULL)
                             } )
    if (is.null(area.JoSAE)) {
      area.JoSAE <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
                               NBRPLT=dunitlut.dom$n.total, 
                               DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, 
                               JA.synth=NA, JA.synth.se=NA)
      setnames(area.JoSAE, "DOMAIN", dunitvar)
    }

        ## Merge estimates
    est <- merge(est, area.JoSAE[, c("DOMAIN", "JFH", "JFH.se", "JA.synth", "JA.synth.se")], 
		by=dunitvar)

    area.sae <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, 
                                    pltdat.dom=pltdat.dom, 
                                    dunitlut.dom=dunitlut.dom, 
                                    cuniqueid=cuniqueid, 
                                    dunitvar=dunitvar, 
                                    predselect.area=predselect.area, 
                                    yn=yn, SApackage="sae"),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(area.sae)) {
      area.sae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
                             NBRPLT=dunitlut.dom$n.total, saeA=NA, saeA.se=NA) 
      setnames(area.sae, "DOMAIN", dunitvar)
    }
    est <- merge(est, area.sae[, c("DOMAIN", "saeA", "saeA.se")], by="DOMAIN")

    area.hbsae <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, cuniqueid=cuniqueid, 
				dunitvar=dunitvar, predselect.area=predselect.area, 
				yn=yn, SApackage="hbsae", prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(area.hbsae)) {
      area.hbsae <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], 
		    NBRPLT=dunitlut.dom$n.total,
		    hbsaeA=NA, hbsaeA.se=NA) 
      setnames(area.hbsae, "DOMAIN", dunitvar)
    }
    est <- merge(est, area.hbsae[, c("DOMAIN", "hbsaeA", "hbsaeA.se")], by="DOMAIN")

    rm(area.JoSAE)
    rm(area.sae)
    rm(area.hbsae)

  } else {
    message("no predictors were selected for area-level models... returning NAs")
    est.NA <- data.frame(DOMAIN=dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
			JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA, 
			saeA=NA, saeA.se=NA, 
			hbsaeA=NA, hbsaeA.se=NA)
    setnames(est.NA, "DOMAIN", dunitvar)

    est <- merge(est, est.NA, by="DOMAIN")
  }

  ## Merge NBRPLT.gt0
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")

  ## Merge AOI
  if ("AOI" %in% names(dunitlut.dom)) {
    if (!"AOI" %in% names(est)) {
      est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
    if (!"AOI" %in% names(pltdat.dom)) {
      pltdat.dom <- merge(pltdat.dom, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
    }
  }
  
  gc()

  if (variable.select) {
    predselect.areadt <- rbindlist(list(predselect.areadt, 
		data.frame(t(predselect.area.coef))), fill=TRUE)
    predselect.unitdt <- rbindlist(list(predselect.unitdt, 
		data.frame(t(predselect.unit.coef))), fill=TRUE)
  } else {
    preds.area <- data.frame(t(ifelse(names(predselect.areadt) %in% predselect.area, 1, 0)))
    setnames(preds.area, names(predselect.areadt))
    predselect.areadt <- rbindlist(list(predselect.areadt, preds.area), fill=TRUE)

    preds.unit <- data.frame(t(ifelse(names(predselect.unitdt) %in% predselect.unit, 1, 0)))
    setnames(preds.unit, names(predselect.unitdt))
    predselect.unitdt <- rbindlist(list(predselect.unitdt, preds.unit), fill=TRUE)
  }

  return(list(est=est, predselect.unit=predselect.unitdt, 
		predselect.area=predselect.areadt,
		pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
}


########################################################################
## By domain
########################################################################
SAest.dom <- function(dom, dat, cuniqueid, dunitlut, pltassgn, dunitvar="DOMAIN", 
		SApackage, SAmethod, prednames=NULL, domain, response=NULL,
		largebnd.val=NULL, showsteps=FALSE, savesteps=FALSE, stepfolder=NULL,
		prior=NULL, variable.select=TRUE) {

  ## Subset tomdat to domain=dom
  dat.dom <- dat[dat[[domain]] == dom,] 
   

  if (nrow(dat.dom) == 0 || sum(!is.na(dat.dom[[domain]])) == 0) {
    domest <- data.table(dom, matrix(c(0, rep(NA,17)), 1, 18), 0, 1) 
    setnames(domest, c(domain, "NBRPLT", "DIR", "DIR.se", 
		"JU.Synth", "JU.GREG", "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", 
		"hbsaeU", "hbsaeU.se", "JFH", "JFH.se",
		"JA.synth", "JA.synth.se", "saeA", "saeA.se", 
		"hbsaeA", "hbsaeA.se", "NBRPLT.gt0", "AOI"))
    return(list(domest, predselect.unit=NULL, predselect.area=NULL, dunitlut.dom=NULL))
  }

#yn=response

  ## Apply function to each dom
  domest <- SAest(yn=response, 
			dat.dom=dat.dom, cuniqueid=cuniqueid, pltassgn=pltassgn,
			dunitlut=dunitlut, dunitvar=dunitvar, prednames=prednames, 
			SApackage=SApackage, SAmethod=SAmethod, largebnd.val=largebnd.val,
			showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder,
			prior=prior, variable.select=variable.select)
  
  domest$est <- data.table(dom, domest$est)
  setnames(domest$est, "dom", domain)
  domest$predselect.unit <- data.table(dom, domest$predselect.unit)
  setnames(domest$predselect.unit, "dom", domain)
  domest$predselect.area <- data.table(dom, domest$predselect.area)
  setnames(domest$predselect.area, "dom", domain)

  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, dat, cuniqueid, largebnd.unique, 
		dunitlut, dunitvar="DOMAIN", SApackage="JoSAE", 
		SAmethod="unit", domain, response, prednames=NULL,
		showsteps=FALSE, savesteps=FALSE, stepfolder=NULL, 
		prior=NULL, variable.select=TRUE) {

  ## subset datasets by largebnd value (e.g., ecosection)
  dat.large <- dat[dat[[largebnd.unique]] == largebnd.val, 
		c(dunitvar, cuniqueid, domain, response), with=FALSE]
  if (nrow(dat.large) == 0) stop("invalid largebnd.val")
  setkeyv(dat.large, c(dunitvar, cuniqueid))

  pltassgn.large <- unique(dat[dat[[largebnd.unique]] == largebnd.val, 
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
#dom=doms[i]

  estlst <- lapply(doms, SAest.dom, 
			        dat=dat.large, cuniqueid=cuniqueid, pltassgn=pltassgn.large,
     			    dunitlut=dunitlut.large, dunitvar=dunitvar,
			        SApackage=SApackage, SAmethod=SAmethod, prednames=prednames, 
			        domain=domain, response=response, largebnd.val=largebnd.val,
			        showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder,
			        prior=prior, variable.select=variable.select)
 
  if (length(doms) > 1) {
    est.large <- data.table(largebnd=largebnd.val, 
				do.call(rbind, do.call(rbind, estlst)[,"est"]))
    setnames(est.large, "largebnd", largebnd.unique)

    predselect.unit <- data.table(largebnd=largebnd.val, 
				do.call(rbind, do.call(rbind, estlst)[,"predselect.unit"]))
    setnames(predselect.unit, "largebnd", largebnd.unique)

    predselect.area <- data.table(largebnd=largebnd.val, 
				do.call(rbind, do.call(rbind, estlst)[,"predselect.area"]))
    setnames(predselect.area, "largebnd", largebnd.unique)

    pltdat.dom <- data.table(largebnd.val, do.call(rbind, 
				do.call(rbind, estlst)[,"pltdat.dom"]))
    dunitlut.dom <- data.table(largebnd.val, do.call(rbind, 
				do.call(rbind, estlst)[,"dunitlut.dom"]))
  } else {
    
    #print(head(do.call(rbind, estlst)[,"est"]$est))
    est.large <- data.table(largebnd=largebnd.val, 
				do.call(rbind, estlst)[,"est"]$est)
    setnames(est.large, "largebnd", largebnd.unique)

    predselect.unit <- data.table(largebnd=largebnd.val, 
				do.call(rbind, estlst)[,"predselect.unit"]$predselect.unit)
    setnames(predselect.unit, "largebnd", largebnd.unique)

    predselect.area <- data.table(largebnd=largebnd.val, 
				do.call(rbind, estlst)[,"predselect.area"]$predselect.area)
    setnames(predselect.area, "largebnd", largebnd.unique)

    pltdat.dom <- data.table(largebnd=largebnd.val, 
				do.call(rbind, estlst)[,"pltdat.dom"]$pltdat.dom)
    setnames(pltdat.dom, "largebnd", largebnd.unique)

    dunitlut.dom <- data.table(largebnd=largebnd.val, 
				do.call(rbind, estlst)[,"dunitlut.dom"]$dunitlut.dom)
    setnames(dunitlut.dom, "largebnd", largebnd.unique)
  }
  
  setkeyv(est.large, dunitvar)
  setkeyv(setDT(pltdat.dom), dunitvar)
  setkeyv(setDT(dunitlut.dom), dunitvar)
  
  rm(estlst)
  gc()
 
  return(list(est.large=est.large, 
			predselect.unit=predselect.unit, 
			predselect.area=predselect.area, 
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
}       

