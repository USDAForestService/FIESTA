SAest.unit <- function(fmla.dom.unit, pltdat.dom, dunitlut.dom, yn, SApackage, 
	dunitvar, prednames.unit, prior = NULL) {

  ## Set global variables
  DOMAIN <- NULL

  pltdat.unit <- data.frame(pltdat.dom)
  dunitlut.unit <- data.frame(dunitlut.dom)

  if (SApackage == "JoSAE") {

    ## create linear mixed model
    ## note: see http://www.win-vector.com/blog/2018/09/r-tip-how-to-pass-a-formula-to-lm/
    dom.lme <- eval(bquote( nlme::lme(.(fmla.dom.unit), data=pltdat.unit, random=~1|DOMAIN)))
    
    ## calculate the variance of the EBLUP estimate
    est.unit <- JoSAE::eblup.mse.f.wrap(domain.data = dunitlut.unit, 
                                        lme.obj = dom.lme, debug=FALSE)
    
    ## subset dataframe before returning
    est <- est.unit[,c("DOMAIN.domain", 
                       yn, "sample.se", "Synth", 
                       "GREG", "GREG.se",
                       "EBLUP","EBLUP.se.1", "n.i.sample")]
    names(est) <- c("DOMAIN", "DIR", "DIR.se", "JU.Synth", "JU.GREG",
                    "JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT")

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()

    return(est)
  }
  
  if (SApackage == "sae") {
    xpop <- dunitlut.unit[,c('DOMAIN', prednames.unit)]
    popsize <- dunitlut.unit[, c("DOMAIN", "npixels")]
    
    est.unit <- suppressMessages(sae::pbmseBHF(formula = fmla.dom.unit,
                              dom = DOMAIN,
                              selectdom = unique(xpop$DOMAIN),
                              meanxpop = xpop,
                              popnsize = popsize,
                            method = "REML",
                              data = pltdat.unit,
                              B = 100))
    
    est <- data.frame(
      DOMAIN = est.unit$est$eblup$domain,
      saeU = est.unit$est$eblup$eblup,
      saeU.se = sqrt(est.unit$mse[["mse"]])
    )

    est <- merge(est, dunitlut.unit[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "n.total"] <- "NBRPLT"
    

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
      est.unit <- hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        Xpop = xpophb, 
        silent = FALSE 
      )
    } else {
      est.unit <- hbsae::fSAE.Unit(
        y = pltdat.unit[[yn]],
        X = model.matrix(fmla.dom.unit[-2], pltdat.unit),
        area = pltdat.unit$DOMAIN,
        #Narea = dunitlut.unit$npixels,
        fpc = FALSE,
        Xpop = xpophb,
        prior = prior, 
        silent = FALSE
      )
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

    rm(dunitlut.unit)
    rm(fmla.dom.unit)
    rm(pltdat.unit)
    gc()
    
    return(est)
  }

}

SAest.area <- function(fmla.dom.area, pltdat.dom, dunitlut.dom, cuniqueid, 
	dunitvar, prednames.area, yn, SApackage, prior=NULL) {
  
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
    xpop.dom <- paste0(prednames.area, ".X.pop")
    fmla.dom.area2 <- as.formula(paste(paste0(yn, ".ybar.i"), 
                                  paste(xpop.dom, collapse= "+"), sep="~"))
    res <-
      JoSAE::sae.ul.f(samp.data = pltdat.area,
                      population.data = dunitlut.area,
                      k.ij = rep(1,nrow(pltdat.area)),
                      formula = fmla.dom.area,
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
    names(est)[names(est) == "n.i"] <- "NBRPLT"
    
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], DIR=NA, DIR.se=NA, 
				JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA, 
				NBRPLT=dunitlut.NA$n.total)
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
    
    est.area <- sae::mseFH(
      formula = fmla.dom.area,
      vardir = var,
      #method = "FH",
      method = "REML",
      data = dunitlut.area
    )
    
    est <- data.frame(
      DOMAIN = dunitlut.area[[dunitvar]],
      saeA = est.area$est$eblup[,1],
      saeA.se = sqrt(est.area$mse)
    )

    est <- merge(est, dunitlut.area[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "DOMAIN"] <- dunitvar
    names(est)[names(est) == "n.total"] <- "NBRPLT"

       
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], saeA=NA, saeA.se=NA, 
				NBRPLT=dunitlut.NA$n.total)
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
      est.area <- hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        silent=TRUE
      )
    } else {
      est.area <- hbsae::fSAE.Area(
        est.init = y,
        var.init = dunitlut.area[["var"]],
        X = X,
        prior = prior,
        silent=TRUE
      )
    }
    
    est <- data.frame(
      DOMAIN = est.area$predAreaNames,
      hbsaeA = est.area$est,
      hbsaeA.se = sqrt(est.area$mse)
    )

    est <- merge(est, dunitlut.area[, c(dunitvar, "n.total")], 
                 by.x="DOMAIN", by.y=dunitvar) 
    names(est)[names(est) == "n.total"] <- "NBRPLT"

       
    if (nrow(dunitlut.NA) > 0) {
      est.NA <- data.table(dunitlut.NA[[dunitvar]], hbsaeA=NA, hbsaeA.se=NA, 
				NBRPLT=dunitlut.NA$n.total)
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
    return(list(est=est, prednames.unit=NULL, prednames.area=NULL, 
		pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
  }  
   
  ## Variable selection for area and unit-level estimators
  ###################################################################
  if (variable.select) {
    ## use the plot level data as response
    prednames.unit <- suppressWarnings(preds.select(y=yn, 
		plt=pltdat.dom, aux=dunitlut.dom, prednames=prednames))

    ## use the domain level means as response
    prednames.area <- suppressWarnings(preds.select(y=yn, 
		plt=dunitlut.dom, aux=dunitlut.dom, prednames=prednames))

    if (length(prednames.area) == 0) {
      message("no predictors were selected for area-level model... returning NAs")
      if (SApackage == "JoSAE") {
        est <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
			DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, JA.synth=NA, 
			JA.synth.se=NA, NBRPLT=dunitlut.dom$n.total)
        setnames(est, "V1", dunitvar)
      } else if (SApackage == "sae") {
        est <- data.table(dunitlut.dom[[dunitvar]],
			saeA=NA, saeA.se=NA, NBRPLT=dunitlut.dom$n.total)
        setnames(est, "V1", dunitvar)
      } else if (SApackage == "hbsae") {  
        est <- data.table(dunitlut.dom[[dunitvar]],
			hbsaeA=NA, hbsaeA.se=NA, NBRPLT=dunitlut.dom$n.total)
        setnames(est, "V1", dunitvar)
      }
    } 
  } else {
    prednames.area <- prednames
    prednames.unit <- prednames
  } 

  if (length(prednames.area) > 0 || length(prednames.unit) > 0) {
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
        if (length(prednames.unit) > 0) {
          ## unit-level selection
          dev.new()
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% prednames.unit, "selected", "not selected")
            if (!is.null(largebnd.val) && largebnd.val != 1) { 
              #main <- paste0(largebnd.val, ": ", ylab, " - ", main2)
              main <- paste0(largebnd.val, " - ", main2)
            } else {
              main <- main2
            }
            plot(dunitlut.dom[[pred]], dunitlut.dom[[yn]], xlab=pred, ylab=ylab, main=main)
          }
        }

        if (length(prednames.area) > 0) {
          ## area-level selection
          dev.new()
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% prednames.area, "selected", "not selected")
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
        if (length(prednames.unit) > 0) {
          ## unit-level selection
          out_layer <- paste0("SApred_unit_", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)
    
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% prednames.unit, "selected", "not selected")
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

        if (length(prednames.area) > 0) {
     
          ## area-level selection
          out_layer <- paste0("SApred_area", ylab)
          jpgfn <- paste0(stepfolder, "/", out_layer, ".jpg")
          jpeg(jpgfn, res=300, units="in", width=pwidth, height=pheight)
    
          par(mfrow=c(rnbr,cnbr)) 
          for (pred in prednames) {
            main2 <- ifelse(pred %in% prednames.area, "selected", "not selected")
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

  if (length(prednames.unit) > 0) {
    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.unit <- stats::as.formula(paste(yn, paste(prednames.unit, collapse= "+"), sep="~"))

    ###  Unit-level estimates
    ############################################
    ## NOTE: changed prednames=prednames.select to prednames
    unit.JoSAE <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, yn=yn, SApackage="JoSAE", 
				dunitvar=dunitvar, prednames.unit=prednames.unit, prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(unit.JoSAE)) {
      unit.JoSAE <- data.table(dunitlut.dom[[dunitvar]],
		DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
		JU.EBLUP=NA, JU.EBLUP.se.1=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(unit.JoSAE, "V1", dunitvar)
    }

    unit.hbsae <- tryCatch(SAest.unit(fmla.dom.unit=fmla.dom.unit, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, yn=yn, SApackage="hbsae", 
				dunitvar=dunitvar, prednames.unit=prednames.unit, prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(unit.hbsae)) {
      unit.hbsae <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		hbsaeU=NA, hbsaeU.se=NA, NBRPLT=dunitlut.dom$n.total) 
      setnames(unit.hbsae, "V1", dunitvar)
    }

    ## Merge estimates
    est <- merge(unit.JoSAE, unit.hbsae[, c(dunitvar, "hbsaeU", "hbsaeU.se")], by=dunitvar)

  } else {

    message("no predictors were selected for unit-level model... returning NAs")
    est <- data.table(dunitlut.dom[[dunitvar]],
			DIR=NA, DIR.se=NA, JU.Synth=NA, JU.GREG=NA, JU.GREG.se=NA, 
			JU.EBLUP=NA, JU.EBLUP.se.1=NA, 
			saeU=NA, saeU.se=NA, 
			hbsaeU=NA, hbsaeU.se=NA, NBRPLT=dunitlut.dom$n.total)
    setnames(est, "V1", dunitvar)
  }

  if (length(prednames.area) > 0) {

    ## create model formula with predictors
    ## note: the variables selected can change depending on the order in original formula (fmla)
    fmla.dom.area <- stats::as.formula(paste(yn, paste(prednames.area, collapse= "+"), sep="~"))
 
    ###  Area-level estimates
    ############################################
    area.JoSAE <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, cuniqueid=cuniqueid, 
				dunitvar=dunitvar, prednames.area=prednames.area, 
				yn=yn, SApackage="JoSAE"),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(area.JoSAE)) {
      area.JoSAE <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		DIR=NA, DIR.se=NA, JFH=NA, JFH.se=NA, JA.synth=NA, 
		JA.synth.se=NA, NBRPLT=dunitlut.dom$n.total)
      setnames(area.JoSAE, "V1", dunitvar)
    }
    ## Merge estimates
    est <- merge(est, area.JoSAE[, c("DOMAIN", "JFH", "JFH.se", "JA.synth", "JA.synth.se")], 
		by=dunitvar)

    area.sae <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, cuniqueid=cuniqueid, 
				dunitvar=dunitvar, prednames.area=prednames.area, 
				yn=yn, SApackage="sae"),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(area.sae)) {
      area.sae <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		saeA=NA, saeA.se=NA, NBRPLT=dunitlut.dom$n.total) 
      setnames(area.sae, "V1", dunitvar)
    }
    est <- merge(est, area.sae[, c("DOMAIN", "saeA", "saeA.se")], by="DOMAIN")

    area.hbsae <- tryCatch(SAest.area(fmla.dom.area=fmla.dom.area, pltdat.dom=pltdat.dom, 
				dunitlut.dom=dunitlut.dom, cuniqueid=cuniqueid, 
				dunitvar=dunitvar, prednames.area=prednames.area, 
				yn=yn, SApackage="hbsae", prior=prior),
				error=function(err) {
					message(err, "\n")
					return(NULL)
				} )
    if (is.null(area.hbsae)) {
      area.hbsae <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
		hbsaeA=NA, hbsaeA.se=NA, NBRPLT=dunitlut.dom$n.total) 
      setnames(area.hbsae, "V1", dunitvar)
    }
    est <- merge(est, area.hbsae[, c("DOMAIN", "hbsaeA", "hbsaeA.se")], by="DOMAIN")

  } else {
    message("no predictors were selected for area-level model... returning NAs")
    est.NA <- data.table(dunitlut.dom[[dunitvar]], AOI=dunitlut.dom$AOI,
			JFH=NA, JFH.se=NA, JA.synth=NA, JA.synth.se=NA, 
			saeA=NA, saeA.se=NA, 
			hbsaeA=NA, hbsaeA.se=NA)
    setnames(est.NA, "V1", dunitvar)

    est <- merge(est, est.NA, by="DOMAIN")
  }

  ## Merge NBRPLT.gt0
  est <- merge(est, NBRPLT.gt0, by="DOMAIN")

  ## Merge AOI
  if (!"AOI" %in% names(est) && "AOI" %in% names(dunitlut.dom)) {
    est <- merge(est, dunitlut.dom[, c("DOMAIN", "AOI")], by="DOMAIN")
  }
  
  rm(unit.JoSAE)
  rm(unit.hbsae)
  rm(area.JoSAE)
  rm(area.sae)
  rm(area.hbsae)
  gc()

  return(list(est=est, prednames.unit=prednames.unit, 
		prednames.area=prednames.area,
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
    if (SAmethod == "unit") {
      if (SApackage == "JoSAE") {
        domest <- data.table(dom, matrix(c(rep(NA,7), 0, 0), 1,9))
        setnames(domest, c(domain, "DIR", "DIR.se", "JU.Synth", "JU.GREG",
		"JU.GREG.se", "JU.EBLUP", "JU.EBLUP.se.1", "NBRPLT", "NBRPLT.gt0"))
      } else if (SApackage == "sae") {
        domest <- data.table(dom, matrix(c(rep(NA,2), 0, 0), 1, 4))
        setnames(domest, c(domain, "saeU", "saeU.se"))
      } else if (SApackage == "hbsae") {
        domest <- data.table(dom, matrix(c(rep(NA,2), 0, 0), 1, 4))
        setnames(domest, c(domain, "hbsaeU", "hbsaeU.se"))
      }

    } else if (SAmethod == "area") {
      if (SApackage == "JoSAE") {
        domest <- data.table(dom, matrix(c(rep(NA,6), 0, 0), 1,8))
        setnames(domest, c(domain, "DIR", "DIR.se", "JFH", "JFH.se",
		"JA.synth", "JA.synth.se", "NBRPLT", "NBRPLT.gt0"))
      } else if (SApackage == "sae") {
        domest <- data.table(dom, matrix(c(rep(NA,2), 0, 0), 1, 4))
        setnames(domest, c(domain, "saeA", "saeA.se"))
      } else if (SApackage == "hbsae") {
        domest <- data.table(dom, matrix(c(rep(NA,2), 0, 0), 1, 4))
        setnames(domest, c(domain, "hbsaeA", "hbsaeA.se"))
      }

    } 
    return(list(domest, prednames.unit=NULL, prednames.area=NULL, dunitlut.dom=NULL))
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
  return(domest)
}


########################################################################
## By largebnd
########################################################################
SAest.large <- function(largebnd.val, dat, cuniqueid, largebnd.att, 
		dunitlut, dunitvar="DOMAIN", SApackage="JoSAE", 
		SAmethod="unit", domain, response, prednames=NULL,
		showsteps=FALSE, savesteps=FALSE, stepfolder=NULL, 
		prior=NULL, variable.select=TRUE) {

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
			showsteps=showsteps, savesteps=savesteps, stepfolder=stepfolder,
			prior=prior, variable.select=variable.select)
 
  if (length(doms) > 1) {
    est.large <- do.call(rbind, do.call( rbind, estlst)[,"est"])
    prednames.unit <- do.call(rbind, estlst)[,"prednames.unit"]
    prednames.area <- do.call(rbind, estlst)[,"prednames.area"]
    names(prednames.unit) <- doms
    names(prednames.area) <- doms
    pltdat.dom <- do.call(rbind, do.call(rbind, estlst)[,"pltdat.dom"])
    dunitlut.dom <- do.call(rbind, do.call(rbind, estlst)[,"dunitlut.dom"])
  } else {
    est.large <- do.call(rbind, estlst)[,"est"]$est
    prednames.unit <- do.call(rbind, estlst)[,"prednames.unit"]$prednames.unit
    prednames.area <- do.call(rbind, estlst)[,"prednames.area"]$prednames.area
    pltdat.dom <- do.call(rbind, estlst)[,"pltdat.dom"]$pltdat.dom
    dunitlut.dom <- do.call(rbind, estlst)[,"dunitlut.dom"]$dunitlut.dom
  }
  setkeyv(est.large, dunitvar)
  setkeyv(setDT(pltdat.dom), dunitvar)
  setkeyv(setDT(dunitlut.dom), dunitvar)
  
  rm(estlst)
  gc()

  return(list(est.large=est.large, 
			prednames.unit=prednames.unit, 
			prednames.area=prednames.area, 
			pltdat.dom=pltdat.dom, dunitlut.dom=dunitlut.dom))
}       

