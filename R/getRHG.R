getRHG <- function(pltx, puniqueid, unitvars, strvar, nonresp.minplotnum=5) {
  ## DESCRIPTION: Get Response Homogeneity Groups (RHG) 

  ## Set global variables
  RHG=TESTVAR=PLOT_STATUS_CD=Nsampmeth=strat <- NULL


  ## Get plot column names
  pltnmlst <- names(pltx)

  ## Set minimum plot number of nonresp 
  #nonresp.minplotnum <- 5

  ## Create table of number of plots by estimation unit and strata
  P2POINTCNT <- pltx[, list(P2POINTCNT=uniqueN(get(puniqueid))),
		by=c(unitvars, strvar)]
  setkeyv(P2POINTCNT, c(unitvars, strvar))


  ## Generate table of nonsampled plots by strata (if nonresp=TRUE)
  if ("PLOT_STATUS_CD" %in% pltnmlst) {
    if (!3 %in% unique(pltx[["PLOT_STATUS_CD"]])) {
      stop("must include PLOT_STATUS_CD = 3 in dataset")
    }

    ## Create table with number of nonsampled plots by strata, substrata
    nonresplut <- pltx[PLOT_STATUS_CD == 3, uniqueN(get(puniqueid)),
					by=c(unitvars, strvar, "SAMP_METHOD_CD")]
    setnames(nonresplut, "V1", "n.nonresp")
    setkeyv(nonresplut, c(unitvars, strvar))
  } else {
    stop("must include PLOT_STATUS_CD")
  }

  P2POINTCNTNR <- setDF(merge(P2POINTCNT, nonresplut, all.x=TRUE))
  P2POINTCNTNR[is.na(P2POINTCNTNR$n.nonresp), "n.nonresp"] <- 0

  P2POINTCNTNR$n.resp <- P2POINTCNTNR$P2POINTCNT - P2POINTCNTNR$n.nonresp

  ## Subset strata from FIADB that have number of plots less than nonresp.minplotnum
  appendltmin <- FALSE
  if (any(P2POINTCNTNR$n.resp <= nonresp.minplotnum)) {
    appendltmin <- TRUE
    unit.ltmin <- P2POINTCNTNR[P2POINTCNTNR$n.resp <= nonresp.minplotnum, unitvars]
  } 

#      ## Check - Get number of plots by estimation unit (maybe take out)
      unit.N <- setDT(P2POINTCNTNR)[, list(Nstrata =.N), by=c(unitvars)]
#      strata.N <- merge(P2POINTCNTNR[, c(unitvars, strvar, "P2POINTCNT"), with=FALSE],
#		unit.N, by=unitvars)
#      if (any(strata.N$P2POINTCNT < 10 & strata.N$n.strata > 1)) {
#         message("there are estimation units with > 1 strata and has strata with less than 10 plots")
#      }
    
  if ("SAMP_METHOD_CD" %in% pltnmlst) {
    ## Create table with response homogeneity groups (RHGs) 
    RHGlut <- pltx[pltx$PLOT_STATUS_CD < 3, uniqueN(get(puniqueid)),
					by=c(unitvars, strvar, "SAMP_METHOD_CD")]
    setnames(RHGlut, "V1", "n.resp")
    setkeyv(RHGlut, c(unitvars, strvar, "SAMP_METHOD_CD"))
#RHGlut[RHGlut$ESTN_UNIT <= 15,]


#######################
        ## TESTING - to make sure no nonresp with office visits##
#        RHGlut.test <- pltcondx[pltcondx$PLOT_STATUS_CD == 3, uniqueN(get(puniqueid)),
#					by=c(unitvars, strvar, "SAMP_METHOD_CD")]
#        setnames(RHGlut.test, "V1", "n.nonresp")
#        setkeyv(RHGlut.test, c(unitvars, strvar, "SAMP_METHOD_CD"))
#RHGlut.test[RHGlut.test,]
#######################

    RHGlut <- merge(RHGlut, unit.N, by=unitvars)

   
    ## Get number of SAMP_METHOD_CD values by estimation unit, strata
    RHGlut[, Nsampmeth := .N, by=c(unitvars, strvar)]

    if (appendltmin) {
      if (length(unitvars) > 1) {
        RHGlut[, TESTVAR := do.call(paste, .SD), .SDcols=unitvars]
        setDT(unit.ltmin)[, TESTVAR := do.call(paste, .SD), .SDcols=unitvars]
        RHGlut.ltmin <- RHGlut[RHGlut$TESTVAR %in% unit.ltmin$TESTVAR, ]
        RHGlut <- RHGlut[!RHGlut$TESTVAR %in% unit.ltmin$TESTVAR, ]
        RHGlut.ltmin[, TESTVAR := NULL] 
        RHGlut[, TESTVAR := NULL] 
      } else {
        RHGlut.ltmin <- RHGlut[RHGlut[[unitvars]] %in% unit.ltmin, ]
        RHGlut <- RHGlut[!RHGlut[[unitvars]] %in% unit.ltmin, ] 
      }
    }

    ## Group SAMP_METHOD_CD with n.resp less than or equal to nonresp.minplotnum
    if (!is.factor(RHGlut$SAMP_METHOD_CD)) {
      RHGlut$strat <- factor(RHGlut$SAMP_METHOD_CD)
    }
    RHGlut$strat <- as.numeric(RHGlut$strat)
    RHGlut$stratnew <- as.character(-1)


    ###########################################################################
    ## Create RHGgrp column based on specific criteria...
    ## First, if the number of Nsampmeth = 2 and one of RHG <= nonresp.minplotnum, then RHGtmp = '1-2'
    ## 1) Within a stratum, if there were both field and office plots and one of the 
    ##    categories (field/office) had less than 5 plots, no RHG was created (A)
    ## 2) Within a stratum, if there is only office or only field plots (i.e., Nsampmeth=1), 
    ##    no RHG is created (A)
    ## 3) Within a stratum, if there is both office and field plots (i.e., Nsampmeth=2), 
    ##    and both have more than minimum number of plots, then if field, RHG = F; if office, RHG = O.
    ## 4) Else, RHG = E.
    ###########################################################################
    RHGgrp <- RHGlut[, groupStrata(.SD, minplotnum=nonresp.minplotnum, nvar="n.resp"), 
			by=c(unitvars, strvar)]

    if (appendltmin) {
      ## Group strata with n.resp less than 2
      if (!is.factor(RHGlut.ltmin$SAMP_METHOD_CD)) {
        RHGlut.ltmin$strat <- factor(RHGlut.ltmin$SAMP_METHOD_CD)
      }
      RHGlut.ltmin$strat <- as.numeric(RHGlut.ltmin$strat)
      RHGlut.ltmin$stratnew <- as.character(-1)

      RHGgrp.ltmin <- RHGlut.ltmin[, groupStrata(.SD, minplotnum=2, nvar="n.resp"), 
			by=c(unitvars, strvar)]

      ## Append to RHGgrp
      RHGgrp <- rbind(RHGgrp, RHGgrp.ltmin)
      setorderv(RHGgrp, c(unitvars, strvar))
    }

    RHGgrp[, strat := NULL]
    setnames(RHGgrp, "stratnew", "RHGtmp")

    RHGgrp$RHG <- with(RHGgrp, 
		ifelse(RHGtmp == "1-2", "A",  ## 1
		   ifelse(Nsampmeth == 1, "A",  ## 2   (added by paul)
			ifelse(Nsampmeth == 2 & RHGtmp == 1, "F",  ## 3
				ifelse(Nsampmeth == 2 & RHGtmp == 2, "O",  ## 3
					"E")))))


    ## Sum n.resp to new strata groups (removing RHGgrp = A)
 #  RHGgrp <- RHGgrp[RHGgrp$RHG != "A", lapply(.SD, sum, na.rm=TRUE), 
 #				by=c(unitvars, strvar, "SAMP_METHOD_CD", "RHG"), .SDcols=c("n.resp")]

    ###########################################################################

    ## TESTING
    #Nsampmeth <- RHGlut[, .N, by=c(unitvars, strvar)]
    #RHGgrp <- merge(RHGgrp, unit.N, by=unitvars)
    #RHGgrp <- merge(RHGgrp, Nsampmeth, by=c(unitvars, strvar))
    #setnames(RHGgrp, "N", "Nsampmeth")
    #RHGgrp

    ## Merge RHGgrp to pltx to append RHG substrata variable 
    ## Note: kicks out nonsampled plots
    pltx <- merge(pltx, RHGgrp[, c(unitvars, strvar, "SAMP_METHOD_CD", "RHG"), with=FALSE], 
			by=c(unitvars, strvar, "SAMP_METHOD_CD"), all.x=TRUE)
    setkeyv(pltx, puniqueid)
    pltx[is.na(RHG), RHG := "A"]

    RHGgrp <- RHGgrp[, lapply(.SD, sum, na.rm=TRUE), 
				by=c(unitvars, strvar, "RHG"), .SDcols=c("n.resp")]

    RHGlut <- merge(RHGgrp, nonresplut[, c(unitvars, strvar, "n.nonresp"), with=FALSE], 
				by=c(unitvars, strvar), all.x=TRUE, all.y=TRUE)
    RHGlut[RHGlut$RHG == "O", "n.nonresp"] <- 0
    #dim(test)
    #RHGlut[, SAMP_METHOD_CD := NULL,]
    RHGlut[is.na(RHGlut$n.nonresp), "n.nonresp"] <- 0

  } else {
    stop("must include SAMP_METHOD_CD")
  }

  return(list(pltx=pltx, RHGlut=RHGlut, P2POINTCNT=P2POINTCNT, nonresplut=nonresplut))
}   


