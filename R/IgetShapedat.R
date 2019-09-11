getShapedat <- function(plt, cond=NULL, addcond=FALSE, condid1=FALSE, actual=FALSE, 
	ACTUALplot=NULL, spcoords=NULL, xycoords=NULL, puniqueid="CN",
 	cuniqueid="PLT_CN", ACI=FALSE, fromqry=NULL, datsource=NULL, dbconn=NULL, 
	xfilter=NULL){

  ## NOTE: The shapefile is generated from coordinates specified from the variable 
  ##	'spcoords' which will identify what type of coordinates to use ("ACTUAL", "FUZZED", 
  ##	or "DIGITIZED"). The shapefile will be in GEOGRAPHIC projection with NAD83 datum. 
  ##	The attributes of the shapefile will include all the plot-level variables and some 
  ##	condition-level variables (depending on whether they were selected by the user).
  ##	The potential list of condition variables are: PLT_CN, CONDPROP_UNADJ, CONDID, 
  ## COND_STATUS_CD, FORTYPCD, FLDTYPCD, FORTYPCDCALC, STDSZCD, FLDSZCD, STDAGE, 
  ##	LIVE_CANOPY_CVR_PCT, OWNGRPCD). 
  ##    
  ## GETS A FEW CONDITION-LEVEL VARIABLE TO ADD TO PLOT-LEVEL DATA.
  ## BASED ON: (1) MIN COND_STATUS_CD; (2) MAX CONDPROP_UNADJ; (3)MAX CRCOV;  
  ##  		(4)MIN STDSZCD; (5)MIN CONDID
  #####################################################################################

  if (!is.null(cond) && addcond) {
    cndpltvarlst <- c(cuniqueid, "CONDPROP_UNADJ", "CONDID", "COND_STATUS_CD", "OWNCD",
		"FORTYPCD", "FLDTYPCD", "FORTYPCDCALC", "FORTYPGRP", "STDSZCD", "FLDSZCD", "STDAGE", 
		"LIVE_CANOPY_CVR_PCT", "ADFORCD", "RESERVCD", "DSTRBCD1", "DSTRBYR1", "ASPECT",
		"SLOPE")
    if (ACI) cndpltvarlst <- c(cndpltvarlst, "COND_STATUS_CD")

    if (!is.null(ACTUALplot)) {
      acndpltvarlst <- c("PLOT_PERIODIC", "ACTUAL_OWNCD")
      if (!all(acndpltvarlst %in% names(ACTUALplot))) 
        acndpltvarlst <- {} 
    } else{ 
      cndpltvarlst <- c(cndpltvarlst, "OWNCD", "OWNGRPCD")
    }

    if (any(names(cond) %in% cndpltvarlst)) {
      cndpltvars <- names(cond)[which(names(cond) %in% cndpltvarlst)]
      condx <- cond[,cndpltvars]
    }

    if (!condid1) { 
      firstrow=FALSE
  
      ## GETS PLOTS WITH NO DUPLICATES
      tab <- data.frame(table(condx[,cuniqueid]))
      names(tab) <- c(cuniqueid, "Freq")
      pltnodups <- tab[tab$Freq == 1,]
      onecond <- condx[condx[,cuniqueid] %in% pltnodups[,cuniqueid],]
      conddups <- condx[!condx[,cuniqueid] %in% pltnodups[,cuniqueid],]

      if (nrow(conddups) != 0) {
        CONDMETHOD <- "CND"

        ## GETS MINIMUM COND_STATUS_CD
        if ("COND_STATUS_CD" %in% names(condx)) {
          conddups <- getdups(cx=conddups, varnm="COND_STATUS_CD", fun=min)
          CONDMETHOD <- paste0(CONDMETHOD, "_ST")
          if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {

            ## GETS MAX CONDPROP_UNADJ
            if ("CONDPROP_UNADJ" %in% names(condx)) {
              conddups <- getdups(cx=conddups, varnm="CONDPROP_UNADJ", fun=max)
              CONDMETHOD <- paste0(CONDMETHOD, "_CP")
              if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {

                ## GETS MAX LIVE_CANOPY_CVR_PCT
                if ("LIVE_CANOPY_CVR_PCT" %in% names(condx) & 
					!all(is.na(condx$LIVE_CANOPY_CVR_PCT))) {
                  conddups <- getdups(cx=conddups, varnm="LIVE_CANOPY_CVR_PCT", fun=max)
                  CONDMETHOD <- paste0(CONDMETHOD, "_CC")
                  if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
            
                    ## GETS MIN STSZCD
                    if ("STDSZCD" %in% names(condx)) {
                      conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                      CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                      if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
                        firstrow <- TRUE
                      } else {
                        onecond <- rbind(onecond, conddups)
                        onecondvars <- names(onecond)[-1]
                      }
                    } else { 
                      firstrow <- TRUE 
                    }  # end STDSZCD
                  } else {
                    onecond <- rbind(onecond, conddups)
                    onecondvars <- names(onecond)[-1]
                  }
                } else {
                  ## GETS MIN STSZCD
                  if ("STDSZCD" %in% names(condx)) {
                    conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                    CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                    if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
                      firstrow <- TRUE
                    } else {
                      onecond <- rbind(onecond, conddups)
                      onecondvars <- names(onecond)[-1]
                    }
                  } else { 
                    firstrow <- TRUE 
                  }  ## end LIVE_CANOPY_COVER_PCT
                }
              } else {
                onecond <- rbind(onecond, conddups)
                onecondvars <- names(onecond)[-1]
              }
            } else {
              ## GETS MAX LIVE_CANOPY_CVR_PCT
              if ("LIVE_CANOPY_CVR_PCT" %in% names(condx)) {
                conddups <- getdups(cx=conddups, varnm="LIVE_CANOPY_CVR_PCT", fun=max)
                CONDMETHOD <- paste0(CONDMETHOD, "_CC")
                if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
            
                  ## GETS MIN STDSZCD
                  if ("STDSZCD" %in% names(condx)) {
                    conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                    CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                    if (length(unique(conddups[,cuniqueid])) < nrow(conddups)){
                      firstrow <- TRUE
                    } else {
                      onecond <- rbind(onecond, conddups)
                      onecondvars <- names(onecond)[-1]
                    }
                  } else { 
                    firstrow <- TRUE 
                  }  ## end STDSZCD
                }
              } else {
                ## GETS MIN STSZCD
                if ("STDSZCD" %in% names(condx)) {
                  conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                  CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                  if (length(unique(conddups$PLT_CN)) < nrow(conddups)){
                    firstrow <- TRUE
                  } else {
                    onecond <- rbind(onecond, conddups)
                    onecondvars <- names(onecond)[-1]
                  }
                } else { 
                  firstrow <- TRUE 
                }  ## end STDZCD
              }  ## end LIVE_CANOPY_COVER_PCT
            }  ## end CONDPROP_UNADJ

          } else {
            onecond <- rbind(onecond, conddups)
            onecondvars <- names(onecond)[-1]
          }

        } else {  ## if no COND_STATUS_CD

          ## GETS MAX CONDPROP_UNADJ
          if ("CONDPROP_UNADJ" %in% names(condx)) {
            conddups <- getdups(cx=condx, varnm="CONDPROP_UNADJ", fun=max)
            CONDMETHOD <- paste0(CONDMETHOD, "_CP")
            if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
  
              ## GETS MAX LIVE_CANOPY_CVR_PCT
              if ("LIVE_CANOPY_CVR_PCT" %in% names(condx)) {
                conddups <- getdups(cx=conddups, varnm="LIVE_CANOPY_CVR_PCT", fun=max)
                CONDMETHOD <- paste0(CONDMETHOD, "_CC")
                if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
            
                  ## GETS MIN STSZCD
                  if ("STDSZCD" %in% names(condx)) {
                    conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                    CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                    if (length(unique(conddups[,cuniqueid])) < nrow(conddups))
                      firstrow <- TRUE
                  } else { firstrow <- TRUE }
                } else {
                  onecond <- rbind(onecond, conddups)
                  onecondvars <- names(onecond)[-1]
                }
              } else {
                ## GETS MIN STSZCD
                if ("STDSZCD" %in% names(condx)) {
                  conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                  CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                  if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
                    firstrow <- TRUE
                  } else {
                    onecond <- rbind(onecond, conddups)
                    onecondvars <- names(onecond)[-1]
                  }
                } else { firstrow <- TRUE }
              }
              
            } else {
              onecond <- rbind(onecond, conddups)
              onecondvars <- names(onecond)[-1]
            }
          } else {
            ## GETS MAX LIVE_CANOPY_CVR_PCT
            if ("LIVE_CANOPY_CVR_PCT" %in% names(condx)) {
              conddups <- getdups(cx=conddups, varnm="LIVE_CANOPY_CVR_PCT", fun=max)
              CONDMETHOD <- paste0(CONDMETHOD, "_CC")
              if (length(unique(conddups[,cuniqueid])) < nrow(conddups)) {
            
                ## GETS MIN STSZCD
                if ("STDSZCD" %in% names(condx)) {
                  conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                  CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                  if (length(unique(conddups[,cuniqueid])) < nrow(conddups))
                    firstrow <- TRUE
                } else { firstrow <- TRUE }
              } else {
                onecond <- rbind(onecond, conddups)
                onecondvars <- names(onecond)[-1]
              }
            } else {
              ## GETS MIN STSZCD
              if ("STDSZCD" %in% names(condx)) {
                conddups <- getdups(cx=conddups, varnm="STDSZCD", fun=min)
                CONDMETHOD <- paste0(CONDMETHOD, "_SZ")
                if (length(unique(conddups[,cuniqueid])) < nrow(conddups))
                  firstrow <- TRUE
              } else { firstrow <- TRUE }
            }
          }
        }  ## end COND_STATUS_CD
      } else {  ## if no duplicates
        CONDMETHOD <- "NOCND"
      }
      
      if (firstrow == TRUE) {

        ## GETS PLOTS WITH NO DUPLICATES
        tab2 <- data.frame(table(conddups[,cuniqueid]))
        names(tab2) <- c(cuniqueid, "Freq")
        pltnodups2 <- tab2[tab2$Freq == 1,]
        onecond <- rbind(onecond, 
		conddups[conddups[,cuniqueid] %in% pltnodups2[,cuniqueid],])
        conddups <- conddups[!conddups[,cuniqueid] %in% pltnodups2[,cuniqueid],]

        ## IF DUPLICATE PLOTS, GET FIRST RECORD
        conddups <- getdups(cx=conddups, varnm="CONDID", fun=min)
        CONDMETHOD <- paste0(CONDMETHOD, "_C1")

        onecond <- rbind(onecond, conddups)
        onecondvars <- names(onecond)[-1]
      }

      onecond$CONDMETHOD <- CONDMETHOD
      onecond[is.na(onecond)] <- 0
    
      cat("\n", "CONDMETHOD added to attribute table: ",
	"Describes which condition-level variables were used to select 1 condition per plot",
        "  ST - the minimum condition status, emphasizing forested conditions", 
        "  CP - the highest proportion", 
        "  CC - the greatest live percent canopy cover", 
        "  SZ - the largest stand size class",
        "  C1 - the minimum CONDID", sep="\n", "\n")

    } else {  ## if condid1 = TRUE
      
      onecond <- condx[condx$CONDID == 1, ]
      onecond$CONDMETHOD <- "CONDID1"
    }

      
    if (actual && spcoords != "PUBLIC") {
      ## MERGE ACTUAL TABLE TO COND TABLE
      conda <- merge(unique(ACTUALplot[,c(cuniqueid, xycoords)]), onecond, by=cuniqueid)
      if ("CONDID" %in% names(ACTUALplot))
        conda <- merge(conda, ACTUALplot[,c(cuniqueid, "CONDID", acndpltvarlst)], 
			by=c(cuniqueid, "CONDID"))
      
      if (!is.null(plt)) {
        ## MERGE PLOT TABLE WITH TABLE WITH ONE CONDITION RECORD
        if (any(c("LAT", "LON") %in% names(conda))) 
          conda <- conda[-which(names(conda) %in% c("LAT","LON"))]
        spplt <- merge(plt[,names(plt)], conda, by.x=puniqueid, by.y=cuniqueid)
      } else {
        spplt <- conda
      }
    } else {
      if (!is.null(plt)) {
        ## MERGE PLOT TABLE WITH TABLE WITH ONE CONDITION RECORD
        spplt <- merge(plt[,names(plt)], onecond, by.x=puniqueid, by.y=cuniqueid)
      } else {
        if (which(names(plt) %in% c("LAT","LON")) == 2) {
          spplt <- merge(unique(cond[,c(cuniqueid, xycoords)]), onecond, by.x=puniqueid, 
			by.y=cuniqueid)
        } else {
          warning("NO SHAPEFILE WAS GENERATED BECAUSE XY COORDS NOT IN TABLE")
        }
      }
    }
  } else {  ## COND IS NULL

    if (actual & spcoords != "PUBLIC") {
      if (is.null(plt)) {
        spplt <- unique(ACTUALplot[, c(cuniqueid, xycoords)])
      } else {
        spplt <- merge(plt[,names(plt)], unique(ACTUALplot[,c(cuniqueid, xycoords)]), 
			by.x=puniqueid, by.y=cuniqueid)
      }
    } else {
      if (is.null(plt)) {
        spvars <- paste0("p.", puniqueid, ", ", 
          addcommas(xycoords, paste0("p")))
        sppltqry <- paste("select ", spvars, "from", fromqry, "where", xfilter)
        tryCatch( spplt <- DBqryORACLE(sppltqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("spplt query is invalid"))

      } else {
        if (xycoords %in% names(plt)) {
          spplt <- plt[,names(plt)]
          names(spplt)[names(spplt) == "CN"] <- "PLT_CN"
        } else {
          spvars <- paste0("p.", puniqueid, ", ", 
            addcommas(xycoords, paste0("p")))

          sppltqry <- paste("select ", spvars, "from", fromqry, "where", xfilter)
          if (datsource == "CSV") {
            sppltx <- sqldf::sqldf(sppltqry)
          } else {
            sppltqry <- paste("select ", spvars, "from", fromqry)
            tryCatch( spplt <- DBqryORACLE(sppltqry, dbconn, dbconnopen=TRUE), 
			error=function(e) stop("spplt query is invalid"))
            spplt <- sp::merge(sppltx, plt[,names(plt)], by="CN")
          }
          names(spplt)[names(spplt) == "CN"] <- "PLT_CN"
        }  
      }
    }

    ## CHANGE CN TO PLT_CN
    if ("CN" %in% names(spplt) && !"PLT_CN" %in% names(spplt))
      names(spplt)[names(spplt) == "CN"] <- "PLT_CN"
  }
 
  return(spplt)
}
