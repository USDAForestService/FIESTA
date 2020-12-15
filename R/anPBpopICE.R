anPBpopICE <- function(ice.pntfn, ice.plotfn=NULL, T1, T2, 
	plotid="plot_id", pntid="dot_cnt", pltassgn=NULL, pltassgnid=NULL, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", 
	unitcombine=FALSE, strata=FALSE, stratalut=NULL, strvar=NULL,  
	getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, sumunits=FALSE, 
	saveobj=FALSE, savedata=FALSE, outfolder=NULL, ...){


  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE



  #########################################################################
  ## Get ICE data 
  ##################################################################
  icedat <- anPBpopICE_data(ice.pntfn, plotid="plot_id", pntid="dot_cnt", 
			T1=T1, T2=T2, appendluts=TRUE, ...)
  names(icedat)
  ice.pnts <- icedat$ice.pnts
  plotid <- icedat$plotid
  pntid <- icedat$pntid
  domlut <- icedat$domlut
  changelut <- icedat$reflst$changelut
  coverlut <- icedat$reflst$coverlut
  uselut <- icedat$reflst$uselut
  agentlut <- icedat$reflst$agentlut

  ## Check data in changelut
  if (!is.null(changelut)) {
    agent_cols <- c("chg_ag_2_GRP", "chg_ag_2_GRP_nm", "change_pnt")
    if (!all(agent_cols %in% names(agentlut))) {     
      miss <- agent_cols[!agent_cols %in% names(agentlut)]
      message("missing columns in agentlut: ", toString(miss))
    }
  }
  ## Check data in coverlut
  if (!is.null(coverlut)) {
    cover_cols <- c("cover", "cover_nm", "cover_GRP", "cover_GRP_nm",
		"cover_GRP2", "cover_GRP2_nm")
    if (!all(cover_cols %in% names(coverlut))) {     
      miss <- cover_cols[!cover_cols %in% names(coverlut)]
      message("missing columns in coverlut: ", toString(miss))
    }
  }
  ## Check data in uselut
  if (!is.null(uselut)) {
    use_cols <- c("use", "use_nm", "use_FOR", "use_FOR_nm")
    if (!all(use_cols %in% names(uselut))) {     
      miss <- use_cols[!use_cols %in% names(uselut)]
      message("missing columns in uselut: ", toString(miss))
    }
  }
  ## Check other columns in ice.pnts
  other_cols <- c("use_1_2", "cover_1_2", "use_1_2_FOR")
  if (!all(other_cols %in% names(ice.pnts))) {     
    miss <- other_cols[!other_cols %in% names(ice.pnts)]
    message("missing columns in ice.pnts: ", toString(miss))
  }
 
  #########################################################################
  ## Get population data
  ##################################################################
  PBpopdatICE <- modPBpop(pnt=ice.pnts, plt=ice.plotfn, plotid=plotid, pntid=pntid, 
        		puniqueid=plotid, pltassgn=pltassgn, pltassgnid=pltassgnid,
			unitarea=unitarea, unitcombine=unitcombine, strata=strata, 
			strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
			stratcombine=stratcombine, sumunits=sumunits, saveobj=saveobj,
			savedata=savedata, outfolder=outfolder)
  PBpopdatICE$reflst <- icedat$reflst
  PBpopdatICE$domlut <- domlut
    
  return(PBpopdatICE) 
}

