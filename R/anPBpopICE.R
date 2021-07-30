anPBpopICE <- function(ice.pntfn=NULL, ice.pltfn=NULL, T1, T2, 
	plotid="plot_id", pntid="dot_cnt", pltassgn=NULL, pltassgnid=NULL, 
	unitvar=NULL, unitvar2=NULL, unitarea=NULL, areavar="ACRES", 
	unit.action="keep", strata=FALSE, stratalut=NULL, strvar=NULL,  
	getwt=TRUE, getwtvar="P1POINTCNT", stratcombine=TRUE, sumunits=FALSE, 
	saveobj=FALSE, savedata=FALSE, outfolder=NULL, overwrite=FALSE, 
	PBdataICE=NULL, ...){


  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  #datstrat=datSTRATA=datACRES <- NULL

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  returnlst <- list()


  ## Check savedata 
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data extraction?", first="NO", gui=gui)  

  ## Check overwrite, outfn.date, outfolder, outfn 
  ########################################################
  if (savedata) 
    outfolder <- pcheck.outfolder(outfolder, gui=gui)



  #########################################################################
  ## Get ICE data 
  #########################################################################
  if (is.null(PBdataICE)) {
    PBdataICE <- anPBpopICE_data(ice.pntfn, ice.pltfn=ice.pltfn,
			plotid="plot_id", pntid="dot_cnt", T1=T1, T2=T2, 
			appendluts=TRUE, savedata=savedata, outfolder=outfolder, 
			overwrite=overwrite, ...)
    returnlst$PBdataICE <- PBdataICE
  } else {
    PBdataICE <- pcheck.object(PBdataICE, objnm="PBdataICE", 
		list.items=c("ice.pnt", "ice.plt", "plotid", "pntid"))
  }
  names(PBdataICE)
  ice.pnt <- PBdataICE$ice.pnt
  ice.plt <- PBdataICE$ice.plt
  plotid <- PBdataICE$plotid
  pntid <- PBdataICE$pntid
  domlut <- PBdataICE$domlut
  changelut <- PBdataICE$reflst$changelut
  coverlut <- PBdataICE$reflst$coverlut
  uselut <- PBdataICE$reflst$uselut
  agentlut <- PBdataICE$reflst$agentlut
  rm(PBdataICE)
  gc()

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
  ## Check other columns in ice.pnt
  other_cols <- c("use_1_2", "cover_1_2", "use_1_2_FOR")
  if (!all(other_cols %in% names(ice.pnt))) {     
    miss <- other_cols[!other_cols %in% names(ice.pnt)]
    message("missing columns in ice.pnt: ", toString(miss))
  }
 
  #########################################################################
  ## Get population data
  ##################################################################
  PBpopdatICE <- modPBpop(pntdat=ice.pnt, plt=ice.pltfn, plotid=plotid, pntid=pntid, 
        		puniqueid=plotid, pltassgn=pltassgn, pltassgnid=pltassgnid,
			unitarea=unitarea, unitvar=unitvar, areavar=areavar, 
			unit.action=unit.action, strata=strata, 
			strvar=strvar, getwt=getwt, getwtvar=getwtvar, 
			stratcombine=stratcombine, sumunits=sumunits, savedata=savedata, 
			outfolder=outfolder)
  PBpopdatICE$reflst <- PBdataICE$reflst
  PBpopdatICE$domlut <- domlut

  if (saveobj) {
    objfn <- getoutfn(outfn="PBpopdatICE", outfolder=outfolder, 
		overwrite=overwrite, ext="rda")
    save(PBpopdatICE, file=objfn)
    message("saving object to: ", objfn)
  } 
    
  return(PBpopdatICE) 
}

