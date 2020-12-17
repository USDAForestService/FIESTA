anPBpopICE_data <- function(ice.pntfn, ice.pltfn=NULL, T1, T2, plotid="plot_id", 
	pntid="dot_cnt", changelut=NULL, coverlut=NULL, uselut=NULL, agentlut=NULL, 
	appendluts=TRUE, domlut=NULL, savedata=FALSE, outfolder=NULL, ...){

  ##########################################################################
  ## DESCRIPTION: Set up ICE data with code classes
  ## ice.pnt	- point-level data file name
  ## T1		- year of Time 1 (YYYY)
  ## T2		- year of Time 2 (YYYY)
  ## plotid	- unique identifier of plots in ice.pnt and ice.plt
  ## pntid		- unique identifier of points in ice.pnt
  ## changelut	- different look up table for land use or land cover change
  ##				(see ref_ICE_change)
  ## coverlut	- different look up table for land cover (see ref_ICE_cover)
  ## uselut	- different look up table for land use (see ref_ICE_use)
  ## agentlut	- different look up table for causal agent (see ref_ICE_agent)
  ## appendluts	- logical. if TRUE, append all look up tables to ice.pnt
  ## domlut	- data frame defining domain codes, names, and titles
  ## savedata	- logical. if TRUE, saves data to outfolder
  ## ... 		- other parameters to datExportdata
  ##########################################################################

  ##################################################################
  ## INITIALIZE SETTINGS AND VARIABLES
  ##################################################################

  ## Set global variables
  cover_1=cover_2=use_1=use_2=chg_ag_2=change_1_2=use_1_2=use_1_2_nm=
	use_1_nm=use_2_nm=cover_1_2=cover_1_2_nm=cover_1_nm=cover_2_nm=
	use_1_2_FOR=use_1_FOR=use_2_FOR=use_1_2_FOR_nm=use_1_FOR_nm=
	use_2_FOR_nm=COLOR <- NULL
  gui <- FALSE

  ## Import ICE dots
  ice.pnt <- FIESTA::pcheck.table(ice.pntfn)

  ## Import ICE plots
  ice.plt <- FIESTA::pcheck.table(ice.pltfn)

  ## Check savedata
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder)
  }


  #########################################################################
  ## Remove points that were not observed or there was seasonal change or
  ## vegetation expansion (change_1_2 == c(0,2,3))
  ## This gives us 45 points per plot where there was observed change, 
  ## and 5 points per plots where there was no change observed.
  #########################################################################
  ice.pnt <- ice.pnt[(change_1_2 %in% c(0,2,3) & get(eval(pntid)) %in% 
	c(1,26,31,36,41)) | change_1_2 == 1,]
  n.ice.pnt <- nrow(ice.pnt)

  ## Check to make sure there are either 5 points or 45 points
  idtab <- table(ice.pnt[[plotid]])
  lt5 <- names(idtab)[idtab < 5]
  if (length(lt5) > 0) 
    stop("check data:", paste(lt5, collapse=", "))

  ## Remove all points that were uninterpretable at Time 1 or Time 2
  ## This makes sure all estimates of change add up.
  ice.pnt <- ice.pnt[cover_1 != 999 & cover_2 != 999 & use_1 != 999 & 
		use_2 != 999 & chg_ag_2 != 99,]
  if (nrow(ice.pnt) < n.ice.pnt)
    message(nrow(ice.pnt) - n.ice.pnt, 
		" uninterpretable points were removed from dataset")



  #########################################################################
  ## LOAD LUTS and merge to ice.pnt table
  #########################################################################

  ## changelut - table defining change
  ##########################################################################
  if (is.null(changelut)) {
    changelut <- FIESTA::ref_ICE_change
  } else {
    changelut <- pcheck.table(changelut, tabnm="changelut", nullcheck=TRUE, gui=gui)
  }

  ## coverlut - table defining land cover codes
  ##########################################################################
  if (is.null(coverlut)) {
    coverlut <- FIESTA::ref_ICE_cover
  } else {
    coverlut <- pcheck.table(coverlut, tabnm="coverlut", nullcheck=TRUE, gui=gui)
  }
    
  ## uselut - table defining land use codes
  ##########################################################################
  if (is.null(uselut)) {
    uselut <- FIESTA::ref_ICE_use
  } else {
    uselut <- pcheck.table(uselut, tabnm="uselut", nullcheck=TRUE, gui=gui)
  }

  ## agentlut - table defining causal agent codes
  ##########################################################################
  if (is.null(agentlut)) {
    agentlut <- FIESTA::ref_ICE_agent
  } else {
    agentlut <- pcheck.table(agentlut, tabnm="agentlut", nullcheck=TRUE, gui=gui)
  }


  ##########################################################################
  ## domlut - table defining domain codes, names, and titles
  ##########################################################################
  if (is.null(domlut)) {
    domlut <- data.table(
	DOMCODE = c("change_1_2", "change_1_2_GRP", "cover_1", "cover_2", 
		"use_1", "use_2", "chg_ag_2", "chg_ag_2_GRP", "cover_1_GRP", "cover_2_GRP", 
		"use_1_FOR", "use_2_FOR", "change_pnt"), 
	DOMNAME = c("change_1_2_nm", "change_1_2_GRP_nm", "cover_1_nm", "cover_2_nm", 
		"use_1_nm", "use_2_nm", "chg_ag_2_nm", "chg_ag_2_GRP_nm", "cover_1_GRP_nm", 
		"cover_2_GRP_nm", "use_1_FOR_nm", "use_2_FOR_nm", "change_pnt_nm"), 
	DOMTITLE = c("Change", "Change", "T1-Cover", "T2-Cover", "T1-Use", "T2-Use", 
		"Change Agent", "Change Agent-GRP", "T1-Cover-GRP", "T2-Cover-GRP", 
		"T1-Use-FOR", "T2-Use-FOR", "Change"), stringsAsFactors = FALSE)
  } else {
    domlut <- FIESTA::pcheck.table(domlut, tabnm="domlut", nullcheck=TRUE, gui=gui)
    domlutnames <- c("DOMCODE", "DOMNAME", "DOMTITLE")
    namesnot <- domlutnames[!which(domlutnames %in% names(domlut))]
    if (length(namesnot) > 0)
      stop("domlut must have column names of DOMCODE, DOMNAME, DOMTITLE")
  }


  ##########################################################################
  ## Check for missing codes and append to ice.pnt (if appendlut=TRUE)
  ##########################################################################
  colorder <- c(plotid, pntid, "change_1_2") 

  ## Check if all values exist in cover_LUT
  misscd <- unique(ice.pnt$cover_1[which(!ice.pnt$cover_1 %in% coverlut$cover)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in cover_LUT: ", addcommas(misscd)))

  if (appendluts) {
    ## cover_1
    col <- "cover_1"
    new_LUT <- copy(coverlut)
    names(new_LUT) <- sub("cover", col, names(new_LUT))
    ice.pnt <- merge(ice.pnt, new_LUT, by=col)
    if ("COLOR" %in% names(ice.pnt))
      ice.pnt[,COLOR := NULL]
      
    colorder <- c(colorder, col, names(new_LUT)[which(!names(new_LUT) %in% c(col, "COLOR"))])
    setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
      
    ## cover_2
    col <- "cover_2"
    new_LUT <- copy(coverlut)
    names(new_LUT) <- sub("cover", col, names(new_LUT))
    ice.pnt <- merge(ice.pnt, new_LUT, by=col)
    if ("COLOR" %in% names(ice.pnt))
      ice.pnt[,COLOR := NULL]
      
    colorder <- c(colorder, col, names(new_LUT)[which(!names(new_LUT) %in% c(col, "COLOR"))])
    setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ##########################################################################
  ## uselut
  ##########################################################################
 
  ## Check if all values exist in use_LUT
  misscd <- unique(ice.pnt$use_1[which(!ice.pnt$use_1 %in% uselut$use)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in use_LUT: ", addcommas(misscd)))
      
  if (appendluts) {
    ## use_1
    col <- "use_1"
    new_LUT <- copy(uselut)
    names(new_LUT) <- sub("use", col, names(new_LUT))
    ice.pnt <- merge(ice.pnt, new_LUT, by=col)
    if ("COLOR" %in% names(ice.pnt)) ice.pnt[,COLOR := NULL]
       
    colorder <- c(colorder, col, names(new_LUT)[which(!names(new_LUT) %in% c(col, "COLOR"))])
    setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))

    ## use_2
    col <- "use_2"
    new_LUT <- copy(uselut)
    names(new_LUT) <- sub("use", col, names(new_LUT))
    ice.pnt <- merge(ice.pnt, new_LUT, by=col)
    if ("COLOR" %in% names(ice.pnt)) ice.pnt[,COLOR := NULL]
       
    colorder <- c(colorder, col, names(new_LUT)[which(!names(new_LUT) %in% c(col, "COLOR"))])
    setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ##########################################################################
  ## change agent
  ##########################################################################

  ## Check if all values exist in chg_ag_2_LUT
  misscd <- unique(ice.pnt$chg_ag_2[which(!ice.pnt$chg_ag_2 %in% agentlut$chg_ag_2)])
  if (length(misscd) > 0)
    print(paste("missing attribute values in agentlut: ", paste(misscd, collapse=", "))) 
      
  if (appendluts) {
    col <- "chg_ag_2"
    ice.pnt <- merge(ice.pnt, agentlut, by=col)
    if ("COLOR" %in% names(ice.pnt)) ice.pnt[,COLOR := NULL]
      
    colorder <- c(colorder, col, names(agentlut)[which(!names(agentlut) %in% 
		c(col, "COLOR"))])
    setcolorder(ice.pnt, c(colorder, names(ice.pnt)[which(!names(ice.pnt) %in% colorder)]))
  }

  ###############################################################################
  ## DEFINE change variables (use_1_2, cover_1_2, use_1_2_FOR) 
  ###############################################################################
  if ("use_1" %in% names(ice.pnt) && "use_2" %in% names(ice.pnt)) {
    ice.pnt[, use_1_2 := paste(use_1, use_2, sep="_to_")]
    if ("use_1_nm" %in% names(ice.pnt) & "use_2_nm" %in% names(ice.pnt)) {
      ice.pnt[, use_1_2_nm := paste(use_1_nm, use_2_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("use_1_2", "use_1_2_nm", "Use_Change"))
    }
  }
  if ("cover_1" %in% names(ice.pnt) && "cover_2" %in% names(ice.pnt)) {
    ice.pnt[, cover_1_2 := paste(cover_1, cover_2, sep="_to_")]
    if ("cover_1_nm" %in% names(ice.pnt) & "cover_2_nm" %in% names(ice.pnt)) {
      ice.pnt[, cover_1_2_nm := paste(cover_1_nm, cover_2_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("cover_1_2", "cover_1_2_nm", "Cover_Change"))
    }
  }

  if ("use_1_FOR" %in% names(ice.pnt) && "use_2_FOR" %in% names(ice.pnt)) {
    if ("use_FOR_nm" %in% names(uselut))
      use_FORlut <- unique(uselut[, c("use_FOR", "use_FOR_nm")])

    ice.pnt[, use_1_2_FOR:= paste(use_1_FOR, use_2_FOR, sep="_to_")]
    if ("use_1_FOR_nm" %in% names(ice.pnt) & "use_2_FOR_nm" %in% names(ice.pnt)) {
      ice.pnt[, use_1_2_FOR_nm := paste(use_1_FOR_nm, use_2_FOR_nm, sep="_to_")]
  
      ## Add to domlut
      if (!is.null(domlut)) 
        domlut <- rbind(domlut, list("use_1_2_FOR", "use_1_2_FOR_nm", "Use_FOR_Change"))
    }
  }

   
  #################################################################################
  ## Variables change_pnt and change_pnt_nm were added to the chg_ag_2_LUT file
  ## These variables indicate whether a point has change. The change_1_2 variable 
  ## only indicates what interpretation method was used in the process, but a true
  ## indication of change must be identified by the points. The causal agent is 
  ## the best identifier of change on a point because you name a causal agent 
  ## even if you don't identify a specific change between T1/T2 LU or T1/T2 LC.
  ## An example would be seeing fire occurring where the land use does not change
  ## and the cover remains the same as well. 
  #################################################################################

  ## Set key, ordering by plotid and pntid
  setkeyv(ice.pnt, c(plotid, pntid))


  ## Save data to outfolder
  if (savedata) {
    ice.pntlutfn <- paste0(basename.NoExt(ice.pntfn), "_lut.csv")
    datExportData(ice.pnt, outfolder=outfolder, out_layer=ice.pntlutfn, ...)

    if (!is.null(ice.plt)) {
      ice.pltlutfn <- paste0(basename.NoExt(ice.pltfn), "_lut.csv")
      datExportData(ice.plt, outfolder=outfolder, out_layer=ice.pltlutfn, ...)
    }
  }

  ## Return data
  returnlst <- list(ice.pnt=ice.pnt, ice.plt=ice.plt, plotid=plotid, pntid=pntid)
  reflst <- list(changelut=changelut, coverlut=coverlut, uselut=uselut, 
				agentlut=agentlut)
  if (!is.null(use_FORlut))
    reflst$use_FORlut <- use_FORlut
  returnlst$reflst <- reflst
  returnlst$domlut <- domlut

  return(returnlst)
}

