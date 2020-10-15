datPlotcnt <- function(plt, yrtype="INVYR", states=NULL, designcd=FALSE, forsamp=TRUE, 
	total=TRUE, subtotal=TRUE, savedata=FALSE, outfolder=NULL, outfn=NULL, gui=FALSE){

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- ifelse(nargs() == 0, TRUE, FALSE)

  ## Set global variables  
  FORNONSAMP2=subtotalcol <- NULL


  ########################################################################
  ### GET PARAMETERS 
  ########################################################################

  ## Check plt table
  pltx <- FIESTA::pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")
  if (is.null(pltx) || nrow(pltx) == 0) 
    stop("must have plt table")

  ## Check yrtype
  yrtype <- FIESTA::pcheck.varchar(var2check=yrtype, varnm="yrtype", gui=gui, 
		checklst=c("INVYR", "MEASYEAR"), caption="Year type?")

  ## Check states
  if (is.null(states))
    if ("STATECD" %in% names(pltx)) states <- unique(pltx[["STATECD"]])    
  stabbr <- FIESTA::pcheck.states(states, statereturn="ABBR")

  ## Check designcd
  designcd <- FIESTA::pcheck.logical(designcd, varnm="designcd", 
	title="Include designcd?", first="NO", gui=gui)


  ## Check total
  ###########################################################
  total <- FIESTA::pcheck.logical(total, varnm="total", 
		title="Include totals?", first="YES", gui=gui)

  ## Check subtotal
  ###########################################################
  subtotal <- FIESTA::pcheck.logical(subtotal, varnm="subtotal", 
		title="Include subtotals?", first="YES", gui=gui)

  ## Check savedata
  ###########################################################
  savedata <- FIESTA::pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables to outfolder?", first="YES", gui=gui)

  ## Check outfolder
  ###########################################################
  if (savedata) {
    outfolder <- FIESTA::pcheck.outfolder(outfolder, gui=gui)
    if (is.null(outfn) || gsub(" ", "", outfn) == "") outfn <- "pltcnt"
  }

  ###  GET PLOT AND CONDITION COUNTS  
  ######################################################################
  vars <- {}
  if ("STATECD" %in% names(pltx)) vars <- c(vars, "STATECD")
  #if ("CYCLE" %in% names(pltx)) vars <- c(vars, "CYCLE")
  subtotalcol <- c(subtotalcol, "STATECD", yrtype) 

  vars <- c(vars, yrtype)
  if (designcd && "DESIGNCD" %in% names(pltx)) {
    if ("DESIGNCD" %in% names(pltx)) vars <- c(vars, "DESIGNCD")
    subtotalcol <- c(subtotalcol, "DESIGNCD") 
  } else {
    designcd <- FALSE
  }
  if (forsamp && "FORNONSAMP" %in% names(pltx)) {
    pltx$FORNONSAMP2 <- as.character(pltx[["FORNONSAMP"]])
    pltx[!pltx$FORNONSAMP %in% c("Sampled-Forest", "Sampled-Nonforest"), 
		FORNONSAMP2 := "Nonsampled"]
    vars <- c(vars, "FORNONSAMP2")
  }


  ## Add State abbreviations
  pltx <- merge(FIESTA::ref_statecd[, c("VALUE", "ABBR")], pltx, 
			by.x="VALUE", by.y="STATECD")
  setnames(pltx, c("VALUE", "ABBR"), c("STATECD", "STABBR"))
  vars <- c("STABBR", vars)
#  setkeyv(pltx, vars)


  pltcnt <- datFreq(x=pltx, xvar=vars, total=total, subtotal=subtotal, 
	subtotalcol=subtotalcol)
  pltcnt <- pltcnt[pltcnt$FREQ != 0,]

  names(pltcnt)[names(pltcnt) == "FREQ"] <- "NBRPLTS"
  names(pltcnt)[names(pltcnt) == "FORNONSAMP2"] <- "PLOT_STATUS"

  #pltcnt <- pltcnt[order(pltcnt[[yrtype]]),] 
    
  #if(designcd){  
  #  if("CYCLE" %in% vars){
  #    pltcnt <- pltcnt[order(pltcnt$CYCLE, pltcnt[,yrtype], pltcnt$DESIGNCD),]
  #  }else{
  #    pltcnt <- pltcnt[order(pltcnt[,yrtype], pltcnt$DESIGNCD),]
  #  }
  #}else{
  #  if("CYCLE" %in% vars){
  #    pltcnt <- pltcnt[order(pltcnt$CYCLE, pltcnt[,yrtype]),]
  #  }else{
  #    pltcnt <- pltcnt[order(pltcnt[,yrtype]),]
  #  }
  #}
  return(pltcnt)
}
