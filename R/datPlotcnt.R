#' Database - Get plot counts.
#' 
#' Extract plot counts by inventory year and state.
#' 
#' 
#' @param plt Data frame. Table of plot-level variables to count plots. If
#' using this table, it must include INVYR.
#' @param yrtype String. Type of year to categorize data ("INVYR", "MEASYEAR").
#' @param states String vector.  The states in plt table.
#' @param designcd Logical. If TRUE, includes FIA design codes in the table.
#' @param forsamp Logical. If TRUE, includes forest/nonforest/nonsampled codes
#' in the table.
#' @param total Logical. If TRUE, a row is added to bottom of table with a
#' total for the whole table.
#' @param subtotal Logical. If TRUE, a row is added to bottom of each section
#' for subtotals.
#' @param savedata Logical. If TRUE, saves data to outfolder as comma-delimited
#' file (*.csv).  No objects are returned. If FALSE, the data are saved as R
#' objects and returned to user.  See details for caveats.
#' @param outfolder String. The output folder path. If NULL and savedata=TRUE
#' or parameters=TRUE, outfolder is the working directory.
#' @param outfn String. The name of the output file. If NULL, defaults to
#' pltcnt_'date'.csv
#' @param gui Logical. If TRUE, gui windows pop up for parameter selection.
#' @return pltcnt - a dataframe of counts (YEAR, STABBR, STCD, PLOTS,
#' NONSAMPLED, FOREST, NONFOREST)
#' @author Tracey S. Frescino
#' @keywords data
#' @export datPlotcnt
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
  pltx <- pcheck.table(plt, gui=gui, tabnm="plt", caption="Plot table?")
  if (is.null(pltx) || nrow(pltx) == 0) 
    stop("must have plt table")

  ## Check yrtype
  yrtype <- pcheck.varchar(var2check=yrtype, varnm="yrtype", gui=gui, 
		checklst=c("INVYR", "MEASYEAR"), caption="Year type?")

  ## Check states
  if (is.null(states))
    if ("STATECD" %in% names(pltx)) states <- unique(pltx[["STATECD"]])    
  stabbr <- pcheck.states(states, statereturn="ABBR")

  ## Check designcd
  designcd <- pcheck.logical(designcd, varnm="designcd", 
	title="Include designcd?", first="NO", gui=gui)


  ## Check total
  ###########################################################
  total <- pcheck.logical(total, varnm="total", 
		title="Include totals?", first="YES", gui=gui)

  ## Check subtotal
  ###########################################################
  subtotal <- pcheck.logical(subtotal, varnm="subtotal", 
		title="Include subtotals?", first="YES", gui=gui)

  ## Check savedata
  ###########################################################
  savedata <- pcheck.logical(savedata, varnm="savedata", 
		title="Save data tables to outfolder?", first="YES", gui=gui)

  ## Check outfolder
  ###########################################################
  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui=gui)
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
  pltx <- merge(FIESTAutils::ref_statecd[, c("VALUE", "ABBR")], pltx, 
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
