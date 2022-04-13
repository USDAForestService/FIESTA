#' Data - Transpose point data to plot-level percent by domain.
#' 
#' Calculates percent point by plot and domain and transpose to plot level.
#' 
#' 
#' @param pnt DF/DT or comma-delimited file (*.csv). Point-level table with one
#' record per point. If NULL, aggregated point counts must be in pntcnt.
#' @param uniqueid String. Name of unique identifier of plot in pnt.
#' @param tvar String. Name of variable to transpose.
#' @param othervars String vector. Name(s) of plot-level variables to merge
#' with transposed data.
#' @return
#' 
#' \item{pltdom.pct}{ Data frame with transposed data. }
#' @author Tracey S. Frescino
#' @keywords data
#' @examples 
#' # Load necessary data from FIESTA
#' ## Point data
#' icepntfn <- system.file("extdata",
#'                         "PB_data/icepnt_utco1135.csv",
#'                          package = "FIESTA")
#' icepnt <- read.csv(icepntfn)
#' 
#' # Convert to percent
#' percent <- datPBpnt2pct(pnt = icepnt,
#'                         uniqueid = "plot_id",
#'                         tvar = "cover_1")
#' head(percent)
#' @export datPBpnt2pct
datPBpnt2pct <- function(pnt, uniqueid, tvar, othervars=NULL) {
  ## DESCRIPTION: calculates percent of points by tvar and plot and 
  ##	transpose to plot level. 

  ## Set global variables
  PtsPerPlot=p.pltdom=nbrpts.pltdom <- NULL
  gui <- FALSE


  ###################################################################################
  ## Check inputs
  ###################################################################################
  pntx <- pcheck.table(pnt, gui=gui, tabnm="pnt", caption="point table?", 
		nullcheck=TRUE, returnsf=FALSE)

  pntvars <- names(pntx)
  uniqueid <- pcheck.varchar(var2check=uniqueid, varnm="uniqueid", gui=gui, 
		checklst=pntvars, caption="UniqueID variable - pnt", 
		warn=paste(uniqueid, "not in in table"), stopifnull=TRUE)

  pntvars <- pntvars[pntvars != uniqueid]
  tvar <- pcheck.varchar(var2check=tvar, varnm="tvar", gui=gui, 
		checklst=pntvars, caption="Point domain category", 
		warn=paste(tvar, "not in in table"), stopifnull=TRUE)

  pntvars <- pntvars[pntvars != tvar]
  othervars <- pcheck.varchar(var2check=othervars, varnm="othervars", gui=gui, 
		checklst=pntvars, caption="Other variables to keep", multiple=TRUE)

  if (nrow(pntx[, othervars, with=FALSE]) > length(unique(pntx[[uniqueid]]))) 
      stop("othervars must be plot level")


  ###################################################################################
  ## Do Work
  ###################################################################################

  ## Number of points per plot
  PtsPerPlot <- pntx[, list(PtsPerPlot=.N), by=uniqueid]
  setkeyv(PtsPerPlot, uniqueid)

  ## Number of points by plot and tvar
  pltdom <- pntx[, list(nbrpts.pltdom=.N), by=c(uniqueid, tvar)]
  setkeyv(pltdom, uniqueid)

  ## Merge number of points per plot to pltdom
  pltdom <- pltdom[PtsPerPlot]

  ## Proportion of points per plot for tvar (nbrpts.pltdom)
  pltdom[, p.pltdom := nbrpts.pltdom / PtsPerPlot * 100]
  pltdom[, (uniqueid) := as.character(get(uniqueid))]
  setkeyv(pltdom, uniqueid)

  ## Transpose rows to columns
  tpltdom <- transpose2col(pltdom, uniqueid, tvar, "p.pltdom")

  ## Merge with other variables in pnt
  if (!is.null(othervars)) {

    ## Check if class of puniqueid in pltx matches class of puniqueid in condx
    tabs <- check.matchclass(pntx, tpltdom, uniqueid)
    pntx <- tabs$tab1
    tpltdom <- tabs$tab2

    pltdom.pct <- merge(unique(pntx[, unique(c(uniqueid, othervars)), with=FALSE]),
		tpltdom, by=uniqueid)
  } else {
    pltdom.pct <- tpltdom
  }
  
  setDF(pltdom.pct)
  return(pltdom.pct)
}
