## PBgetest			Calculate estimation variables.
## getpltdom.prop	Calculate proportion of points by domain
## getgainloss		Calculate gain/loss based on all rowvar/colvar values
## transpose2row		Transpose data.table columns to rows
## transpose2col		Transpose data.table rows to columns
## bp_wrap.it
## bp_width.jpg


#########################################################################
## DEFINE FUNCTIONS
#########################################################################

PBgetest <- function(xdat, areavar=NULL, tabtype="AREA", phatcol="phat", phatcol.var="phat.var"){

  ########################################################################################
  ## DESCRIPTION: Calculates the following estimation variables
  ## if tabtype = AREA:
  ## est		- estimated acres of land { phat*areavar }
  ## est.var	- variance of estimated acres of land { phat.var*areavar }
  ## est.se	- standard error of estimated acres of land { sqrt(est.var) }
  ## est.cv	- coefficient of variance of estimated acres of land { est.se/est }
  ## pse		- percent sampling error { est.cv*100 }
  ## if tabtype = PCT:
  ## est		- estimated percent cover of land { phat*100 }
  ## est.var	- variance of estimated percent cover of land { phat.var*100^2 }
  ## est.se	- standard error of estimated percent cover of land { sqrt(est.var) }
  ## est.cv	- coefficient of variance of estimated percent cover of land { est.se/est }
  ## pse		- percent sampling error { est.cv*100 }
  ########################################################################################

  ## Set global variables
  est=est.var=est.se=est.cv=pse <- NULL

  if (tabtype == "AREA" && !is.null(areavar)) {

    ## Estimated acres
    xdat[,	est := get(phatcol) * get(areavar)][,
		est.var := get(phatcol.var) * get(areavar)^2]
  } else {
    ## Estimated percent cover
    xdat[,	est := get(phatcol) * 100][,
		est.var := get(phatcol.var) * 100^2]
  }

  ## Calculate standard error (se), coefficient of variation (cv), and
  ##	percent sampling error (pse). for numerator
  suppressWarnings(
  xdat[,	est.se := sqrt(est.var)][,
		est.cv := est.se/est][,
		pse := est.cv*100] )

  ## Change all na values to 0 values
  xdat[is.na(xdat)] <- 0

  return(xdat)
}


getpltdom.prop <- function(pntall, uniqueid, domain, strunitvars=NULL) {
  ## DESCRIPTION: calculates proportion of points by domain, plot,
  ##	and strata/estimation unit

  if (!any(class(pntall) %in% "data.table")) pntall <- setDT(pntall)

  ## Set global variables
  PtsPerPlot=p.pltdom=nbrpts.pltdom <- NULL

  ## NUMBER OF POINTS PER PLOT
  PtsPerPlot <- pntall[, list(PtsPerPlot=.N), by=uniqueid]
  setkeyv(PtsPerPlot, uniqueid)

  ## STRATA/PLOT/DOMAIN LEVEL:
  ## Number of points by strata/plot/domain
  pltdom.prop <- pntall[, list(nbrpts.pltdom=.N), by=c(strunitvars, uniqueid, domain)]
  setkeyv(pltdom.prop, uniqueid)

  ## Append number of points per plot to pltdom.prop
  pltdom.prop <- pltdom.prop[PtsPerPlot]

  ## Proportion of points per plot for domain (nbrpts.pltdom)
  pltdom.prop[, p.pltdom := nbrpts.pltdom / PtsPerPlot]
  pltdom.prop[, (uniqueid) := as.character(get(uniqueid))]
  setkeyv(pltdom.prop, uniqueid)

  return(pltdom.prop)
}

getgainloss <- function(val, pltdom.prop, plotid, rowvar, colvar, strlut,
	unitvar, strvar, tabtype="PCT", areavar=NULL, unitacres=NULL, sumunits=FALSE,
	value.var="p.pltdom") {
  ## DESCRIPTION: calculates gain/loss based on all rowvar/colvar values or values
  ## 			input by user (gainloss.vals)

  ## INITIALIZE SETTINGS AND VARIABLES
  .=var=n=cov=diff.phat.str=strwt=diff.phat.var.str=diff.phat=diff.phat.var=dif=
	gain.phat.str=loss.phat.str=gain.phat.var.str=n.strasta=loss.phat.var.str=
	gainloss.covar.str=gain.phat=loss.phat=gain.phat.var=loss.phat.var=n.strata=
	TOTACRES=WEIGHT <- NULL


  strunitvars <- c(unitvar, strvar)

  ## Create a copy of table of plot/domain - level proportions
  pltdom.val <- copy(pltdom.prop)
  pltdom.val[[rowvar]] <- as.character(pltdom.val[[rowvar]])
  pltdom.val[[colvar]] <- as.character(pltdom.val[[colvar]])

  ## Create binary columns for each value (equal to value or not equal to value)
  pltdom.val[get(eval(rowvar)) != val, (rowvar) := paste("Not", val, sep="-")]
  pltdom.val[get(eval(colvar)) != val, (colvar) := paste("Not", val, sep="-")]

  ## Define gain and loss column names
  #gainnm <- paste(paste("Not", val, sep="-"), val)
  #lossnm <- paste(val, paste("Not", val, sep="-"))

  gainnm <- paste(paste("Not", val, sep="-"), val, sep=" to ")
  lossnm <- paste(val, paste("Not", val, sep="-"), sep=" to ")


  ## Transpose table of plot/domain-level proportions to plot-level data
#  plt.val <- dcast(pltdom.val,
#		get(eval(unitvar)) + get(eval(strvar)) + get(eval(plotid)) ~
#		paste(get(eval(rowvar)), get(eval(colvar)), sep=" "),
#			fun.aggregate = sum, value.var=value.var)
#  setnames(plt.val, c("unitvar", "strvar", "plotid"), c(unitvar, strvar, plotid))

  plt.val <- data.table::dcast(pltdom.val,
		get(eval(unitvar)) + get(eval(strvar)) + get(eval(plotid)) ~
		paste(get(eval(rowvar)), get(eval(colvar)), sep=" to "),
			fun.aggregate = sum, value.var=value.var)
  setnames(plt.val, c("unitvar", "strvar", "plotid"), c(unitvar, strvar, plotid))

  ## Add columns with 0 values if they do not exist.
  if (!gainnm %in% names(plt.val)) {
    plt.val[, (gainnm) := 0]
  }
  if (!lossnm %in% names(plt.val)) {
    plt.val[, (lossnm) := 0]
  }

  ## Calculate estimate proportions, variance, and covariance of the gain and loss
  ## variable by strata (estimation unit)
  gainloss.bystr <- plt.val[, .(
	gain.phat.str = mean(get(gainnm)),
	loss.phat.str = mean(get(lossnm)),
	gain.phat.var.str = var(get(gainnm)),
	loss.phat.var.str = var(get(lossnm)),
	gainloss.covar.str = cov(get(gainnm), get(lossnm))), by=strunitvars]
  setkeyv(gainloss.bystr, strunitvars)

  ## Merge to strata table
  strlut <- setDT(strlut)
  setkeyv(strlut, strunitvars)
  gainloss.bystr <- gainloss.bystr[strlut]

  ## Calculate difference of gain and loss estimate proportions and the variance
  ## of this estimate.
  val.bystr <- gainloss.bystr[, ':=' (
	gain.phat.var.str = gain.phat.var.str/n.strata,
	loss.phat.var.str = loss.phat.var.str/n.strata,
     	gainloss.covar.str = gainloss.covar.str/n.strata,
	diff.phat.str = gain.phat.str - loss.phat.str)][,
	diff.phat.var.str := (gain.phat.var.str + loss.phat.var.str) -
				(2 * gainloss.covar.str )]
  setkeyv(val.bystr, strunitvars)

  ## Multiply estimate by strata weight and variance by strata weight squared
  val.bystr[,  ':=' (
	gain.phat = gain.phat.str * strwt,
 	gain.phat.var = gain.phat.var.str * strwt ^ 2,
	loss.phat = loss.phat.str * strwt,
 	loss.phat.var = loss.phat.var.str * strwt ^ 2,
	diff.phat = diff.phat.str * strwt,
	diff.phat.var = diff.phat.var.str * strwt ^ 2)]

  ## Sum  estimate and variance for stratas
  val.byunit <- val.bystr[, lapply(.SD, sum), by=unitvar,
	.SDcols=c("gain.phat", "gain.phat.var", "loss.phat", "loss.phat.var",
	"diff.phat", "diff.phat.var")]


  if (tabtype == "AREA" || sumunits) {
    tabs <- check.matchclass(unitacres, val.byunit, unitvar)
    unitacres <- tabs$tab1
    val.byunit <- tabs$tab2
    setkeyv(val.byunit, unitvar)
    val.byunit <- val.byunit[unitacres, nomatch=0]

    if (sumunits) {
      val.byunit[, TOTACRES := sum(unique(val.byunit[, c(unitvar, areavar),
		with=FALSE])[[areavar]])][, WEIGHT := get(areavar) / TOTACRES]
      val.grp <- val.byunit[, list(unitvar="TOTAL",
			gain.phat=sum(gain.phat * WEIGHT),
			gain.phat.var=sum(gain.phat.var * WEIGHT^2),
			loss.phat=sum(loss.phat * WEIGHT),
			loss.phat.var=sum(loss.phat.var * WEIGHT^2),
			diff.phat=sum(diff.phat * WEIGHT),
			diff.phat.var=sum(diff.phat.var * WEIGHT^2))]
      setnames(val.grp, "unitvar", unitvar)
    } else {
      val.grp <- val.byunit
    }

    #est.variables <- c("est", "est.var", "est.se", "est.cv", "pse")
    #val.grp <- PBgetest(val.grp, areavar=areavar, tabtype=tabtype,
	#			phat="gain.phat", phat.var="gain.phat.var")
    #setnames(val.grp, est.variables, paste("gain", est.variables, sep="."))
    #val.grp <- PBgetest(val.grp, areavar=areavar, tabtype=tabtype,
	#			phat="loss.phat", phat.var="loss.phat.var")
    #setnames(val.grp, est.variables, paste("loss", est.variables, sep="."))

  } else {

    val.grp <- copy(val.byunit)
  }

  val.grp <- val.grp[, ':=' (gain.val = gainnm, loss.val = lossnm,
	gain.est = gain.phat * 100, gain.se = sqrt(gain.phat.var) * 100,
	loss.est = loss.phat * 100, loss.se = sqrt(loss.phat.var) * 100,
	diff.est = diff.phat * 100, diff.se = sqrt(diff.phat.var) * 100)][,
	':=' (gain.phat = NULL, gain.phat.var=NULL, loss.phat=NULL,
	loss.phat.var=NULL, diff.phat=NULL, diff.phat.var=NULL)]


  gainlossvars <- c("gain.est", "gain.se", "loss.est", "loss.se", "diff.est", "diff.se")
  setcolorder(val.grp, c(unitvar, "gain.val", "loss.val", gainlossvars))

  #returnlst <- list(est <- val.grp)
  #if (sumunits) returnlst$est.unit <- val.byunit
  returnlst <- val.grp

  return(returnlst)
}

transpose2row <- function(x, uniqueid, tvars=NULL, na.rm=TRUE, tnewname=NULL,
	tvalue=NULL, returnfactor=FALSE) {
  ## DESCRIPTION: transpose data.table variables from columns to row
  ## ARGUMENTS:
  ## x 		DT. Data.table to transpose
  ## uniqueid	String vector. Name(s) of unique identifer for table
  ## tvars		String vector. Names of data.table columns to transpose
  ## na.rm		Logical. If TRUE, removes NA values after transpose

  xt <- melt(x, id.vars=uniqueid, measure.vars=tvars, na.rm=na.rm)
  if (is.data.table(xt)) setkeyv(xt, uniqueid)

  if (!is.null(tnewname))
    setnames(xt, "variable", tnewname)
  if (!is.null(tvalue))
    setnames(xt, "value", tvalue)

  if (!returnfactor) {
    xt[["variable"]] <- as.character(xt[["variable"]])
  }

  return(xt)
}

transpose2col <- function(x, uniqueid, tvar, value.var, fill=0, fun.aggregate=sum) {
  ## DESCRIPTION: transpose data.table variables from columns to row
  ## ARGUMENTS:
  ## x 		DT. Data.table to transpose
  ## uniqueid	String vector. Name(s) of unique identifer for table
  ## tvar		String vector. Names of data.table columns to transpose
  ## na.rm		Logical. If TRUE, removes NA values after transpose

  dcast.formula <- sprintf("%s ~ %s", uniqueid, tvar)
  xt <- dcast(x, formula=dcast.formula, value.var=value.var, fill=fill,
		fun.aggregate=fun.aggregate)
  if (is.data.table(xt)) setkeyv(xt, uniqueid)

  return(xt)
}

bp_wrap.it <- function(x, len){
  ## DESCRIPTION: wraps text of y label on barplot
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
	USE.NAMES = FALSE) }

bp_width.jpg <- function(x){
  ## DESCRIPTION: gets width of jpg image, corresponding to number of categories
  if(length(x) < 8){
    ifelse(max(unlist(lapply(strsplit(names(x), " "), nchar))) < 20, 10, 11)
  }else{
    ifelse(max(unlist(lapply(strsplit(names(x), " "), nchar))) < 20, 12, 14)
  }
}


