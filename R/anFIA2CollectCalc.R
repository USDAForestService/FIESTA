anFIA2CollectCalc <- function(pltstrat, cond, tree, seed, unitarea, strlut, 
	level_1_code, level_1_label=NULL, level_2_code=NULL, level_2_label=NULL,
	savedata=FALSE) {


  ###############################################################
  ## DESCRIPTION: 
  ## Translate FIA data to Open Foris Collect/Calc data sets
  ###############################################################

  ## Set global variables
  STRATUM_DESCR=P1POINTCNT=strwt=V1=AREA <- NULL

  collectdat <- list()
  calcdat <- list()


  ## Define variables
  level1.vars.fia <- c(level_1_code, level_1_label)
  level1.vars.calc <- c("level_1_code")
  if (!is.null(level_1_label)) 
    level1.vars.calc <- c("level_1_code", "level_1_label")

  level2.vars.fia <- c(level_2_code, level_2_label)
  if (!is.null(level_2_label)) 
    level2.vars.calc <- c("level_2_code", "level_2_label")

  areavar.fia <- "ACRES"
  areavar.calc <- "level_2_area"
  strwtvar <- "stratum_area"


  #####################################################################################
  ## Compose calc-aois data - CALC  (unitarea)
  #####################################################################################

  ## level 2 - by county
  calc.aois.level2 <- aggregate(unitarea[[areavar.fia]], 
			unitarea[, c(level1.vars.fia, level2.vars.fia)], sum)
  names(calc.aois.level2) <- c(level1.vars.calc, level2.vars.calc, areavar.calc)
  print(head(calc.aois.level2))

  calcdat$calc.aois.level2 <- setDF(calc.aois.level2)
  if (savedata)
    write.csv(calc.aois.level2, "calc/calc-aois_level2.csv", row.names=FALSE)

  ## level 1 - by state
  calc.aois.level1 <- aggregate(unitarea[[areavar.fia]], unitarea[, level1.vars.fia], sum)
  names(calc.aois.level1) <- c(level1.vars.calc, areavar.calc)
  print(head(calc.aois.level1))

  calcdat$calc.aois.level1 <- setDF(calc.aois.level1)
  if (savedata)
    write.csv(calc.aois.level1, "calc/calc-aois_level1.csv", row.names=FALSE)


  #####################################################################################
  ## Compose calc-stratum data (stratalut) 
  ## - level1 (by state) and level2 (by county)
  #####################################################################################
  strlut <- setDT(merge(strlut, 
	unitarea[, c("ESTN_UNIT", "ESTN_UNIT_DESCR", "STATE_NAME", "ACRES")], 
	by="ESTN_UNIT"))
  strlut[strlut$STRATUMCD == 2, STRATUM_DESCR := "Brown"]


  ## level1 (by state)
  ####################################
  stratum.level1.vars.fia <- c(level1.vars.fia, "STRATUMCD", "STRATUM_DESCR")
  stratum.level1.vars.calc <- c(level1.vars.calc, "stratum_code", "stratum_label")

  calc.stratum.level1 <- strlut[, sum(P1POINTCNT), by=stratum.level1.vars.fia]
  calc.stratum.level1[, strwt:=prop.table(V1)]
  setnames(calc.stratum.level1, "V1", "P1POINTCNT")
  calc.stratum.level1[, AREA := strwt * sum(unitarea[[areavar.fia]])]

  calc.stratum.level1 <- calc.stratum.level1[, c(stratum.level1.vars.fia, "AREA"), with=FALSE]
  names(calc.stratum.level1) <- c(stratum.level1.vars.calc, strwtvar)
  print(calc.stratum.level1)

  calcdat$calc.stratum.level1 <- setDF(calc.stratum.level1)
  if (savedata)
    write.csv(calc.stratum.level1, "calc/calc-stratum_level1.csv", row.names=FALSE)


  ## level2 (by county)
  ####################################
  stratum.level2.vars.fia <- c(level1.vars.fia, level2.vars.fia, "STRATUMCD", "STRATUM_DESCR")
  stratum.level2.vars.calc <- c(level1.vars.calc, level2.vars.calc, "stratum_code", "stratum_label")

  calc.stratum.level2 <- strlut[, sum(P1POINTCNT), by=stratum.level2.vars.fia]
  calc.stratum.level2[, strwt:=prop.table(V1), by=level2.vars.fia]
  setnames(calc.stratum.level2, "V1", "P1POINTCNT")
  calc.stratum.level2 <- merge(calc.stratum.level2, 
		unitarea[, c(level2.vars.fia, areavar.fia)], by=level2.vars.fia)
  calc.stratum.level2[, (strwtvar) := strwt * get(areavar.fia)]
  calc.stratum.level2 <- calc.stratum.level2[, c(stratum.level2.vars.fia, strwtvar), with=FALSE]
  names(calc.stratum.level2) <- c(stratum.level2.vars.calc, strwtvar)
  print(head(calc.stratum.level2))

  calcdat$calc.stratum.level2 <- setDF(calc.stratum.level2)
  if (savedata)
    write.csv(calc.stratum.level2, "calc/calc-stratum_level2.csv", row.names=FALSE)



  #####################################################################################
  ## Create Sample Design data for Sampling point data - COLLECT
  #####################################################################################
  ## level1 - plt
  ## level2 - subplot
  ## x - LON_PUBLIC		NOTE: only 1 coordinate for cluster - center plot
  ## y - LAT_PUBLIC		NOTE: only 1 coordinate for cluster - center plot
  sampdesign <- unique(pltstrat[, c("STATECD", "ESTN_UNIT", "CN", "LON_PUBLIC", "LAT_PUBLIC")])
  names(sampdesign) <- c("level1_code", "level2_code", "level3_code", "x", "y")
  sampdesign$srs_id <- "EPSG:4326"
  print(head(sampdesign))

  collectdat$sampdesign <- setDF(sampdesign)
  if (savedata)
    write.csv(sampdesign, "tables/sampdesign.csv", row.names=FALSE)

 
  ## Create cluster table for stand table - COLLECT
  ####################################################################
  ## NOTE: cluster names must be lowercase
  puniqueid.fia <- "CN"
  puniqueid.calc <- "cn"

  pltvars <- c("UNITCD", "STATECD", "COUNTYCD", "INVYR", "MEASYEAR", 
		"PLOT_STATUS_CD", "ESTN_UNIT", "STRATUMCD")
  plt <- pltstrat[, c(puniqueid.fia, pltvars)]

  pltnames <- pltvars
  names(plt) <- c(puniqueid.calc, tolower(pltnames))
  print(head(plt))

  collectdat$plot <- setDF(plt)
  if (savedata)
    write.csv(plt, "tables/plot.csv", row.names=FALSE)

  ## Create stand table for stand table - COLLECT
  ####################################################################
  puniqueid.calc2 <- "plot_cn"
  cuniqueid.fia <- c("PLT_CN", "CONDID")
  cuniqueid.calc <- c(puniqueid.calc2, "condid")

  condvars <- c("CONDPROP_UNADJ", "SUBPPROP_UNADJ", "MICRPROP_UNADJ", 
	"COND_STATUS_CD", "OWNGRPCD", "FORTYPCD", "STDSZCD", 
	"RESERVCD", "ADFORCD")
  condnames <- condvars

  cond <- cond[, c(cuniqueid.fia, condvars)]
  names(cond) <- c(cuniqueid.calc, tolower(condnames))

  domnames <- tolower(c("FORTYPCD", "OWNGRPCD", "STDSZCD", "RESERVCD", "ADFORCD"))
  cond[is.na(cond)] <- -1
  cond <- cond[cond[[puniqueid.calc2]] %in% plt[[puniqueid.calc]],]
  print(head(cond))

  collectdat$cond <- setDF(cond)
  if (savedata)
    write.csv(cond, "tables/cond.csv", row.names=FALSE)

  ## Create tree data for tree table - COLLECT
  ####################################################################
  tuniqueid.fia <- c("PLT_CN", "CONDID")
  tuniqueid.calc <- c(puniqueid.calc2, "cond_condid")

  tree$tree_no <- paste(tree$SUBP, tree$TREE, sep="_")
  treevars <- c("tree_no", "STATUSCD", "SPCD", "DIA", "HT", 
			"VOLCFNET", "TPA_UNADJ", "BA", "DRYBIO_AG")
  tree <- tree[, c(tuniqueid.fia, treevars)]
  names(tree) <- c(tuniqueid.calc, tolower(treevars))
  print(head(tree))

  collectdat$tree <- setDF(tree)
  if (savedata) 
    write.csv(tree, "tables/tree.csv", row.names=FALSE)

  ## Create seed data for seed table - COLLECT
  ####################################################################
  seed <- seed[order(seed$PLT_CN, seed$SUBP), ]
  seed$SEED <- sequence(tabulate(as.factor(paste(seed$PLT_CN, seed$SUBP))))
  seed$seed_no <- paste(seed$SUBP, seed$SEED, sep="_")
  seedvars <- c("seed_no", "SPCD", "TREECOUNT_CALC", "TOTAGE", "TPA_UNADJ")
  seed <- seed[, c(tuniqueid.fia, seedvars)]
  names(seed) <- c(tuniqueid.calc, tolower(seedvars))
  print(head(seed))

  collectdat$seed <- setDF(seed)
  if (savedata)
    write.csv(seed, "tables/seed.csv", row.names=FALSE)

  return(list(collectdat=collectdat, calcdat=calcdat))
}

