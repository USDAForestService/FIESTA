dbgettable <- function(dbtable_layer, 
                       indb, 
                       datsource, dbtablst, 
                       dbconn, schema,
                       stabbr,
                       pltx,
                       pfromqry,
                       puniqueid,
                       defaultVars,
                       defaultvarlst = NULL,
                       evalFilter,
                       stateFilters,
                       lowernames = FALSE) {
  
  ## DESCRIPTION: get database table from datamart, a database, csv file, or R ojbect
  
  PLT_CN <- NULL
  prefix <- "tab"
  prefix. <- paste0(prefix, ".")
  SCHEMA. <- ""
  if (!is.null(schema)) SCHEMA. <- paste0(schema, ".")
  
  if (indb) {
    dbtablenm <- chkdbtab(dbtablst, dbtable_layer)
    if (is.null(dbtablenm)) {
      message("there is no ", dbtablenm, " SUBPLOT table in database")
      return(NULL)
    } else {
      ## Get SUBPLOT fields
      dbtableflds <- dbgetflds(conn = dbconn, schema = schema, tabnm = dbtablenm, upper = TRUE)
    }
    
  } else {
    dbtablenm <- toupper(dbtable_layer)
    
    if (datsource == "datamart") {
      DBTABLE <- FIESTA::DBgetCSV(dbtable_layer, stabbr, returnDT=TRUE, stopifnull=FALSE)
    } else {
      DBTABLE <- pcheck.table(dbtable_layer, stopifnull = TRUE, stopifinvalid = TRUE)
    }
    
    if (is.null(DBTABLE)) {
      message("there is no SUBPLOT table in datamart")
      return(NULL)
    } else {
      dbtablenm <- "DBTABLE"
      names(DBTABLE) <- toupper(names(DBTABLE))
      dbtableflds <- names(DBTABLE)
    }
  }
  
  if (!is.null(dbtablenm)) {
    
    ## Check variables in database
    if (defaultVars && !is.null(defaultvarlst)) {
      defaultvarlst <- defaultvarlst[defaultvarlst %in% dbtableflds]
      
      ## Add commas
      dbtabvars <- toString(paste0(prefix., defaultvarlst))
    } else {
      dbtabvars <- paste0(prefix., "*")
    }
    
    ## Create query for SUBPLOT
    dbtabfromqry <- paste0(pfromqry, 
                           " \nJOIN ", SCHEMA., dbtablenm, " tab ON tab.PLT_CN = p.", puniqueid)
    
    dbtab.qry <- paste0("SELECT DISTINCT ", dbtabvars,
                        "\nFROM ", dbtabfromqry,
                        "\nWHERE ", paste0(evalFilter, stateFilters))
    
    ## Query SQLite database or R object
    if (indb) {
      dbtabx <- tryCatch(
        DBI::dbGetQuery(dbconn, dbtab.qry),
        error=function(e) {
          return(NULL) })
    } else {
      dbtabx <- tryCatch(
        sqldf::sqldf(dbtab.qry, stringsAsFactors=FALSE, connection = NULL),
        error=function(e) {
          return(NULL) })
    }
    
    if (is.null(dbtabx) || nrow(dbtabx) == 0) {
      message(dbtablenm, " query is invalid")
      message(dbtab.qry)
      
    } else {
      names(dbtabx) <- toupper(names(dbtabx))
      
      dbtabx <- data.table::setDT(dbtabx)
      
      if ("PLT_CN" %in% names(dbtabx)) {
        dbtabx[, PLT_CN := as.character(PLT_CN)]
        setkey(dbtabx, "PLT_CN")
        
        ## Subset overall filters from condx
        dbtabx <- dbtabx[dbtabx$PLT_CN %in% pltx$CN, ]
      }
      
      if (lowernames) {
        names(dbtabx) <- tolower(names(dbtabx))
      }
    }
  }
  returnlst <- list(dbtabx = dbtabx, dbtab.qry = dbtab.qry)
  
  if (datsource == "datamart") {
    if (exists("DBTABLE")) rm(DBTABLE)
  }
  
  return(returnlst)
}

