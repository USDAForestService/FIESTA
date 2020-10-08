write2sqlite <- function(tab, SQLitefn, out_name=NULL, gpkg=FALSE, 
 	outfolder=NULL, overwrite=FALSE, appendtab=FALSE, createnew=FALSE,
	dbconnopen=FALSE, index.unique=NULL, index=NULL){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##  
  ## ARGUMENTS: 
  ##  tab    DF. The table to output.
  ##  out_name	String. Name of output layer.
  ##  outfn.date	Adds a date to the end of the file name
  ##  index.unique	String. Unique index
  ##  index			String. Non-unique index 
  ##
  ## VALUE:
  ##  Writes data frame to SQLite database.
  ####################################################################################

  ## Check SQLite connection
  ###########################################################
  if (createnew) {
    dbconn <- DBcreateSQLite(SQLitefn=SQLitefn, outfolder=outfolder, dbconnopen=TRUE, 
		gpkg=gpkg, overwrite=overwrite)
  } else {
    dbconn <- DBtestSQLite(SQLitefn=SQLitefn, outfolder=outfolder, dbconnopen=TRUE, 
		gpkg=gpkg, showlist=FALSE)
  }
 
  ## Check out_name
  if (is.null(out_name)) 
    out_name <- "tab"

  ## Check tab
  tab <- pcheck.table(tab)

  ## Write table to database
  DBI::dbWriteTable(dbconn, out_name, tab, append=appendtab, overwrite=overwrite)
  message(paste("writing", out_name, "to", SQLitefn))

  if (!is.null(index.unique) && !all(index.unique %in% names(tab))) {
    message("invalid index.unique... names not in ", out_name)
    index.unique <- NULL
  }

  if (!is.null(index.unique)) {
    idxnm <- paste0(out_name, "_", paste(tolower(index.unique), collapse="_"), "_idx")
    if (sum(duplicated(tab[,index.unique, with=FALSE])) > 0) {
      warning(idxnm, " is not unique... creating non-unique index")
      idxsql <- paste0("create index ", idxnm, " ON ", out_name,   
				"(", paste(index.unique, collapse=","), ")")
    } else {
      idxsql <- paste0("create unique index ", idxnm, " ON ", out_name, 
				"(", paste(index.unique, collapse=","), ")")

      DBI::dbExecute(dbconn, idxsql)
      message(sub("create", "creating", idxsql))
    }
  }
  if (!is.null(index) && !all(index %in% names(tab))) {
    message("invalid index... names not in tab")
    index <- NULL
  }
 
  if (!is.null(index)) {
    indxnm <- paste0(out_name, "_", paste(tolower(index), collapse="_"), "_idx")
    message("adding index: ", indxnm, " to ", out_name)
    idxsql <- paste0("create index ", idxnm, " ON ",
				out_name, "(",  paste(index, collapse=","), ")")
    DBI::dbExecute(dbconn, idxsql)
    message(sub("create", "creating", idxsql))
  }

  ## If closedb is TRUE, close the sql database dbconnection.
  if (!dbconnopen)
    DBI::dbDisconnect(dbconn)
}
