selectvars <- function(dbconn, schema, tabnm, varkeep=NULL, vardelete=NULL, titletab=NULL){
  ## DESCRIPTION: Internal function to extract column names for tables in database 
  ## 		and prompt user to select variables. The varkeep variables are removed before 
  ##		selection and added later.
  ## ARGUMENTS:
  ## dbconn - Oracle database connection (must be already open)
  ## SCHEMA - Oracle schema
  ## tabnm - table name
  ## varkeep - Variables that should always be kept regardless of selection
  ## vardelete - Variables that should not be selected.. removed from selection list
  ## titletab - The type of variable to select
  ## VALUE: 
  ## vars - Selected variables
  ## filtervarlst - Complete variable list (before selection)  

  qry <- paste0("select column_name from ALL_TAB_COLS where owner = '", schema, "' 
            and table_name = '", tabnm, "'")
  tryCatch( varlst <- DBqryORACLE(qry, dbconn), 
			error=function(e) stop("ALL_TAB query is invalid"))

  ## Remove important variables and add later
  if (!is.null(varkeep)) varlst <- varlst[-which(varlst %in% varkeep)]

  ## Remove other variables not needed   
  if (!is.null(vardelete)) varlst <- varlst[-which(varlst %in% vardelete)]
  
  filtervarlst <- varlst
  vars <- {}
  addvars <- "YES"
  while (addvars == "YES") {
    varselect <- select.list(c("NONE", sort(varlst)), title=paste(titletab, "var(s)?"), 
      multiple=TRUE)
    if (length(varselect) == 0) {
      return(NULL)
    } else if (length(varselect) == 1 && varselect[1] == "NONE") {
      break
    } else {
      vars <- c(vars, varselect)
    }
    #print(vars)

    varlst <- varlst[-which(varlst %in% vars)]
    if (is.null(titletab)) {
      titlex <- "Any more variables?"
    } else {
      titlex <- paste("More", tolower(titletab), "vars?")
    }
    addvars <- select.list(c("NO","YES"), title=titlex, multiple=FALSE)
    if (addvars == "") stop("")
  }
  vars <- c(varkeep, vars)
  
  return(list(vars=vars, filtervarlst=filtervarlst))
}
