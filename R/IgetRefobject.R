getRefobject <- function(xvar){
  ## DESCRIPTION: Internal function to get the name of the reftable  
  ##		reference table associated with the variable.
  ## ARGUMENTS:
  ## xvar - The name of the variable.
  ## VALUE: The table name


  vardiff <- c("FORTYPCD", "FLDTYPCD", "FORTYPGRP", "FORTYPCDCALC", "TYPGRPCD",
	paste0("DSTRBCD", seq(1:3)), paste0("TRTCD", seq(1:3)))

  if (!xvar %in% vardiff) {
    if (!xvar %in% FIESTA::ref_codes[["VARIABLE"]]) {
      return(NULL)
    } else {
      return(xvar)
    }
  } else {

    switch(xvar,
      FORTYPCD = 'FORTYPCD',
      FLDTYPCD = 'FORTYPCD',
      FORTYPGRP = 'FORTYPCD',
      FORTYPCDCALC = 'FORTYPCD',
      TYPGRPCD = 'FORTYPCD',
      DSTRBCD1 = 'DSTRBCD',
      DSTRBCD2 = 'DSTRBCD',
      DSTRBCD3 = 'DSTRBCD',
      TRTCD1 = 'TRTCD',
      TRTCD2 = 'TRTCD',
      TRTCD3 = 'TRTCD',
    )

  }
}  
