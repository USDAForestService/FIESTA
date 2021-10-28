getRefcodes <- function(xvar){
  ## DESCRIPTION: Internal function to facilitate looking at codes from FIESTA::ref_codes
  ##		If xvar does not exist, returns NULL
  ## ARGUMENTS:
  ## xvar - The name of the variable.
  ## VALUE: The table name


  vardiff <- c("FORTYPCD", "FLDTYPCD", "FORTYPGRP", "FORTYPCDCALC", "TYPGRPCD",
	paste0("DSTRBCD", seq(1:3)), paste0("TRTCD", seq(1:3)))

  if (!xvar %in% vardiff) {
    if (grepl("PREV_", xvar)) {
      xvar <- sub("PREV_", "", xvar)
    }
    if (!xvar %in% FIESTA::ref_codes[["VARIABLE"]]) {
      return(NULL)
    } 
  } else {

    switch(xvar,
      FORTYPCD = 'FORTYPCD',
      FLDTYPCD = 'FORTYPCD',
      FORTYPGRP = 'FORTYPCD',
      FLDTYPGRP = 'FORTYPCD',
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
  return(ref_codes[ref_codes$VARIABLE == xvar, 
		c("VALUE", "MEANING", "GROUPCD", "GROUPNM")])

}  
