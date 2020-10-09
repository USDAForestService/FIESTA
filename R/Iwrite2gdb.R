write2gdb <- function(layer, gdbfn, out_name=NULL, overwrite=FALSE){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##  
  ## ARGUMENTS: 
  ##  layer    DF. The table to output.
  ##  out_name	String. Name of output layer.
  ##
  ## VALUE:
  ##  Writes data frame to ESRI file geodatabase.
  ####################################################################################

  ## Check SQLite connection
  ###########################################################
  gdbfn <- DBtestESRIgdb(gdbfn=gdbfn, showlist=FALSE)

  ## Check out_name
  if (is.null(out_name)) 
    out_name <- "layer"

  ## Write table to database
  arcgisbinding::arc.write(file.path(gdbfn, out_name), data=layer, overwrite=overwrite)

}
