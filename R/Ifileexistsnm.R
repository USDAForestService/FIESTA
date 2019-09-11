fileexistsnm <- function(outfolder, basenm, ext){
  ## DESCRIPTION: Internal function to check if file exists..  
  ## If so, adds a number to the basenm and returns just the basenm.

  i <- 1
  newbasenm <- basenm
  while(file.exists(paste0(outfolder, "/", newbasenm, ".", ext))){
    newbasenm <- paste0(basenm, "_", i)
    i <- i + 1}
  return(newbasenm)
}
