getfunnm <- function(x){
  if(length(grep(".Primitive", deparse(x))) > 0){
    if(length(grep("max", deparse(x))) > 0){
      funnm <- "max"
    }else if(length(grep("min", deparse(x))) > 0){
      funnm <- "min"
    }else if(length(grep("sum", deparse(x))) > 0){
      funnm <- "sum"
    }
  }else if(length(grep("standardGeneric", deparse(x))) > 0){
    if(length(grep("mean", deparse(x))) > 0){
      funnm <- "mean"
    }
  }else{
    warning("unknown function")
    funnm <- "fun"
  }
  return(funnm)
}
