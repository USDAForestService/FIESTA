## kable.table - to create pretty tables


kable.table <- function(est, title.row, title.rowvar) {
    ## DESCRIPTION: create pretty tables
   
    ## set options to convert NA values to "--"
    options(knitr.kable.NA = "--")

    est[,-1] <- suppressWarnings(lapply(est[-1], as.numeric))

    knitr::kable(est,
    format = "pandoc",   # default
    caption = wraptitle(title.row, 80),
    col.names = names(est),
    row.names = FALSE,
    align = c("r"),       # align = c("c", "c", "c", "r")
    digits = 2,
    format.args=list(big.mark = ",")
    # padding = 2         # inner spacing
  ) 
}
