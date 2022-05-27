changeclass <- function(tab, noIDate=TRUE) {
  ## Description: changes class of columns if integer64 or IDate
  ## 	if integer64 - if CN, PLT_CN, or PLT_CN, change to character 
  ##	if integer64 - if not CN, PLT_CN, or PLT_CN, change to numeric 
  ##	if IDate - change to character

  isdt <- TRUE
  if (!"data.table" %in% class(tab)) {
    tab <- setDT(tab) 
    isdt <- FALSE
  }
  tabclass <- unlist(lapply(tab, class))
 
  if (any(tabclass == "integer64")) { 
    int64vars <- names(tabclass)[tabclass == "integer64"]
    int64vars.CN <- int64vars[int64vars %in% c("CN", "PLT_CN", "PREV_PLT_CN")]
    int64vars.notCN <- int64vars[!int64vars %in% int64vars.CN]

    if (length(int64vars.CN) > 0) {
      tab[, (int64vars) := lapply(.SD, as.character), .SDcols=int64vars]
    } 
    if (length(int64vars.notCN) > 0) {
      tab[, (int64vars) := lapply(.SD, as.numeric), .SDcols=int64vars]
    } 
  }

  if (noIDate) {
    cols <- names(tab)[unlist(lapply(tab, function(x) any(class(x) == "IDate")))]
    if (length(cols) > 0) {
      tab[, (cols) := lapply(.SD, as.character), .SDcols=cols]
      if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
        tab$MODIFIED_DATE <- as.character(tab$MODIFIED_DATE)
      }
    } 
  } else {
    if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
      tab$MODIFIED_DATE <- as.IDate(tab$MODIFIED_DATE)
    }
  }

  if (isdt) {
    tab <- setDF(tab)
  }
  return(tab)
}
 