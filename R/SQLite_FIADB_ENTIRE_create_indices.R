SQLite_FIADB_ENTIRE_create_indices <- function(USdsn) {
  ## Description: Creates indices in the National FIA database
  
  USconn <- DBtestSQLite(USdsn, dbconnopen  = TRUE)

  ## Create an index on a database table
  createidx(USconn, tbl = "PLOT", index_cols = "CN", unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("PLOT", "INVYR", "COUNTYCD", "UNITCD", "STATECD"), unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("PREV_PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "COND", index_cols = c("CONDID", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("TREE", "SUBP", "CONDID", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("SPCD"), unique = FALSE)
  createidx(USconn, tbl = "TREE", index_cols = c("CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("SPCD", "SUBP", "CONDID", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("PLT_CN", "EVALID"), unique = TRUE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("PLOT", "INVYR", "COUNTYCD", "STATECD", "EVALID"), unique = TRUE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("STRATUMCD", "ESTN_UNIT"), unique = FALSE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("STRATUM_CN"), unique = FALSE)
  createidx(USconn, tbl = "SUBPLOT", index_cols = c("SUBP", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("CONDID", "SUBP", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("SUBP", "PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("CONDID", "PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "PLOTGEOM", index_cols = c("CN"), unique = TRUE)
  createidx(USconn, tbl = "REF_SPECIES", index_cols = c("SPCD"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("CONDID", "PREVCOND", "SUBPTYP", "SUBP", "PLT_CN", "PREV_PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SURVEY", index_cols = c("INVYR", "STATECD"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("EVAL_GRP_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("EVALID", "RSCD"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("EVAL_GRP", "RSCD"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("EVAL_GRP"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_GRP_CN", "EVAL_TYP"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_GRP_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_STRATUM", index_cols = c("CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_STRATUM", index_cols = c("STRATUMCD", "ESTN_UNIT", "EVALID"), unique = FALSE)
  

  ## Disconnect database
  DBI::dbDisconnect(USconn)
}
