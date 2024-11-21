SQLite_FIADB_ENTIRE_create_indices <- function(USdsn) {
  ## Description: Creates indices in the National FIA database
  
  USconn <- DBtestSQLite(USdsn, dbconnopen  = TRUE)

  ## Create an index on a database table
  createidx(USconn, tbl = "PLOT", index_cols = "CN", unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR"), unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("PREV_PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "COND", index_cols = c("PLT_CN", "CONDID"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("PLT_CN", "CONDID", "SUBP", "TREE"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("SPCD"), unique = FALSE)
  createidx(USconn, tbl = "TREE", index_cols = c("CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("PLT_CN", "CONDID", "SUBP", "SPCD"), unique = TRUE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("PLT_CN", "EVALID"), unique = TRUE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("EVALID", "STATECD", "COUNTYCD", "PLOT", "INVYR"), unique = TRUE)
  createidx(USconn, tbl = "SUBPLOT", index_cols = c("PLT_CN", "SUBP"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "SUBP", "CONDID"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "SUBP"), unique = FALSE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "CONDID"), unique = FALSE)
  createidx(USconn, tbl = "PLOTGEOM", index_cols = c("CN"), unique = TRUE)
  createidx(USconn, tbl = "REF_SPECIES", index_cols = c("SPCD"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("PLT_CN", "PREV_PLT_CN", "SUBP", "SUBPTYP", "CONDID", "PREVCOND"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SURVEY", index_cols = c("STATECD", "INVYR"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("EVAL_GRP_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("RSCD", "EVALID"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("RSCD", "EVAL_GRP"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("EVAL_GRP"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_GRP_CN", "EVAL_TYP"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_GRP_CN"), unique = FALSE)


  ## Disconnect database
  DBI::dbDisconnect(USconn)
}
