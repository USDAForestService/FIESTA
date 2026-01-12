SQLite_FIADB_ENTIRE_create_indices <- function(USdsn) {
  ## Description: Creates indices in the National FIA database
  
  USconn <- DBtestSQLite(USdsn, dbconnopen  = TRUE)

  ## Create an index on a database table
  createidx(USconn, tbl = "PLOT", index_cols = "CN", unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("STATECD", "UNITCD", "COUNTYCD", "INVYR", "PLOT"), unique = TRUE)
  createidx(USconn, tbl = "PLOT", index_cols = c("PREV_PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "COND", index_cols = c("PLT_CN", "CONDID"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("PLT_CN", "CONDID", "SUBP", "TREE"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("SPCD"), unique = FALSE)
  createidx(USconn, tbl = "TREE", index_cols = c("CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("PLT_CN", "CONDID", "SUBP", "SPCD"), unique = TRUE)
  createidx(USconn, tbl = "SEEDLING", index_cols = c("PLT_CN"), unique = FALSE)
  createidx(USconn, tbl = "PLOTGEOM", index_cols = c("CN"), unique = TRUE)
  
  ## ref tables
  createidx(USconn, tbl = "REF_SPECIES", index_cols = c("SPCD"), unique = TRUE)
  
  ## pop tables
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("EVALID", "PLT_CN"), unique = TRUE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("EVALID", "STATECD", "COUNTYCD", "INVYR", "PLOT"), unique = TRUE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("ESTN_UNIT", "STRATUMCD"), unique = FALSE)
  createidx(USconn, tbl = "POP_PLOT_STRATUM_ASSGN", index_cols = c("STRATUM_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("EVAL_GRP_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL", index_cols = c("RSCD", "EVALID"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("RSCD", "EVAL_GRP"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_GRP", index_cols = c("EVAL_GRP"), unique = FALSE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_TYP", "EVAL_GRP_CN"), unique = TRUE)
  createidx(USconn, tbl = "POP_EVAL_TYP", index_cols = c("EVAL_GRP_CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_STRATUM", index_cols = c("CN"), unique = FALSE)
  createidx(USconn, tbl = "POP_STRATUM", index_cols = c("EVALID", "ESTN_UNIT", "STRATUMCD"), unique = FALSE)
  createidx(USconn, tbl = "SURVEY", index_cols = c("STATECD", "INVYR"), unique = FALSE)

  ## subplot table  
  createidx(USconn, tbl = "SUBPLOT", index_cols = c("PLT_CN", "SUBP"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "SUBP", "CONDID"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "SUBP"), unique = FALSE)
  createidx(USconn, tbl = "SUBP_COND", index_cols = c("PLT_CN", "CONDID"), unique = FALSE)

  ## change / grm tables
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("PLT_CN", "PREV_PLT_CN", "CONDID", "PREVCOND", "SUBPTYP", "SUBP"), unique = TRUE)
  createidx(USconn, tbl = "SUBP_COND_CHNG_MTRX", index_cols = c("PLT_CN"), unique = FALSE)
  
  createidx(USconn, tbl = "TREE_GRM_COMPONENT", index_cols = c("PREV_TRE_CN"), unique = FALSE)
  createidx(USconn, tbl = "TREE_GRM_COMPONENT", index_cols = c("TRE_CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE_GRM_COMPONENT", index_cols = c("PLT_CN"), unique = FALSE)

  createidx(USconn, tbl = "TREE_GRM_MIDPT", index_cols = c("PREV_TRE_CN"), unique = FALSE)
  createidx(USconn, tbl = "TREE_GRM_MIDPT", index_cols = c("TRE_CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE_GRM_MIDPT", index_cols = c("PLT_CN"), unique = FALSE)
  
  createidx(USconn, tbl = "TREE_GRM_BEGIN", index_cols = c("PREV_TRE_CN"), unique = FALSE)
  createidx(USconn, tbl = "TREE_GRM_BEGIN", index_cols = c("TRE_CN"), unique = TRUE)
  createidx(USconn, tbl = "TREE_GRM_BEGIN", index_cols = c("PLT_CN"), unique = FALSE)

  ## Disconnect database
  DBI::dbDisconnect(USconn)
}
