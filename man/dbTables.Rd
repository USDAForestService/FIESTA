% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbTables.R
\name{dbTables}
\alias{dbTables}
\title{List of population tables.}
\usage{
dbTables(
  plot_layer = "PLOT",
  cond_layer = "COND",
  tree_layer = "TREE",
  seed_layer = "SEEDLING",
  plotgeom_layer = "PLOTGEOM",
  vsubpspp_layer = "P2VEG_SUBPLOT_SPP",
  vsubpstr_layer = "P2VEG_SUBP_STRUCTURE",
  invsubp_layer = "INVASIVE_SUBPLOT_SPP",
  subplot_layer = "SUBPLOT",
  subpcond_layer = "SUBP_COND",
  dwm_layer = "COND_DWM_CALC",
  cwd_layer = "DWM_COARSE_WOODY_DEBRIS",
  fwd_layer = "DWM_FINE_WOODY_DEBRIS",
  sccm_layer = "SUBP_COND_CHNG_MTRX",
  grm_layer = "TREE_GRM_COMPONENT",
  grmb_layer = "TREE_GRM_BEGIN",
  grmm_layer = "TREE_GRM_MIDPT",
  survey_layer = "SURVEY",
  popeval_layer = "POP_EVAL",
  popevalgrp_layer = "POP_EVAL_GRP",
  popevaltyp_layer = "POP_EVAL_TYP",
  popstratum_layer = "POP_STRATUM",
  popestnunit_layer = "POP_ESTN_UNIT",
  ppsa_layer = "POP_PLOT_STRATUM_ASSGN",
  fvsstandplot_layer = "FVS_STANDINIT_PLOT",
  fvsstandcond_layer = "FVS_STANDINIT_COND",
  refspp_layer = "REF_SPECIES",
  other_layers = NULL,
  ...
)
}
\arguments{
\item{plot_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Plot-level data (PLOT).}

\item{cond_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Condition-level data (COND).}

\item{tree_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Tree-level data (TREE).}

\item{seed_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Seedling data (SEEDLING).}

\item{plotgeom_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Plot-level GIS extracted data (PLOTGEOM).}

\item{vsubpspp_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Understory vegetation species data 
(P2VEG_SUBPLOT_SPP).}

\item{vsubpstr_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Understory vegetation structure data 
(P2VEG_SUBP_STRUCTURE).}

\item{invsubp_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Understory vegetation invasives data 
(INVASIVE_SUBPLOT_SPP).}

\item{subplot_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Subplot-level data (SUBPLOT).}

\item{subpcond_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Subplot condition-level data (SUBP_COND).}

\item{dwm_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Down wood material data (COND_DWM_CALC)}

\item{cwd_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Down wood material, coarse woody debris data 
(DWM_COARSE_WOODY_DEBRIS)}

\item{fwd_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Down wood material, fine woody debris data 
(DWM_FINE_WOODY_DEBRIS)}

\item{sccm_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Subplot-level change matrix data
(SUBP_COND_CHNG_MTRX).}

\item{grm_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Tree growth, removal, mortality data 
(TREE_GRM_COMPONENT).}

\item{grmb_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Tree growth, removal, mortality begin data 
(TREE_GRM_BEGIN).}

\item{grmm_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Tree growth, removal, mortality midpoint data 
(TREE_GRM_MIDPT).}

\item{survey_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population survey (SURVEY) data.}

\item{popeval_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population evaluation (POP_EVAL) data.}

\item{popevalgrp_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population evaluation group data (POP_EVAL_GRP).}

\item{popevaltyp_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population evaluation type data (POP_EVAL_TYP).}

\item{popstratum_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population stratum data (POP_STRATUM).}

\item{popestnunit_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population estimation unit data (POP_ESTN_UNIT).}

\item{ppsa_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Population plot stratum assignment data (
(POP_PLOT_STRATUM_ASSGN).}

\item{fvsstandplot_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. FVS stand plot data (FVS_STANDINIT_PLOT)}

\item{fvsstandcond_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. FVS stand plot data (FVS_STANDINIT_COND)}

\item{refspp_layer}{R object, comma-delimited file(*.csv), or name of
layer in database. Reference table for species (REF_SPECIES).}

\item{other_layers}{R object, comma-delimited file(*.csv), or name of
layer in database. Other layers to extract from database.}

\item{...}{For extendibility.}
}
\value{
A list of user-supplied parameters and parameter values for strata.
}
\description{
Returns a list of user-supplied parameters and parameter values for data 
tables to be supplied to *DB functions.
}
\details{
If no parameters, an empty list is returned.
}
\examples{
dbTables(plot_layer = FIESTA::WYplt)
}
\author{
Tracey S. Frescino
}
\keyword{list}
