
<!-- badges: start -->

[![R-CMD-check](https://github.com/USDAForestService/FIESTA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USDAForestService/FIESTA/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/FIESTA)](https://CRAN.R-project.org/package=FIESTA)
[![CRAN
checks](https://badges.cranchecks.info/worst/FIESTA.svg)](https://cran.r-project.org/web/checks/check_results_FIESTA.html)
[![r-universe
status](https://usdaforestservice.r-universe.dev/badges/FIESTA)](https://usdaforestservice.r-universe.dev/FIESTA)
<!-- badges: end -->

<!-- [![GitHub Super-Linter](https://github.com/USDAForestService/FIESTA/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter) -->

# FIESTA <img src="https://github.com/USDAForestService/FIESTA/blob/master/figs/fiesta_grey.png?raw=true" align="right" width=150 />

## Overview

The R package, `FIESTA` (Forest Inventory ESTimation and Analysis) is a
research estimation tool for analysts that work with sample-based
inventory data from the U.S. Department of Agriculture, Forest Service,
Forest Inventory and Analysis (FIA) Program. `FIESTA` can generate FIA’s
traditional state-wide estimates while also accommodate: unique
population boundaries, different evaluation time periods, customized
stratification schemes, non-standard variance equations, integration of
multi-scale remotely-sensed data and other auxiliary information, and
interaction with other modeling and estimation tools from CRAN’s library
of packages. `FIESTA` contains a collection of functions that can query
FIA databases, summarize and compile plot and spatial data, and generate
estimates with associated sampling errors.

Functions are organized by type or objective and are named with a
corresponding prefix (Fig. 1). `FIESTA` core functions (CORE) facilitate
data extraction and compilation of data input information and are used
independently or within the `FIESTA` estimation modules. `FIESTA`
estimation modules (MODULE) combine multiple functions from `FIESTA` or
other packages to generate population estimates using different
estimation strategies. Each module has an associated `mod*pop` function
for compiling the population data and calculations (e.g., adjustments
for nonresponse, standardizing auxiliary data) for a custom boundary and
can be used for generating multiple estimates. `FIESTA` analysis
functions, found in the `FIESTAnalysis` package, streamline different
estimation routines by wrapping (i.e., combining) estimation modules and
other functions for a specific purpose.

##### Core Functions

- Database tools (`DB*`) - functions for querying and extracting data
  from FIA’s national database.
- Data tools (`dat*`) - functions for summarizing and exploring FIA
  data.
- Spatial tools (`sp*`) - functions for manipulating and summarizing
  spatial data.

##### Estimation Modules (mod)

- Green-Book (`modGB*`) - functions for FIA’s standard Green-Book
  estimators.
- Photo-Based (`modPB*`) - functions for supplementary photo-based
  estimators.
- Small Area (`modSA*`) - functions for integration with available small
  area estimators (SAE).
- Model-Assisted (`modMA*`) - functions for integration with available
  Model-Assisted estimators.

##### Analysis Functions

- Analysis functions (`an*`) - wrapper functions for steam-lining
  estimation processes. These functions reside in the `FIESTAnalysis`
  package.

## Installation

#### Stable installation

You can install the current stable version of `FIESTA` from CRAN:

``` r
install.packages("FIESTA")
```

#### Developmental installation

Or, if you’d like to install the developmental version of `FIESTA`, you
can do so through a few steps:

##### 1. Install Rtools or xcode

If you are using the Windows OS, in order to install source code from
GitHub, you must install Rtools from
[CRAN](https://cran.r-project.org/). Install the most current Rtools for
Windows 64-bit at [this
link](https://cran.r-project.org/bin/windows/Rtools/).

If you are using macOS, you’ll need to install xcode developer tools to
install source code from GitHub. To do so, run the following code in
your terminal (not the R console):

    xcode-select --install

##### 2. Install the developmental version of `FIESTA` (and `FIESTAutils`)

First note that the developmental version of `FIESTA` may rely on a
developmental version of `FIESTAutils`. For both of these installations,
you’ll need to make sure to have the `remotes` package, and then you can
install both packages from GitHub:

``` r
# Install developmental FIESTAutils first
remotes::install_github("USDAForestService/FIESTAutils",
                        dependencies = TRUE)

# Then install developmental FIESTA
remotes::install_github("USDAForestService/FIESTA",
                        dependencies = TRUE)
```

## Bug Reports

To report a bug with `FIESTA`, please open an issue on the [`FIESTA`
GitHub Repository issues
page](https://github.com/USDAForestService/FIESTA/issues). Please
provide a description of the bug, and a reproducible example. For help
creating a reproducible example, see the
[`reprex`](https://reprex.tidyverse.org/) R package.

## Copyright and License

This code was written and prepared by U.S. Government employees on
official time, and therefore it is in the public domain and not subject
to copyright.

License is GPL-3.

## Accessing Documentation

### Vignettes

The vignette tutorials from `FIESTA` can be accessed from [the package
website](https://usdaforestservice.github.io/FIESTA/articles/). The
vignettes are split up into a few groups: general manuals ([Module
Estimates](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_manual_mod_est.html)
and [Population
Data](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_manual_mod_pop.html)),
core functions ([Database
Tools](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_DB.html),
[Data
Tools](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_dat.html),
and [Spatial
Tools](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_sp.html)),
and estimation modules ([Green-book
Estimators](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_GB.html),
[Model-Assisted
Estimators](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_MA.html),
[Small Area
Estimators](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_SA.html),
and [Photo-Based
Module](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_PB.html)).
We suggest you read the general manuals first if you are new to
`FIESTA`.

### External Data

You can access documentation for external data included in `FIESTA` in
the [`extdata-README.md` file](inst/extdata-README.md).

## Examples

These examples make use of vignettes that come with `FIESTA`, and these
vignettes can be found by calling `vignette(package = "FIESTA")`. The
data used in these examples come with the `FIESTA` package and are from
Wyoming, inventory years 2011-2013 (Evaluation 561301). We first load
`FIESTA` to run these examples:

``` r
library(FIESTA)
```

### Example 1: Green-book estimation

In order to produce estimates based on the Green-book, we first use the
`GBpopdat` function to produce population data for our areas of
interest. We can look at a summary of the population data below.

``` r
GBpopdat <- modGBpop(popTabs = list(cond = FIESTA::WYcond,  
                                    tree = FIESTA::WYtree),      
                     popTabIDs = list(cond = "PLT_CN"),
                     pltassgn = FIESTA::WYpltassgn,
                     pltassgnid = "CN",
                     pjoinid = "PLT_CN",
                     unitarea = FIESTA::WYunitarea,
                     unitvar = "ESTN_UNIT", 
                     strata = TRUE,
                     stratalut = FIESTA::WYstratalut,
                     strata_opts = strata_options(getwt = TRUE))

summary(GBpopdat)
#>               Length Class      Mode     
#> module         1     -none-     character
#> popType        1     -none-     character
#> pltidsadj      5     data.table list     
#> pltcondx      29     data.table list     
#> pltcondflds   28     -none-     character
#> pjoinid        1     -none-     character
#> cuniqueid      1     -none-     character
#> condid         1     -none-     character
#> ACI            1     -none-     logical  
#> areawt         1     -none-     character
#> areawt2        0     -none-     NULL     
#> adjcase        1     -none-     character
#> dbqueries      4     -none-     list     
#> dbqueriesWITH  4     -none-     list     
#> pltassgnx      4     data.table list     
#> pltassgnid     1     -none-     character
#> unitarea       2     data.table list     
#> areavar        1     -none-     character
#> areaunits      1     -none-     character
#> unitvar        1     -none-     character
#> unitvars       1     -none-     character
#> unitltmin      1     -none-     numeric  
#> strata         1     -none-     logical  
#> stratalut      6     data.table list     
#> strvar         1     -none-     character
#> strwtvar       1     -none-     character
#> plotsampcnt    0     data.frame list     
#> condsampcnt    3     data.frame list     
#> states         1     -none-     character
#> invyrs         0     -none-     NULL     
#> adj            1     -none-     character
#> P2POINTCNT     3     data.frame list     
#> plotunitcnt    2     data.frame list     
#> treex         19     data.table list     
#> tuniqueid      1     -none-     character
#> adjfactors     7     data.table list     
#> adjvarlst      4     -none-     character
#> popdatindb     1     -none-     logical
```

Note that the `GBpopdat` list generated by `modGBpop` contains many
items. Some examples include the number of plots by plot status
(`plotsampcnt`), the number of conditions by condition status
(`condsampcnt`), the strata-level population data, including number of
plots and adjustment factors (`stratalut`), and the adjustment factors
added to the condition-level, tree-level, and seedling data (`condx`,
`treex`, and `seedx`, respectfully).

Now, with the `GBpopdat` object, we can quickly produce estimates of
basal area (`estvar = "BA"`) by county in Wyoming for the 2011-2013
years.

``` r
GBest <- modGBtree(GBpopdat = GBpopdat,
                   estvar = "BA",
                   estvar.filter = "STATUSCD == 1",
                   sumunits = FALSE)
```

We again output a list, now with estimates/standard errors, raw data,
state code, and inventory year:

``` r
str(GBest, max.level = 2)
#> List of 4
#>  $ est    :'data.frame': 23 obs. of  3 variables:
#>   ..$ ESTN_UNIT             : chr [1:23] "1" "11" "13" "15" ...
#>   ..$ Estimate              : num [1:23] 34637492 34861880 70412559 308998 4687031 ...
#>   ..$ Percent Sampling Error: num [1:23] 11.7 14.4 11.6 84.6 67.7 ...
#>  $ raw    :List of 12
#>   ..$ unit_totest  :'data.frame':    23 obs. of  18 variables:
#>   ..$ domdat       :'data.frame':    590 obs. of  4 variables:
#>   ..$ domdatqry    : chr "WITH\npltids AS\n(SELECT DISTINCT PLT_CN\n FROM condx), \n----- pltcondx\npltcondx AS\n(SELECT c.PLT_CN, c.COND"| __truncated__
#>   ..$ estvar       : chr "BA"
#>   ..$ estvar.filter: chr "STATUSCD == 1"
#>   ..$ module       : chr "GB"
#>   ..$ esttype      : chr "TREE"
#>   ..$ GBmethod     : chr "PS"
#>   ..$ rowvar       : chr "TOTAL"
#>   ..$ colvar       : chr "NONE"
#>   ..$ areaunits    : chr "acres"
#>   ..$ estunits     : chr "square feet"
#>  $ statecd: int 56
#>  $ states : chr "Wyoming"
```

### Example 2: Model-assisted estimation

`FIESTA` makes it easy to do estimation through techniques such as
model-assited estimation and small area estimation. In this example, we
use a similar process to the Green-Book estimation above to produce
estimates for the same region, but through a generalized regression
(GREG) model-assisted estimator. First, we get our population data:

``` r
MApopdat <- modMApop(popTabs = list(tree = FIESTA::WYtree,
                                    cond = FIESTA::WYcond),
                     pltassgn = FIESTA::WYpltassgn,
                     pltassgnid = "CN",
                     unitarea = FIESTA::WYunitarea,
                     unitvar = "ESTN_UNIT",
                     unitzonal = FIESTA::WYunitzonal,
                     prednames = c("dem", "tcc", "tpi", "tnt"),
                     predfac = "tnt")
```

Now, analogous to the `modGBtree()` function we can produce estimates
with the `modMAtree()` function

``` r
MAest <- modMAtree(MApopdat = MApopdat,
                   MAmethod = "greg",
                   estvar = "BA",
                   estvar.filter = "STATUSCD == 1")
```

and we can see the output of `modMAtree()`:

``` r
str(MAest, max.level = 2)
#> List of 4
#>  $ est    :'data.frame': 23 obs. of  3 variables:
#>   ..$ ESTN_UNIT             : chr [1:23] "1" "11" "13" "15" ...
#>   ..$ Estimate              : num [1:23] 75809929 117896280 312165085 -14313592 42191880 ...
#>   ..$ Percent Sampling Error: num [1:23] 17 12.3 11 0 43.6 ...
#>  $ raw    :List of 13
#>   ..$ unit_totest  :'data.frame':    23 obs. of  18 variables:
#>   ..$ domdat       :'data.frame':    590 obs. of  5 variables:
#>   ..$ plotweights  :Classes 'data.table' and 'data.frame':   556 obs. of  5 variables:
#>   .. ..- attr(*, ".internal.selfref")=<externalptr> 
#>   ..$ estvar       : chr "BA"
#>   ..$ estvar.filter: chr "STATUSCD == 1"
#>   ..$ module       : chr "MA"
#>   ..$ esttype      : chr "TREE"
#>   ..$ MAmethod     : chr "greg"
#>   ..$ predselectlst:List of 1
#>   ..$ rowvar       : chr "TOTAL"
#>   ..$ colvar       : chr "NONE"
#>   ..$ areaunits    : chr "acres"
#>   ..$ estunits     : chr "square feet"
#>  $ statecd: int 56
#>  $ states : chr "Wyoming"
```
