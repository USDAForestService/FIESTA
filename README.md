
<!-- [![GitHub Super-Linter](https://github.com/USDAForestService/FIESTA/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter) -->

<b>Authors:</b> Frescino, Tracey S.; Moisen, Gretchen G.; Patterson,
Paul L.; Toney, Chris; White, Grayson W.

# 🎉 FIESTA <img src="https://github.com/USDAForestService/FIESTA/blob/master/figs/fiesta_grey.png?raw=true" align="right" width=150 />

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
of packages. `FIESTA` contains a collection of functions that can access
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

-   Database tools (`DB*`) - functions for querying and extracting data
    from FIA’s national database.
-   Data tools (`dat*`) - functions for summarizing and exploring FIA
    data.
-   Spatial tools (`sp*`) - functions for manipulating and summarizing
    spatial data.

##### Estimation Modules (mod)

-   Green-Book (`modGB*`) - functions for FIA’s standard Green-Book
    estimators.
-   Photo-Based (`modPB*`) - functions for supplementary photo-based
    estimators.
-   Small Area (`modSA*`) - functions for integration with available
    small area estimators (SAE).
-   Model-Assisted (`modMA*`) - functions for integration with available
    Model-Assisted estimators.

##### Analysis Functions

-   Analysis functions (`an*`) - wrapper functions for steam-lining
    estimation processes. These functions reside in the `FIESTAnalysis`
    package.

## Installation

<!-- #### Stable installation -->
<!-- You can install the current stable version of `FIESTA` from CRAN: -->
<!-- ```{r, eval = F} -->
<!-- install.packages("FIESTA") -->
<!-- ``` -->

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
                        build_vignettes = TRUE,
                        dependencies = TRUE)
```

## Bug Reports

To report a bug with `FIESTA`, please open an issue on the [`FIESTA`
GitHub Repository issues
page](https://github.com/USDAForestService/FIESTA/issues). Please
provide a description of the bug, and a reproducible example. For help
creating a reproducible example, see the
[`reprex`](https://reprex.tidyverse.org/) R package.

## License

This code was written and prepared by a U.S. Government employee on
official time, and therefore it is in the public domain and not subject
to copyright.

## Accessing Documentation

### Vignettes

To see a list of vignette tutorials from `FIESTA`, you can run the
following code:

``` r
vignette(package = "FIESTA")
```

These vignettes are split up into a few groups:

#### General Manuals

``` r
# Estimation manual
vignette("FIESTA_manual_mod_est", package = "FIESTA")

# Population data manual
vignette("FIESTA_manual_mod_pop", package = "FIESTA")
```

#### Core functions

``` r
# Data tools
vignette("FIESTA_tutorial_dat", package = "FIESTA")

# Database tools
vignette("FIESTA_tutorial_DB", package = "FIESTA")

# Spatial tools
vignette("FIESTA_tutorial_sp", package = "FIESTA")
```

#### Estimation Modules

``` r
# Green-Book estimation
vignette("FIESTA_tutorial_GB", package = "FIESTA")

# Model-assisted estimation
vignette("FIESTA_tutorial_MA", package = "FIESTA")

# Small area estimation
vignette("FIESTA_tutorial_SA", package = "FIESTA")

# Photo-based estimation
vignette("FIESTA_tutorial_PB", package = "FIESTA")
```

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
#>             Length Class      Mode     
#> popType      1     -none-     character
#> condx       12     data.table list     
#> pltcondx    43     data.table list     
#> cuniqueid    1     -none-     character
#> condid       1     -none-     character
#> ACI.filter   1     -none-     character
#> unitarea     2     data.table list     
#> areavar      1     -none-     character
#> areaunits    1     -none-     character
#> unitvar      1     -none-     character
#> unitvars     1     -none-     character
#> strata       1     -none-     logical  
#> stratalut   18     data.table list     
#> strvar       1     -none-     character
#> strwtvar     1     -none-     character
#> expcondtab  12     data.table list     
#> plotsampcnt  3     data.table list     
#> condsampcnt  3     data.table list     
#> states       1     -none-     character
#> invyrs       1     by         list     
#> estvar.area  1     -none-     character
#> adj          1     -none-     character
#> treex       21     data.table list     
#> tuniqueid    1     -none-     character
#> adjtree      1     -none-     logical
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
#>   ..$ ESTN_UNIT             : int [1:23] 1 3 5 7 9 11 13 15 17 19 ...
#>   ..$ Estimate              : num [1:23] 35117889 24184274 8223787 73174024 16031144 ...
#>   ..$ Percent Sampling Error: num [1:23] 11.71 26.91 32.24 8.56 31.4 ...
#>  $ raw    :List of 11
#>   ..$ unit_totest  :'data.frame':    23 obs. of  17 variables:
#>   ..$ domdat       :'data.frame':    590 obs. of  14 variables:
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
#>  $ invyr  : int [1:3] 2011 2012 2013
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
#>  $ est    :Classes 'data.table' and 'data.frame':    23 obs. of  3 variables:
#>   ..$ ESTN_UNIT             : int [1:23] 1 3 5 7 9 11 13 15 17 19 ...
#>   ..$ Estimate              : num [1:23] 34769303 28493559 8260491 69278394 23200459 ...
#>   ..$ Percent Sampling Error: num [1:23] 8.7 19.8 25.69 9.23 20.17 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>   ..- attr(*, "sorted")= chr "ESTN_UNIT"
#>  $ raw    :List of 13
#>   ..$ unit_totest  :'data.frame':    23 obs. of  18 variables:
#>   ..$ domdat       :'data.frame':    3210 obs. of  17 variables:
#>   ..$ plotweights  :List of 1
#>   ..$ estvar       : chr "BA"
#>   ..$ estvar.filter: chr "STATUSCD == 1"
#>   ..$ module       : chr "MA"
#>   ..$ esttype      : chr "TREE"
#>   ..$ MAmethod     : chr "greg"
#>   ..$ predselectlst:List of 1
#>   ..$ rowvar       : chr "TOTAL"
#>   ..$ colvar       : chr "NONE"
#>   ..$ areaunits    : chr "acres"
#>   ..$ estunits     : int [1:23] 1 3 5 7 9 11 13 15 17 19 ...
#>  $ statecd: int 56
#>  $ invyr  : int [1:3] 2011 2012 2013
```
