[![GitHub Super-Linter](https://github.com/USDAForestService/FIESTA/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)

<b>Authors:</b> Frescino, Tracey S.; Moisen, Gretchen G.; Patterson, Paul L.; Toney, Chris; Freeman, Elizabeth A.

The R package, FIESTA (Forest Inventory ESTimation and Analysis) is a research estimation tool for analysts that work with sample-based inventory data from the U.S. Department of Agriculture, Forest Service, Forest Inventory and Analysis (FIA) Program to accommodate: unique population boundaries, different evaluation time periods, customized stratification schemes, non-standard variance equations, integration of multi-scale remotely-sensed data and other ancillary information, and interaction with other modeling and estimation tools from CRAN R's library of packages. The FIESTA package contains a collection of functions that can access FIA databases, summarize and compile plot and spatial data, and generate estimates with associated sampling errors. Check Wiki for installation instructions.

<b>License:</b> This code was written and prepared by a U.S. Government employee on official time, and therefore it is in the public domain and not subject to copyright. 


<b>Installation:<b>


<b>Install FIESTA packages<b>
- Checks to make sure all FIESTA dependent packages are installed
- Removes old version of FIESTA from current R library
- pkginstalled <- installed.packages()
- if ("FIESTA" %in% row.names(pkginstalled)) remove.packages("FIESTA")

Define function
```
chkpkg <- function(pkg) {
  ## DESCRIPTION: Function to check if package exists
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies=TRUE)
  if (!require(pkg, character.only = TRUE)) stop("load failure: ", pkg)
}
```

If first time using FIESTA, install the following packages using the code below 
NOTE: select closest CRAN mirror for download

```
FIESTA.pkgs <- c('data.table', 'sf', 'rgdal', 'Rcpp')
lapply(FIESTA.pkgs, chkpkg)

FIESTA_suggests.pkgs <- c('knitr', 'rmarkdown', 'rJava', 'xlsx', 'JoSAE', 'mase', 
		'xml2', 'nlme', 'RSQLite', 'DBI', 'sqldf', 'httr', 'sp', 'raster')
lapply(FIESTA_suggests.pkgs, chkpkg)
```

Note: For Macintosh computers, you may need to install xcode developer tools first
Open Terminal and run the following code:
```
xcode-select --install
```


<b>HELP and vignettes<b>

To get help for fiesta package
```
help(package="FIESTA")
```

To get tutorials from FIESTA package
```
vignette(package="FIESTA")
```

Core functions
```
vignette("FIESTA_tutorial_DB", package="FIESTA")
vignette("FIESTA_tutorial_dat", package="FIESTA")
vignette("FIESTA_tutorial_sp", package="FIESTA")
```

Modules
```
vignette("FIESTA_tutorial_GB", package="FIESTA")
vignette("FIESTA_tutorial_GBcustom", package="FIESTA")
vignette("FIESTA_tutorial_PB", package="FIESTA")
vignette("FIESTA_tutorial_SA", package="FIESTA")
vignette("FIESTA_tutorial_MA", package="FIESTA")
```
