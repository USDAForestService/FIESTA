[![GitHub Super-Linter](https://github.com/USDAForestService/FIESTA/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)

<b>Authors:</b> Frescino, Tracey S.; Moisen, Gretchen G.; Patterson, Paul L.; Toney, Chris; Freeman, Elizabeth A.

# FIESTA

## Overview

The R package, FIESTA (Forest Inventory ESTimation and Analysis) is a research estimation tool for analysts that work with sample-based inventory data from the U.S. Department of Agriculture, Forest Service, Forest Inventory and Analysis (FIA) Program to accommodate: unique population boundaries, different evaluation time periods, customized stratification schemes, non-standard variance equations, integration of multi-scale remotely-sensed data and other ancillary information, and interaction with other modeling and estimation tools from CRAN R's library of packages. The FIESTA package contains a collection of functions that can access FIA databases, summarize and compile plot and spatial data, and generate estimates with associated sampling errors. Check Wiki for installation instructions.

## License:

This code was written and prepared by a U.S. Government employee on official time, and therefore it is in the public domain and not subject to copyright. 


## Installation:

### 1. Install Rtools

If Windows OS, in order to install source code from GitHub, you must install Rtools from 
the CRAN website (https://cran.r-project.org/). Go to Download R for Windows and Install the most current Rtools for Windows 64-bit.


### 2. Create token for GitHub

For ease of installing and updating FIESTA, generate a token from GitHub settings.
Note: FIESTA is frequently updated. You will use this token each time you update 
FIESTA (until public release).

1. In the upper-right corner of any page, click your profile photo, then click Settings in dropdown menu.
2. In the left sidebar, click Developer settings.
3. Go to Personal access tokens.
4. Click Generate new token.
5. Give your token a descriptive name.
6. Check all boxes
7. Save token (~30 character string) to a file and as an R object.


### 3. Install FIESTA suggested packages

- Checks to make sure all FIESTA dependent packages are installed
- Removes old version of FIESTA from current R library
- pkginstalled <- installed.packages()
- if ("FIESTA" %in% row.names(pkginstalled)) remove.packages("FIESTA")

Use the following function to check and install packages for FIESTA.
Note: copy/paste function into RStudio or RGui 
```
chkpkg <- function(pkg) {
  ## DESCRIPTION: Function to check if package exists
  ## If package does not exist, it will install the package
  if (!require(pkg, character.only = TRUE)) {
    message("installing ", pkg, " package...")
    install.packages(pkg, dependencies=TRUE)
  }
  if (!require(pkg, character.only = TRUE)) {
    stop("load failure: ", pkg)
  } 
}
```

FIESTA dependent and imported packages are installed when you install the source code 
from GitHub.

If you are not installing the source code and it is the first time using FIESTA, 
you must install the following packages:
depend.pkgs <- c('data.table', 'sf', 'rgdal', 'Rcpp')
lapply(depend.pkgs, chkpkg)

You will need to install suggested packages even if installing the source code.
Installing suggested packages is optional, depending on how you are using FIESTA. 

NOTE: select closest CRAN mirror for download

```
## Load suggests packages
misc.pkgs <- c('devtools', 'units')
lapply(misc.pkgs, chkpkg)


## For database extraction
db.pkgs <- c('DBI', 'odbc', 'sqldf', 'RSQLite')
lapply(db.pkgs, chkpkg)

## For model-assisted estimation
ma.pkgs <- c('mase')
lapply(ma.pkgs, chkpkg)

## For small-area estimation
sa.pkgs <- c('sae', 'JoSAE', 'nlme')
lapply(sa.pkgs, chkpkg)

## For xlsx output
xlsx.pkgs <- c('rJava', 'xlsx')
lapply(xlsx.pkgs, chkpkg)

## For reports
report.pkgs <- c('knitr', 'rmarkdown', 'RColorBrewer')
lapply(report.pkgs, chkpkg)

## For PB module reports
pbreport.pkgs <- c('pheatmap')
lapply(pbreport.pkgs, chkpkg)

## For spatial manipulation
sp.pkgs <- c('rgeos', 'raster')
lapply(sp.pkgs, chkpkg)

```


### 4. Install FIESTA package from source code

If you are updating FIESTA, first make sure the FIESTA library is not attached, 
then uninstall from your library.

```
detach("package:FIESTA", unload=TRUE)
remove.packages("FIESTA", lib=.libPaths()) 
```

Next install FIESTA from GitHub, using the token you saved as an R object.

```
## Set your token to an R character object, in quotes (Replace your_token with ~30 character string)
token <- "your_token"

## Install from github
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("https://github.com/USDAForestService/FIESTA", 
		auth_token = token,
		build_vignettes = TRUE,
 		INSTALL_opts = c("--compile-both"),
		dependencies=c("Depends", "Imports"))
```



Note: For Macintosh computers, you may need to install xcode developer tools first
Open Terminal and run the following code:
```
xcode-select --install
```


## HELP and vignettes

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
