# UBL
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/paobranco/UBL.svg?branch=master)](https://travis-ci.org/paobranco/UBL)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/UBL)](https://cran.r-project.org/package=UBL)
[![Downloads](https://cranlogs.r-pkg.org/badges/UBL)](https://cran.rstudio.com/web/packages/UBL/)


An R package implementing several pre-processing approaches to utility-based learning, both for classification and regression predictive tasks

**To Install the Latest Stable  Release (from github) do the following in R:**

    library(devtools)  # You need to install this package!
    install_github("paobranco/UBL",ref="master")


**To Install the Latest Development Release (from github) do the following in R:**

    library(devtools)  # You need to install this package!
    install_github("paobranco/UBL",ref="development")

If this previous install_github calls somehow fail (there are reports of problems with different libcurl library version on Linux hosts) you may try in alternative the following in R:

    library(devtools)
    install_git("https://github.com/paobranco/UBL",branch="development")
    install_git("https://github.com/paobranco/UBL",branch="master")


After installation using any of the above procedures, the package can be used as any other R package by doing:

     library(UBL)


Further help and illustrations can be obtained through the many help pages of each function defined in the package that contain lots of illustrative examples. Again, these help pages can be accessed as any other R package, through R help system (e.g. running **help.start()** at R command line)
