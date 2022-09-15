# svBase - Functions for Base Objects like Data Frames in SciViews <a href='https://www.sciviews.org/svBase'><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/svBase/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/svBase/actions/workflows/R-CMD-check.yaml) [![Coverage status](https://img.shields.io/codecov/c/github/SciViews/svBase/main.svg)](https://codecov.io/github/SciViews/svBase?branch=master) [![CRAN status](https://www.r-pkg.org/badges/version/svBase)](https://cran.r-project.org/package=svBase) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

<!-- badges: end -->

{svBase} sets up the way data frames (with objects like R base's **data.frame**, **data.table** and tibble **tbl_df**) are managed in SciViews. The user can select the class of object it uses by default and many other SciViews functions return that format. Also conversion from one to the other is smoothed, including for the management of **data.frame**'s row names or **data.table**'s keys. Also homogeneous ways to create a data frame or to print it are also provided.

## Installation

You can install the released version of {svBase} from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("svBase")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {svMisc} package from GitHub (source from **master** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/svBase")
```

R should install all required dependencies automatically, and then it should compile and install {svBase}.

## Further explore {svBase}

You can get further help about this package this way: Make the {svBase} package available in your R session:

``` r
library("svBase")
```

Get help about this package:

``` r
library(help = "svBase")
help("svBase-package")
vignette("svBase") # None is installed with install_github()
```

For further instructions, please, refer to the help pages at <https://www.sciviews.org/svBase/>.

## Code of Conduct

Please note that the {svBase} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
