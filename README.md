
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Register studies

<!-- badges: start -->

<!-- badges: end -->

Data utilised in register studies, often called as register-based data
or administrative data, typically consists of id’s, time stamp variables
and codes describing different events. This package has been designed
specifically for handling of such data.

regstudies-package is based on tidyverse and examples utilise widely
tidyverse programming style. The functions are designed with ICD codes
in mind, but can be used with any kind of string formatted codes and are
thus not necessarily limited to healthcare data. Users are allowed to
make their own definitions of code classifiers, so the package suits
needs of different countries having their own code definitions of
disease codes for example.

Authors: [Juho Kopra](https://github.com/jukop), [Jani
Miettinen](https://github.com/janikmiet), [Reijo
Sund](https://github.com/rsund)

## Installation

You can install the released version of regstudies from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("regstudies") # maybe some day!! 
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jukop/regstudies")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(regstudies)
## basic example code
```

For developers:

  - [release notes](http://r-pkgs.had.co.nz/release.html)
