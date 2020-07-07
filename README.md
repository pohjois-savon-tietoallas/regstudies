
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Register studies

<!-- badges: start -->

<!-- badges: end -->

Data utilised in register studies, often called as register-based data
or administrative data, typically consists of id’s, time stamp variables
and codes describing different events. This package has been designed
specifically for handling of such data.

Package is based on tidyverse and examples utilise widely tidyverse
programming style. The functions are designed with ICD codes in mind,
but can be used with any kind of string formatted codes and are thus not
necessarily limited to healthcare data. Users are allowed to make their
own definitions of code classifiers, so the package suits needs of
different countries having their own code definitions of disease codes
for example.

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
devtools::install_github("pohjois-savon-tietoallas/regstudies")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(regstudies)

# ´regstudies´ makes it easy to classify codes such as ICD-codes to groups and also to sum scores based on the groupings.
sample_data <- left_join(sample_cohort,sample_regdata)
sample_data %>% 
  classify_charlson(icd_codes = CODE1) %>%
  sum_score(score_charlson)

# Users can define the code groups themselves using simple regular expression syntax.
my_classes <- 
  tribble(~class_myclass,         ~label_myclass,                                 ~icd10,                 ~icd9, ~score,
                  "aids",          "AIDS or HIV",                  "^B20|^B21|^B22|^B24",      "^042|^043|^044",      7,
              "dementia",             "Dementia", "^F00|^F01|^F02|^F03|^F051|^G30|^G311",    "^290|^2941|^3312",      3,
                   "pud", "Peptic ulcer disease",                  "^K25|^K26|^K27|^K28", "^531|^532|^533|^534",      1
          )

# This classification definition has two different type of codes.
head(my_classes)

# Data has three different types of codes.
head(sample_data)

# Different groups types can are automatically handled simultaneously.
sample_data %>% 
  classify_codes(codes=CODE1,diag_tbl = read_classes(my_classes)) %>%
  sum_score(score)
```

## Citation

To cite package ‘regstudies’ in publications use:

Juho Kopra, Jani Miettinen and Reijo Sund (2020). regstudies: Tools for
analyzing register data. R package version 0.5.1.

A BibTeX entry for LaTeX users is

``` 
  @Manual{,
    title = {regstudies: Tools for analyzing register data},
    author = {Juho Kopra and Jani Miettinen and Reijo Sund},
    year = {2020},
    note = {R package version 0.5.1},
  }
```

For developers:

  - [release notes](http://r-pkgs.had.co.nz/release.html)
  - [pkgdown](https://pkgdown.r-lib.org/articles/pkgdown.html)
  - [pkg dev](http://r-pkgs.had.co.nz/)
