
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
                  "aids",          "AIDS or HIV",                  "^B20|^B21|^B22|^B24",      "^042|^043|^044",     7,
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

    #> 
    #> To cite package 'regstudies' in publications use:
    #> 
    #>   Juho Kopra, Jani Miettinen and Reijo Sund (2020). regstudies: Tools
    #>   for analyzing register data. R package version 0.5.1.
    #>   https://github.com/pohjois-savon-tietoallas/regstudies
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {regstudies: Tools for analyzing register data},
    #>     author = {Juho Kopra and Jani Miettinen and Reijo Sund},
    #>     year = {2020},
    #>     note = {R package version 0.5.1},
    #>     url = {https://github.com/pohjois-savon-tietoallas/regstudies},
    #>   }

## Developers, check [contributing guide](CONTRIBUTING.html)

## References

The current version (0.5.2) contains classification tables for
Elixhauser and Charlson comorbidity indices. The Elixhauser and Charlson
classifications for ICD-9-CM and ICD-10 versions were originally
implemented as regular expressions in
[`comorbidity`](https://ellessenne.github.io/comorbidity/) by Gasparini
(2018). They have been extended based on Pylvänäinen et al. (2019) for
Social Security Institution (SII) entitlement and medication
reimbursement codes. The key references for the Elixhauser and Charlson
comorbidity indices are Charlson et al. (1987), Elixhauser et al. (1998)
and Quan et al. (2015). The scores utilised in the same tables are based
on van Walraven et al. (2009) and Quan et al. (2015). All the source
code in `regstudies` are written by original authors.

  - Charlson, Pompei, Ales & MacKenzie (1987) *A new method of
    classifying prognostic comorbidity in longitudinal studies:
    development and validation*. Journal of Chronic Diseases. 40(5),
    373-83. DOI:
    [10.1016/0021-9681(87)90171-8](https://doi.org/10.1016/0021-9681\(87\)90171-8)
  - Elixhauser, Steiner, Harris and Coffey (1998). *Comorbidity measures
    for use with administrative data*. Medical Care, 36(1), 8-27. DOI:
    [10.1097/00005650-199801000-00004](https://doi.org/10.1097/00005650-199801000-00004)
  - Gasparini (2018) *comorbidity: An R package for computing
    comorbidity scores* Journal of Open Source Software, 3(23), 648.
    DOI: [10.21105/joss.00648](https://doi.org/10.21105/joss.00648)
  - Pylväläinen, Talala, Murtola, Taari, Raitanen, Tammela, & Auvinen
    (2019). *Charlson Comorbidity Index Based On Hospital Episode
    Statistics Performs Adequately In Predicting Mortality, But Its
    Discriminative Ability Diminishes Over Time*. Clinical epidemiology,
    11, 923. DOI:
    [10.2147/CLEP.S218697](https://dx.doi.org/10.2147%2FCLEP.S218697)
  - Quan, Sundararajan, Halfon, Fong, Burnand, Luthi, Saunders, Beck,
    Feasby & Ghali (2015) *Coding algorithms for defining comorbidities
    in ICD-9-CM and ICD-10 administrative data*. Medical Care, 43(11):
    pp. 1130-39. DOI:
    [10.1097/01.mlr.0000182534.19832.83](https://doi.org/10.1097/01.mlr.0000182534.19832.83)
  - van Walraven, Austin, Jennings, Quan & Forster (2009). *A
    modification of the Elixhauser comorbidity measures into a point
    system for hospital death using administrative data*. Medical Care,
    47(6), 626-33. DOI:
    [10.1097/mlr.0b013e31819432e5](https://doi.org/10.1097/mlr.0b013e31819432e5)
