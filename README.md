# difNLR
DIF and DDF Detection by Non-Linear Regression Models.

[![R-CMD-check](https://github.com/adelahladka/difNLR/workflows/R-CMD-check/badge.svg)](https://github.com/adelahladka/difNLR/actions)
[![Codecov test coverage](https://codecov.io/gh/adelahladka/difNLR/graph/badge.svg)](https://app.codecov.io/gh/adelahladka/difNLR)
![GHversion](https://img.shields.io/github/release/adelahladka/difNLR.svg)
[![version](https://www.r-pkg.org/badges/version/difNLR)](https://CRAN.R-project.org/package=difNLR)
![cranlogs](https://cranlogs.r-pkg.org/badges/difNLR)

## Description
The `difNLR` package provides methods for detecting differential item
functioning (DIF) using non-linear regression models. Both uniform and
non-uniform DIF effects can be detected when considering a single focal group.
Additionally, the method allows for testing differences in guessing or
inattention parameters between the reference and focal group. DIF detection is
performed using either a likelihood-ratio test, an F-test, or Wald's test of a
submodel. The software offers a variety of algorithms for estimating item
parameters.

Furthermore, the `difNLR` package includes methods for detecting differential
distractor functioning (DDF) using multinomial log-linear regression model. It
also introduces DIF detection approaches for ordinal data via adjacent category
logit and cumulative logit regression models.

<p align="center">
  <img src="inst/DIF_NLR.png" width=32%/> 
  <img src="inst/DDF_CLRM_cumulative.png" width=32%/> 
  <img src="inst/DDF_CLRM_category.png" width=32%/> 
</p>


## Installation
The easiest way to get `difNLR` package is to install it from CRAN:
```
install.packages("difNLR")
```
Or you can get the newest development version from GitHub:
```
# install.packages("devtools")
devtools::install_github("adelahladka/difNLR")
```
## Version
Current version on [**CRAN**](https://CRAN.R-project.org/package=difNLR) is
1.5.1-1. The newest development version available on
[**GitHub**](https://github.com/adelahladka/difNLR) is 1.5.1-3.

## Reference
To cite `difNLR` package in publications, please, use:

<ul>Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression models for DIF and DDF detection. 
  <i>The R Journal, 12</i>(1), 300--323, 
  https://doi.org/10.32614/RJ-2020-014</ul>

<ul>Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with
  Nonlinear Regression: A Non-IRT Approach Accounting for Guessing. 
  <i>Journal of Educational Measurement, 54</i>(4), 498--517, 
  https://doi.org/10.1111/jedm.12158</ul>
  

To cite new estimation approaches provided in the `difNLR()` function, please, use:

<ul>Hladka, A., Martinkova, P., & Brabec, M. (2025). New iterative algorithms for estimation of item functioning. 
  <i>Journal of Educational and Behavioral Statistics. </i> 
  Online first, https://doi.org/10.3102/10769986241312354</ul>
  
## Try online
You can try some functionalities of the `difNLR` package
[online](https://shiny.cs.cas.cz/ShinyItemAnalysis/) using
[`ShinyItemAnalysis`](https://github.com/patriciamar/ShinyItemAnalysis)
application and package and its DIF/Fairness section.
  
## Getting help
In case you find any bug or just need help with the `difNLR` package, you can leave
your message as an issue here or directly contact us at hladka@cs.cas.cz
