# difNLR
DIF and DDF Detection by Non-Linear Regression Models.

![GHversion](https://img.shields.io/github/release/adelahladka/difNLR.svg)
[![version](https://www.r-pkg.org/badges/version/difNLR)](https://CRAN.R-project.org/package=difNLR)
![cranlogs](https://cranlogs.r-pkg.org/badges/difNLR)

## Description
The difNLR package containts method for detection of differential item functioning (DIF) based on non-linear regression. Both uniform and non-uniform DIF effects can be detected when considering one focal group. The method also allows to test the difference in guessing or inattention parameters between reference and focal group. DIF detection method is based either on likelihood-ratio test, or on F-test of submodel. Package also offers methods for detection of differential distractor functioning (DDF) based on multinomial log-linear regression model and newly methods for DIF detection among ordinal data via adjacent category and cumulative logist regression models.

<p align="center">
  <img src="inst/DIF_NLR.png" width=33%/> 
  <img src="inst/DDF_CLRM_cumulative.png" width=33%/> 
  <img src="inst/DDF_CLRM_category.png" width=33%/> 
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
Current version on [**CRAN**](https://CRAN.R-project.org/package=difNLR) is 1.3.3. The newest development version available on [**GitHub**](https://github.com/drabinova/difNLR) is 1.3.3.

## Reference
To cite `difNLR` package in publications, please, use:

  Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic regression models for DIF and DDF detection. 
  The R journal. Under review.

  Drabinova, A. & Martinkova, P. (2017). Detection of Differential Item Functioning with
  Nonlinear Regression: A Non-IRT Approach Accounting for Guessing. Journal of
  Educational Measurement, 54(4), 498-517. [DOI: 10.1111/jedm.12158](https://doi.org/10.1111/jedm.12158).
  
## Try online
You can try some functionalities of the `difNLR` package [online](https://shiny.cs.cas.cz/ShinyItemAnalysis/) using [`ShinyItemAnalysis`](https://github.com/patriciamar/ShinyItemAnalysis) application and package and its DIF/Fairness section.
  
## Getting help
In case you find any bug or just need help with `difNLR` package, you can leave your message as an issue here or directly contact us at hladka@cs.cas.cz
