# difNLR
DIF and DDF Detection by Non-Linear Regression Models.

![GHversion](https://img.shields.io/github/release/drabinova/difNLR.svg)
[![version](https://www.r-pkg.org/badges/version/difNLR)](https://CRAN.R-project.org/package=difNLR)
![cranlogs](https://cranlogs.r-pkg.org/badges/difNLR)

## Description

The difNLR package containts method for detection of differential item functioning (DIF)
based on non-linear regression. Both uniform and non-uniform DIF effects can be detected when considering one focal group. The method also allows to test the difference in guessing or inattention parameters between reference and focal group. DIF detection method is based either on likelihood-ratio test, or on F-test of submodel. Package also offers method for detection of differential distractor functioning (DDF) based on multinomial log-linear regression model.

## Installation
The easiest way to get `difNLR` package is to install it from CRAN:
```
install.packages("difNLR")
```
Or you can get the newest development version from GitHub:
```
# install.packages("devtools")
devtools::install_github("drabinova/difNLR")
```
## Version
Current version on [**CRAN**](https://CRAN.R-project.org/package=difNLR) is 1.2.2. The newest development version available on [**GitHub**](https://github.com/drabinova/difNLR) is 1.2.2.

## Reference

To cite `difNLR` package in publications, please, use:

  Drabinova A., Martinkova P., & Zvara K. (2018). difNLR: DIF and DDF detection by non-linear
  regression models. R package version 1.2.2. [https://CRAN.R-project.org/package=difNLR]( https://CRAN.R-project.org/package=difNLR)

  Drabinova A., & Martinkova P. (2017). Detection of Differential Item Functioning with
  Nonlinear Regression: A Non-IRT Approach Accounting for Guessing. Journal of
  Educational Measurement, 54(4), 498-517. [DOI: 10.1111/jedm.12158](http://onlinelibrary.wiley.com/doi/10.1111/jedm.12158/full).
  
## Getting help
In case you find any bug or just need help with `difNLR` package, you can leave your message as an issue here or directly contact us at drabinova@cs.cas.cz
