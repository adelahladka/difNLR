# Extract item parameter estimates from an object of the `"difNLR"` class.

S3 method for extracting the item parameter estimates from an object of
the `"difNLR"` class.

## Usage

``` r
# S3 method for class 'difNLR'
coef(
  object,
  item = "all",
  SE = FALSE,
  simplify = FALSE,
  IRTpars = TRUE,
  CI = 0.95,
  ...
)
```

## Arguments

- object:

  an object of the `"difNLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  the `Data`), or item identifiers (integers specifying the column
  number).

- SE:

  logical: should the standard errors of the estimated item parameters
  be also returned? (the default is `FALSE`).

- simplify:

  logical: should the estimated item parameters be simplified to a
  matrix? (the default is `FALSE`).

- IRTpars:

  logical: should the estimated item parameters be returned in he IRT
  parameterization? (the default is `TRUE`).

- CI:

  numeric: a significance level for confidence intervals (CIs) of item
  parameter estimates (the default is `0.95` for 95% CI). With 0 value,
  no CIs are displayed.

- ...:

  other generic parameters for the
  [`coef()`](https://rdrr.io/r/stats/coef.html) method.

## References

Drabinova, A. & Martinkova, P. (2017). Detection of differential item
functioning with nonlinear regression: A non-IRT approach accounting for
guessing. Journal of Educational Measurement, 54(4), 498–517,
[doi:10.1111/jedm.12158](https://doi.org/10.1111/jedm.12158) .

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

## See also

[`difNLR`](https://adelahladka.github.io/difNLR/reference/difNLR.md) for
DIF detection among binary data using the generalized logistic
regression model.  
[`coef`](https://rdrr.io/r/stats/coef.html) for a generic function for
extracting parameter estimates.

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  

Karel Zvara  
Faculty of Mathematics and Physics, Charles University  

## Examples

``` r
if (FALSE) { # \dontrun{
# loading data
data(GMAT)
Data <- GMAT[, 1:20] # items
group <- GMAT[, "group"] # group membership variable

# testing both DIF effects using likelihood-ratio test and
# 3PL model with fixed guessing for groups
(x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))

# estimated parameters
coef(x)
# includes standard errors
coef(x, SE = TRUE)
# includes standard errors and simplifies to matrix
coef(x, SE = TRUE, simplify = TRUE)
# intercept-slope parameterization
coef(x, IRTpars = FALSE)
# intercept-slope parameterization, simplifies to matrix, turn off confidence intervals
coef(x, IRTpars = FALSE, simplify = TRUE, CI = 0)
# for DIF items only
coef(x, item = x$DIFitems, IRTpars = FALSE, simplify = TRUE, CI = 0)
} # }
```
