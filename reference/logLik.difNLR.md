# Log-likelihood and information criteria for an object of the `"difNLR"` class.

S3 methods for extracting log-likelihood, Akaike's information criterion
(AIC) and Schwarz's Bayesian criterion (BIC) for an object of the
`"difNLR"` class.

## Usage

``` r
# S3 method for class 'difNLR'
logLik(object, item = "all", ...)

# S3 method for class 'difNLR'
AIC(object, item = "all", ...)

# S3 method for class 'difNLR'
BIC(object, item = "all", ...)
```

## Arguments

- object:

  an object of the `"difNLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  the `Data`), or item identifiers (integers specifying the column
  number).

- ...:

  other generic parameters for S3 methods.

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
[`logLik`](https://rdrr.io/r/stats/logLik.html) for a generic function
extracting log-likelihood.  
[`AIC`](https://rdrr.io/r/stats/AIC.html) for a generic function
calculating AIC and BIC.

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

# AIC, BIC, log-likelihood
AIC(x)
BIC(x)
logLik(x)

# AIC, BIC, log-likelihood for the first item
AIC(x, item = 1)
BIC(x, item = 1)
logLik(x, item = 1)
} # }
```
