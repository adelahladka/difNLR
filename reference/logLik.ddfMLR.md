# Log-likelihood and information criteria for an object of `"ddfMLR"` class.

S3 methods for extracting log-likelihood, Akaike's information criterion
(AIC) and Schwarz's Bayesian criterion (BIC) for an object of `"ddfMLR"`
class.

## Usage

``` r
# S3 method for class 'ddfMLR'
logLik(object, item = "all", ...)

# S3 method for class 'ddfMLR'
AIC(object, item = "all", ...)

# S3 method for class 'ddfMLR'
BIC(object, item = "all", ...)
```

## Arguments

- object:

  an object of `"ddfMLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  `Data`), or item identifiers (integers specifying the column number).

- ...:

  other generic parameters for S3 methods.

## See also

[`ddfMLR`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md) for
DDF detection among nominal data.  
[`logLik`](https://rdrr.io/r/stats/logLik.html) for generic function
extracting log-likelihood.  
[`AIC`](https://rdrr.io/r/stats/AIC.html) for generic function
calculating AIC and BIC.

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
Faculty of Mathematics and Physics, Charles University  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  

## Examples

``` r
if (FALSE) { # \dontrun{
# loading data
data(GMATtest, GMATkey)
Data <- GMATtest[, 1:20] # items
group <- GMATtest[, "group"] # group membership variable
key <- GMATkey # correct answers

# testing both DDF effects
(x <- ddfMLR(Data, group, focal.name = 1, key))

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
