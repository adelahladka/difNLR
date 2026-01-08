# Extract model coefficients from an object of `"ddfMLR"` class.

S3 method for extracting estimated model coefficients from an object of
`"ddfMLR"` class.

## Usage

``` r
# S3 method for class 'ddfMLR'
coef(object, SE = FALSE, simplify = FALSE, IRTpars = TRUE, CI = 0.95, ...)
```

## Arguments

- object:

  an object of `"ddfMLR"` class.

- SE:

  logical: should the standard errors of estimated parameters be also
  returned? (default is `FALSE`).

- simplify:

  logical: should the estimated parameters be simplified to a matrix?
  (default is `FALSE`).

- IRTpars:

  logical: should the estimated parameters be returned in IRT
  parameterization? (default is `TRUE`).

- CI:

  numeric: level of confidence interval for parameters, default is
  `0.95` for 95% confidence interval.

- ...:

  other generic parameters for
  [`coef()`](https://rdrr.io/r/stats/coef.html) function.

## See also

[`ddfMLR`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md) for
DDF detection among nominal data.  
[`coef`](https://rdrr.io/r/stats/coef.html) for generic function
extracting model coefficients.

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
} # }
```
