# Extract model coefficients from an object of `"difORD"` class.

S3 method for extracting estimated model coefficients from an object of
`"difORD"` class.

## Usage

``` r
# S3 method for class 'difORD'
coef(object, SE = FALSE, simplify = FALSE, IRTpars = TRUE, CI = 0.95, ...)
```

## Arguments

- object:

  an object of `"difORD"` class.

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

[`difORD`](https://adelahladka.github.io/difNLR/reference/difORD.md) for
DIF detection among ordinal data.  
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
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)] # items
group <- Anxiety[, "gender"] # group membership variable

# testing both DIF effects with adjacent category logit model
(x <- difORD(Data, group, focal.name = 1, model = "adjacent"))

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
