# Predicted values for an object of the `"difNLR"` class.

S3 method for predictions from the fitted model used in the object of
the `"difNLR"` class.

## Usage

``` r
# S3 method for class 'difNLR'
predict(object, item = "all", match, group, interval = "none", CI = 0.95, ...)
```

## Arguments

- object:

  an object of the `"difNLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  the `Data`), or item identifiers (integers specifying the column
  number).

- match:

  numeric: a matching criterion for new observations.

- group:

  numeric: a group membership variable for new observations.

- interval:

  character: a type of interval calculation, either `"none"` (default)
  or `"confidence"` for confidence interval.

- CI:

  numeric: a significance level for confidence interval (the default is
  `0.95` for 95% confidence interval).

- ...:

  other generic parameters for the
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.

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
[`predict`](https://rdrr.io/r/stats/predict.html) for a generic function
for prediction.

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

# predicted values
summary(predict(x))
predict(x, item = 1)
predict(x, item = "Item1")

# predicted values for new observations - average score
predict(x, item = 1, match = 0, group = 0) # reference group
predict(x, item = 1, match = 0, group = 1) # focal group
predict(x, item = 1, match = 0, group = c(0, 1)) # both groups

# predicted values for new observations - various Z-scores and groups
new.match <- rep(c(-1, 0, 1), each = 2)
new.group <- rep(c(0, 1), 3)
predict(x, item = 1, match = new.match, group = new.group)

# predicted values for new observations with confidence intervals
predict(x, item = 1, match = new.match, group = new.group, interval = "confidence")
predict(x, item = c(2, 4), match = new.match, group = new.group, interval = "confidence")
} # }
```
