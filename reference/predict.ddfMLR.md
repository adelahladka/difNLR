# Predicted values for an object of `"ddfMLR"` class.

S3 method for predictions from the model used in the object of
`"ddfMLR"` class.

## Usage

``` r
# S3 method for class 'ddfMLR'
predict(object, item = "all", match, group, ...)
```

## Arguments

- object:

  an object of `"ddfMLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  `Data`), or item identifiers (integers specifying the column number).

- match:

  numeric: matching criterion for new observations.

- group:

  numeric: group membership for new observations.

- ...:

  other generic parameters for
  [`predict()`](https://rdrr.io/r/stats/predict.html) function.

## References

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

## See also

[`ddfMLR`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md) for
DDF detection among nominal data using multinomial log-linear regression
model.  
[`predict`](https://rdrr.io/r/stats/predict.html) for generic function
for prediction.

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
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

# fitted values
predict(x, item = 1)

# predicted values
predict(x, item = 1, match = 0, group = c(0, 1))
predict(x, item = x$DDFitems, match = 0, group = c(0, 1))
} # }
```
