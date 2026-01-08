# Predicted values for an object of `"difORD"` class.

S3 method for predictions from the model used in the object of
`"difORD"` class.

## Usage

``` r
# S3 method for class 'difORD'
predict(object, item = "all", match, group, type = "category", ...)
```

## Arguments

- object:

  an object of `"difORD"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  `Data`), or item identifiers (integers specifying the column number).

- match:

  numeric: matching criterion for new observations.

- group:

  numeric: group membership for new observations.

- type:

  character: type of probability to be computed. Either `"category"` for
  category probabilities or `"cumulative"` for cumulative probabilities.
  Cumulative probabilities are available only for cumulative logit
  model.

- ...:

  other generic parameters for
  [`predict()`](https://rdrr.io/r/stats/predict.html) function.

## References

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

## See also

[`difORD`](https://adelahladka.github.io/difNLR/reference/difORD.md) for
DIF detection among ordinal data using either cumulative logit or
adjacent category logit model.  
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
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)] # items
group <- Anxiety[, "gender"] # group membership variable

# testing both DIF effects with cumulative logit model
(x <- difORD(Data, group, focal.name = 1, model = "cumulative"))

# fitted values
predict(x, item = "R6")

# predicted values
predict(x, item = "R6", match = 0, group = c(0, 1))
predict(x, item = "R6", match = 0, group = c(0, 1), type = "cumulative")
predict(x, item = c("R6", "R7"), match = 0, group = c(0, 1))

# testing both DIF effects with adjacent category logit model
(x <- difORD(Data, group, focal.name = 1, model = "adjacent"))

# fitted values
predict(x, item = "R6")

# predicted values
predict(x, item = "R6", match = 0, group = c(0, 1))
predict(x, item = c("R6", "R7"), match = 0, group = c(0, 1))
} # }
```
