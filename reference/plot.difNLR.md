# ICC and test statistics plots for an object of the `"difNLR"` class.

A plotting method for an object of the `"difNLR"` class using the
ggplot2 package.

Two types of plots are available. The first one is obtained by setting
`plot.type = "cc"` (default). The characteristic curves for items
specified in the `item` argument are plotted. Plotted curves represent
the best fitted model.

The second plot is obtained by setting `plot.type = "stat"`. The test
statistics (either LR-test, F-test, or Wald test; depending on argument
`test`) are displayed on the Y axis, for each converged item. The
detection threshold is displayed by a horizontal line and items detected
as DIF are printed with the red color. Only parameters `size` and
`title` are used.

## Usage

``` r
# S3 method for class 'difNLR'
plot(
  x,
  plot.type = "cc",
  item = "all",
  group.names,
  draw.empirical = TRUE,
  draw.CI = FALSE,
  ...
)
```

## Arguments

- x:

  an object of the `"difNLR"` class.

- plot.type:

  character: a type of a plot to be plotted (either `"cc"` for
  characteristic curves (default), or `"stat"` for test statistics).

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  the `Data`), or item identifiers (integers specifying the column
  number).

- group.names:

  character: names of the reference and focal groups.

- draw.empirical:

  logical: should empirical probabilities be plotted as points? (the
  default value is `TRUE`).

- draw.CI:

  logical: should confidence intervals for predicted values be plotted?
  (the default value is `FALSE`).

- ...:

  other generic parameters for the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method.

## Value

For an option `plot.type = "stat"`, returns object of the `"ggplot"`
class. In the case of `plot.type = "cc"`, returns a list of objects of
the `"ggplot"` class.

Outputs can be edited and modified as a standard `"ggplot"` object
including colours, titles, shapes, or linetypes.

Note that the option `draw.CI = TRUE` returns confidence intervals for
predicted values as calculated by the
[`predict.difNLR`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md).
Confidence intervals may overlap even in the case that item functions
differently.

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
[`predict.difNLR`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md)
for prediction.
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) for a
general function to plot with the "ggplot2" package.

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

# item characteristic curves
plot(x)
plot(x, item = x$DIFitems)
plot(x, item = 1)
plot(x, item = "Item2", group.names = c("Group 1", "Group 2"))

# item characteristic curves without empirical probabilities
plot(x, item = 1, draw.empirical = FALSE)

# item characteristic curves without empirical probabilities but with CI
plot(x, item = 1, draw.empirical = FALSE, draw.CI = TRUE)

# graphical devices - test statistics
plot(x, plot.type = "stat")
} # }
```
