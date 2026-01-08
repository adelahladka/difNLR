# ICC plots for an object of `"difORD"` class.

Plot method for an object of `"difORD"` class using ggplot2.

The characteristic curves (category probabilities) for an item specified
in `item` argument are plotted. Plotted curves represent the best model.
For cumulative logit model, also cumulative probabilities may be
plotted.

## Usage

``` r
# S3 method for class 'difORD'
plot(x, item = "all", plot.type, group.names, ...)
```

## Arguments

- x:

  an object of `"difORD"` class.

- item:

  numeric or character: either character `"all"` to apply for all
  converged items (default), or a vector of item names (column names of
  `Data`), or item identifiers (integers specifying the column number).

- plot.type:

  character: which plot should be displayed for cumulative logit
  regression model. Either `"category"` (default) for category
  probabilities or `"cumulative"` for cumulative probabilities.

- group.names:

  character: names of reference and focal group.

- ...:

  other generic parameters for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function.

## Value

Returns list of objects of class `"ggplot"`.

## See also

[`difORD`](https://adelahladka.github.io/difNLR/reference/difORD.md) for
DIF detection among ordinal data.  
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) for
general function to plot a `"ggplot"` object.

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

# graphical devices
plot(x, item = 6)
plot(x, item = "R6", group.names = c("Males", "Females"))

# testing both DIF effects with cumulative logit model
(x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
plot(x, item = 7, plot.type = "cumulative")
plot(x, item = 7, plot.type = "category")
} # }
```
