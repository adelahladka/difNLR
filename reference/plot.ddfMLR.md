# ICC plots for an object of `"ddfMLR"` class.

Plot method for an object of `"ddfMLR"` class using ggplot2.

The characteristic curves for an item specified in `item` argument are
plotted. Plotted curves represent the best model.

## Usage

``` r
# S3 method for class 'ddfMLR'
plot(x, item = "all", group.names, ...)
```

## Arguments

- x:

  an object of `"ddfMLR"` class.

- item:

  numeric or character: either character `"all"` to apply for all items
  (default), or a vector of item names (column names of `Data`), or item
  identifiers (integers specifying the column number).

- group.names:

  character: names of reference and focal group.

- ...:

  other generic parameters for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function.

## Value

Returns list of objects of class `"ggplot"`.

## See also

[`ddfMLR`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md) for
DDF detection.  
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
data(GMATtest, GMATkey)
Data <- GMATtest[, 1:20] # items
group <- GMATtest[, "group"] # group membership variable
key <- GMATkey # correct answers

# testing both DDF effects
(x <- ddfMLR(Data, group, focal.name = 1, key))

# graphical devices
plot(x, item = "Item1", group.names = c("Group 1", "Group 2"))
plot(x, item = x$DDFitems)
plot(x, item = c(3, 1, 5))
} # }
```
