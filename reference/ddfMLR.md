# DDF detection for nominal data.

Performs DDF detection procedure for nominal data based on multinomial
log-linear regression model and likelihood ratio test of a submodel.

## Usage

``` r
ddfMLR(Data, group, focal.name, key, type = "both", match = "zscore", anchor = NULL,
       purify = FALSE, nrIter = 10, p.adjust.method = "none",
       alpha = 0.05, parametrization)
```

## Arguments

- Data:

  data.frame or matrix: dataset which rows represent unscored examinee
  answers (nominal) and columns correspond to the items. In addition,
  `Data` can hold the vector of group membership.

- group:

  numeric or character: a dichotomous vector of the same length as
  `nrow(Data)` or a column identifier of `Data`.

- focal.name:

  numeric or character: indicates the level of `group` which corresponds
  to focal group.

- key:

  character: the answer key. Each element corresponds to the correct
  answer of one item.

- type:

  character: type of DDF to be tested. Either `"both"` for uniform and
  non-uniform DDF (i.e., difference in parameters `"a"` and `"b"`)
  (default), or `"udif"` for uniform DDF only (i.e., difference in
  difficulty parameter `"b"`), or `"nudif"` for non-uniform DDF only
  (i.e., difference in discrimination parameter `"a"`). Can be specified
  as a single value (for all items) or as an item-specific vector.

- match:

  character or numeric: matching criterion to be used as an estimate of
  the trait. It can be either `"zscore"` (default; standardized total
  score), `"score"` (total test score), `"restscore"` (total score
  without the tested item), `"zrestscore"` (standardized total score
  without the tested item), a numeric vector of the same length as a
  number of observations in the `Data`, or a numeric matrix of the same
  dimensions as `Data` (each column represents matching criterion for
  one item).

- anchor:

  character or numeric: specification of DIF-free (anchor) items used to
  compute the matching criterion (`match`). Can be either `NULL`
  (default; all items are used for the calculation), or a vector of item
  identifiers (integers indicating column numbers or item names in
  \`Data\`) specifying which items are currently considered as anchor
  items. This argument is ignored if the `match` is not `"zscore"`,
  `"score"`, `"restscore"`, or `"zrestscore"`. For `match = "score"` or
  `match = "zscore"`, the matching criterion is computed from the items
  specified in the anchor set. For `match = "restscore"` or
  `match = "zrestscore"`, the same anchor items are used, except that
  the item currently under test is excluded from the computation.

- purify:

  logical: should the item purification be applied? (default is
  `FALSE`). Item purification is not applied when set of anchor items in
  `anchor` is specified or when `match` is not `"zscore"`, `"score"`,
  `"restscore"`, or `"zrestscore"`.

- nrIter:

  numeric: the maximal number of iterations in the item purification
  (default is 10).

- p.adjust.method:

  character: method for multiple comparison correction. Possible values
  are `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`,
  `"BY"`, `"fdr"`, and `"none"` (default). For more details see
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- alpha:

  numeric: significance level (default is 0.05).

- parametrization:

  deprecated. Use
  [`coef.ddfMLR`](https://adelahladka.github.io/difNLR/reference/coef.ddfMLR.md)
  for different parameterizations.

## Value

The `ddfMLR()` function returns an object of class `"ddfMLR"`. The
output including values of the test statistics, p-values, and items
marked as DDF is displayed by the
[`print()`](https://rdrr.io/r/base/print.html) method.

A list of class `"ddfMLR"` with the following arguments:

- `Sval`:

  the values of likelihood ratio test statistics.

- `mlrPAR`:

  the estimates of final model.

- `mlrSE`:

  standard errors of the estimates of final model.

- `parM0`:

  the estimates of null model.

- `parM1`:

  the estimates of alternative model.

- `llM0`:

  log-likelihood of null model.

- `llM1`:

  log-likelihood of alternative model.

- `AIC0`:

  AIC of null model.

- `AIC1`:

  AIC of alternative model.

- `BIC0`:

  BIC of null model.

- `BIC1`:

  BIC of alternative model.

- `DDFitems`:

  either the column identifiers of the items which were detected as DDF,
  or `"No DDF item detected"` in case no item was detected as DDF.

- `type`:

  character: type of DDF that was tested.

- `anchor`:

  DIF free items specified by the `anchor` and `purify`.

- `purification`:

  `purify` value.

- `nrPur`:

  number of iterations in item purification process. Returned only if
  `purify` is `TRUE`.

- `ddfPur`:

  a binary matrix with one row per iteration of item purification and
  one column per item. `"1"` in i-th row and j-th column means that j-th
  item was identified as DDF in i-th iteration. Returned only if
  `purify` is `TRUE`.

- `conv.puri`:

  logical indicating whether item purification process converged before
  the maximal number `nrIter` of iterations. Returned only if `purify`
  is `TRUE`.

- `p.adjust.method`:

  character: method for multiple comparison correction which was
  applied.

- `pval`:

  the p-values by likelihood ratio test.

- `adj.pval`:

  the adjusted p-values by likelihood ratio test using
  `p.adjust.method`.

- `df`:

  the degress of freedom of likelihood ratio test.

- `alpha`:

  numeric: significance level.

- `Data`:

  the data matrix.

- `group`:

  the vector of group membership.

- `group.names`:

  levels of grouping variable.

- `key`:

  key of correct answers.

- `match`:

  matching criterion.

- `match.name`:

  Name of the matching criterion.

For an object of class `"ddfMLR"` several methods are available (e.g.
`methods(class = "ddfMLR")`).

## Details

Performs DDF detection procedure for nominal data based on multinomial
log-linear regression model and likelihood ratio test of submodel.
Probability of selection the \\k\\-th category (distractor) is \$\$P(y =
k) = exp((a_k + a_kDif \* g) \* (x - b_k - b_kDif \* g))) / (1 + \sum
exp((a_l + a_lDif \* g) \* (x - b_l - b_lDif \* g))), \$\$ where \\x\\
is by default standardized total score (also called Z-score) and \\g\\
is a group membership. Parameters \\a_k\\ and \\b_k\\ are discrimination
and difficulty for the \\k\\-th category. Terms \\a_kDif\\ and
\\b_kDif\\ then represent differences between two groups (reference and
focal) in relevant parameters. Probability of correct answer (specified
in argument `key`) is \$\$P(y = k) = 1/(1 + \sum exp((a_l + a_lDif \*
g)\*(x - b_l - b_lDif \* g))). \$\$ Parameters are estimated via neural
networks. For more details see
[`multinom`](https://rdrr.io/pkg/nnet/man/multinom.html).

Missing values are allowed but discarded for item estimation. They must
be coded as `NA` for both, `Data` and `group` arguments.

## References

Agresti, A. (2010). Analysis of ordinal categorical data. Second
edition. John Wiley & Sons.

Hladka, A. (2021). Statistical models for detection of differential item
functioning. Dissertation thesis. Faculty of Mathematics and Physics,
Charles University.

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

## See also

[`plot.ddfMLR`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
for graphical representation of item characteristic curves.  
[`coef.ddfMLR`](https://adelahladka.github.io/difNLR/reference/coef.ddfMLR.md)
for extraction of item parameters with their standard errors.  
[`logLik.ddfMLR`](https://adelahladka.github.io/difNLR/reference/logLik.ddfMLR.md),
[`AIC.ddfMLR`](https://adelahladka.github.io/difNLR/reference/logLik.ddfMLR.md),
[`BIC.ddfMLR`](https://adelahladka.github.io/difNLR/reference/logLik.ddfMLR.md)
for extraction of log-likelihood and information criteria.  

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for multiple
comparison corrections.  
[`multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) for estimation
function using neural networks.

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
plot(x, item = 1)

# estimated parameters
coef(x)
coef(x, SE = TRUE)
coef(x, SE = TRUE, simplify = TRUE)

# AIC, BIC, log-likelihood
AIC(x)
BIC(x)
logLik(x)

# AIC, BIC, log-likelihood for the first item
AIC(x, item = 1)
BIC(x, item = 1)
logLik(x, item = 1)

# testing both DDF effects with Benjamini-Hochberg adjustment method
ddfMLR(Data, group, focal.name = 1, key, p.adjust.method = "BH")

# testing both DDF effects with item purification
ddfMLR(Data, group, focal.name = 1, key, purify = TRUE)

# testing uniform DDF effects
ddfMLR(Data, group, focal.name = 1, key, type = "udif")
# testing non-uniform DDF effects
ddfMLR(Data, group, focal.name = 1, key, type = "nudif")

# testing both DDF effects with different matching criteria
ddfMLR(Data, group, focal.name = 1, key, match = "score")
ddfMLR(Data, group, focal.name = 1, key, match = "restscore")
ddfMLR(Data, group, focal.name = 1, key, match = "zrestscore")
match <- rowSums(GMAT[, 1:20])
ddfMLR(Data, group, focal.name = 1, key, match = match)
match <- replicate(ncol(Data), GMAT$criterion)
ddfMLR(Data, group, focal.name = 1, key, match = match)
match <- as.data.frame(match)
ddfMLR(Data, group, focal.name = 1, key, match = match)
} # }
```
