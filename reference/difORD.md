# DIF detection among ordinal data.

Performs DIF detection procedure for ordinal data based either on
adjacent category logit model or on cumulative logit model and
likelihood ratio test of a submodel.

## Usage

``` r
difORD(Data, group, focal.name, model = "adjacent", type = "both", match = "zscore",
       anchor = NULL, purify = FALSE, nrIter = 10, p.adjust.method = "none",
       alpha = 0.05, parametrization)
```

## Arguments

- Data:

  data.frame or matrix: dataset which rows represent ordinally scored
  examinee answers and columns correspond to the items. In addition,
  `Data` can hold the vector of group membership.

- group:

  numeric or character: a dichotomous vector of the same length as
  `nrow(Data)` or a column identifier of `Data`.

- focal.name:

  numeric or character: indicates the level of `group` which corresponds
  to focal group.

- model:

  character: logistic regression model for ordinal data (either
  `"adjacent"` (default) or `"cumulative"`). See **Details**.

- type:

  character: type of DIF to be tested. Either `"both"` for uniform and
  non-uniform DIF (default), or `"udif"` for uniform DIF only, or
  `"nudif"` for non-uniform DIF only. Can be specified as a single value
  (for all items) or as an item-specific vector.

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
  [`coef.difORD`](https://adelahladka.github.io/difNLR/reference/coef.difORD.md)
  for different parameterizations.

## Value

The `difORD()` function returns an object of class `"difORD"`. The
output including values of the test statistics, p-values, and items
marked as DIF is displayed by the
[`print()`](https://rdrr.io/r/base/print.html) method.

A list of class `"difORD"` with the following arguments:

- `Sval`:

  the values of likelihood ratio test statistics.

- `ordPAR`:

  the estimates of the final model.

- `ordSE`:

  standard errors of the estimates of the final model.

- `parM0`:

  the estimates of null model.

- `parM1`:

  the estimates of alternative model.

- `llM0`:

  log-likelihood of null model.

- `llM1`:

  log-likelihood of alternative model.

- `AICM0`:

  AIC of null model.

- `AICM1`:

  AIC of alternative model.

- `BICM0`:

  BIC of null model.

- `BICM1`:

  BIC of alternative model.

- `DIFitems`:

  either the column identifiers of the items which were detected as DIF,
  or `"No DIF item detected"` in case no item was detected as DIF.

- `model`:

  model used for DIF detection.

- `type`:

  character: type of DIF that was tested.

- `anchor`:

  DIF free items specified by the `anchor` and `purify`.

- `purification`:

  `purify` value.

- `nrPur`:

  number of iterations in item purification process. Returned only if
  `purify` is `TRUE`.

- `difPur`:

  a binary matrix with one row per iteration of item purification and
  one column per item. `"1"` in i-th row and j-th column means that j-th
  item was identified as DIF in i-th iteration. Returned only if
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

- `match`:

  matching criterion.

- `match.name`:

  Name of the matching criterion.

For an object of class `"difORD"` several methods are available (e.g.,
`methods(class = "difORD")`).

## Details

Calculates DIF likelihood ratio statistics based either on adjacent
category logit model or on cumulative logit model for ordinal data.

Using adjacent category logit model, logarithm of ratio of probabilities
of two adjacent categories is \$\$log(P(y = k) / P(y = k - 1)) = b_0k +
b_1 \* x + b_2k \* g + b_3 \* x:g,\$\$ where \\x\\ is by default
standardized total score (also called Z-score) and \\g\\ is a group
membership.

Using cumulative logit model, probability of gaining at least \\k\\
points is given by 2PL model, i.e., \$\$P(y \>= k) = exp(b_0k + b_1 \*
x + b_2k \* g + b_3 \* x:g) / (1 + exp(b_0k + b_1 \* x + b_2k \* g + b_3
\* x:g)).\$\$ The category probability (i.e., probability of gaining
exactly \\k\\ points) is then \\P(y = k) = P(y \>= k) - P(y \>= k +
1)\\.

Both models are estimated by iteratively reweighted least squares. For
more details see [`vglm`](https://rdrr.io/pkg/VGAM/man/vglm.html).

Missing values are allowed but discarded for item estimation. They must
be coded as `NA` for both, `Data` and `group` parameters.

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

[`plot.difORD`](https://adelahladka.github.io/difNLR/reference/plot.difORD.md)
for graphical representation of item characteristic curves.  
[`coef.difORD`](https://adelahladka.github.io/difNLR/reference/coef.difORD.md)
for extraction of item parameters with their standard errors.  
[`predict.difORD`](https://adelahladka.github.io/difNLR/reference/predict.difORD.md)
for calculation of predicted values.  
[`logLik.difORD`](https://adelahladka.github.io/difNLR/reference/logLik.difORD.md),
[`AIC.difORD`](https://adelahladka.github.io/difNLR/reference/logLik.difORD.md),
[`BIC.difORD`](https://adelahladka.github.io/difNLR/reference/logLik.difORD.md)
for extraction of log-likelihood and information criteria.  

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for multiple
comparison corrections.  
[`vglm`](https://rdrr.io/pkg/VGAM/man/vglm.html) for estimation function
using iteratively reweighted least squares.

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
# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)] # items
group <- Anxiety[, "gender"] # group membership variable

# testing both DIF effects with adjacent category logit model
(x <- difORD(Data, group, focal.name = 1, model = "adjacent"))
#> Detection of both types of Differential Item Functioning
#> for ordinal data using adjacent category logit regression
#> model
#> 
#> Likelihood-ratio Chi-square statistics
#> 
#> Item purification was not applied
#> No p-value adjustment for multiple comparisons
#> 
#>     Chisq-value P-value   
#> R1   1.2551      0.5339   
#> R2   5.9526      0.0510 . 
#> R3   0.0852      0.9583   
#> R4   0.1258      0.9390   
#> R5   1.0432      0.5936   
#> R6   9.8619      0.0072 **
#> R7   9.9535      0.0069 **
#> R8   1.0119      0.6029   
#> R9   2.8220      0.2439   
#> R10  5.2412      0.0728 . 
#> R11  2.5074      0.2855   
#> R12  4.0344      0.1330   
#> R13  1.6216      0.4445   
#> R14  0.5069      0.7761   
#> R15  1.6559      0.4370   
#> R16  3.9444      0.1391   
#> R17  1.7717      0.4124   
#> R18  0.1236      0.9401   
#> R19  9.1928      0.0101 * 
#> R20 11.1244      0.0038 **
#> R21  3.0459      0.2181   
#> R22  3.7980      0.1497   
#> R23  2.7844      0.2485   
#> R24  0.5137      0.7735   
#> R25  1.0364      0.5956   
#> R26  0.9524      0.6211   
#> R27  0.2938      0.8634   
#> R28  4.3879      0.1115   
#> R29  3.4921      0.1745   
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Items detected as DIF items:
#>  R6
#>  R7
#>  R19
#>  R20
if (FALSE) { # \dontrun{
# graphical devices
plot(x, item = 6)
plot(x, item = "R6")
plot(x, item = "R6", group.names = c("Males", "Females"))

# estimated parameters
coef(x)
coef(x, SE = TRUE) # with SE
coef(x, SE = TRUE, simplify = TRUE) # with SE, simplified

# AIC, BIC, log-likelihood
AIC(x)
BIC(x)
logLik(x)

# AIC, BIC, log-likelihood for the first item
AIC(x, item = 1)
BIC(x, item = 1)
logLik(x, item = 1)

# testing both DIF effects with Benjamini-Hochberg adjustment method
difORD(Data, group, focal.name = 1, model = "adjacent", p.adjust.method = "BH")

# testing both DIF effects with item purification
difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE)

# testing uniform DIF effects
difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif")
# testing non-uniform DIF effects
difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")

# testing both DIF effects with different matching criteria
ddfMLR(Data, group, focal.name = 1, model = "adjacent", match = "score")
difORD(Data, group, focal.name = 1, model = "adjacent", match = "restscore")
difORD(Data, group, focal.name = 1, key, match = "zrestscore")
match <- rowSums(GMAT[, 1:20])
difORD(Data, group, focal.name = 1, key, match = match)
match <- replicate(ncol(Data), GMAT$criterion)
difORD(Data, group, focal.name = 1, key, match = match)
match <- as.data.frame(match)
difORD(Data, group, focal.name = 1, key, match = match)

difORD(Data, group, focal.name = 1, model = "adjacent", match = "score")

# testing both DIF effects with cumulative logit model
(x <- difORD(Data, group, focal.name = 1, model = "cumulative"))
# graphical devices
plot(x, item = 7, plot.type = "cumulative")
plot(x, item = 7, plot.type = "category")

# estimated parameters
coef(x, simplify = TRUE)
} # }
```
