# DIF likelihood ratio statistics for ordinal data.

Calculates DIF likelihood ratio statistics for ordinal data based either
on adjacent category logit regression model or on cumulative logit
regression model.

## Usage

``` r
ORD(Data, group, model = "adjacent", type = "both", match = "zscore",
    anchor = 1:ncol(Data), p.adjust.method = "none",
    alpha = 0.05, parametrization)
```

## Arguments

- Data:

  data.frame or matrix: dataset which rows represent ordinally scored
  examinee answers and columns correspond to the items.

- group:

  numeric: binary vector of group membership. `"0"` for reference group,
  `"1"` for focal group.

- model:

  character: logistic regression model for ordinal data (either
  `"adjacent"` (default) or `"cumulative"`). See **Details**.

- type:

  character: type of DIF to be tested. Either `"both"` for uniform and
  non-uniform DIF (i.e., difference in parameters `"a"` and `"b"`)
  (default), or `"udif"` for uniform DIF only (i.e., difference in
  difficulty parameter `"b"`), or `"nudif"` for non-uniform DIF only
  (i.e., difference in discrimination parameter `"a"`). Can be specified
  as a single value (for all items) or as an item-specific vector.

- match:

  numeric or character: matching criterion to be used as an estimate of
  trait. Can be either `"zscore"` (default, standardized total score),
  `"score"` (total test score), or vector of the same length as number
  of observations in `Data`.

- anchor:

  character or numeric: specification of DIF free items. A vector of
  item identifiers (integers specifying the column number) specifying
  which items are currently considered as anchor (DIF free) items.
  Argument is ignored if `match` is not `"zscore"` or `"score"`.

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

A list with the following arguments:

- `Sval`:

  the values of likelihood ratio test statistics.

- `pval`:

  the p-values by likelihood ratio test.

- `adj.pval`:

  the adjusted p-values by likelihood ratio test using
  `p.adjust.method`.

- `df`:

  the degress of freedom of likelihood ratio test.

- `par.m0`:

  the estimates of null model.

- `par.m1`:

  the estimates of alternative model.

- `se.m0`:

  standard errors of parameters in null model.

- `se.m1`:

  standard errors of parameters in alternative model.

- `cov.m0`:

  list of covariance matrices of item parameters for null model.

- `cov.m1`:

  list of covariance matrices of item parameters for alternative model.

- `ll.m0`:

  log-likelihood of null model.

- `ll.m1`:

  log-likelihood of alternative model.

- `AIC.m0`:

  AIC of null model.

- `AIC.m1`:

  AIC of alternative model.

- `BIC.m0`:

  BIC of null model.

- `BIC.m1`:

  BIC of alternative model.

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

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)
[`vglm`](https://rdrr.io/pkg/VGAM/man/vglm.html)

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

# testing both DIF effects
ORD(Data, group, type = "both")

# testing uniform DIF effects
ORD(Data, group, type = "udif")

# testing non-uniform DIF effects
ORD(Data, group, type = "nudif")

# testing DIF using cumulative logit model
ORD(Data, group, model = "cumulative")
} # }
```
