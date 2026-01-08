# DDF likelihood ratio statistics based on multinomial log-linear regression model.

Calculates DDF likelihood ratio statistics for nominal data based on
multinomial log-linear model.

## Usage

``` r
MLR(Data, group, key, type = "both", match = "zscore", anchor = 1:ncol(Data),
    p.adjust.method = "none", alpha = 0.05, parametrization)
```

## Arguments

- Data:

  data.frame or matrix: dataset which rows represent unscored examinee
  answers (nominal) and columns correspond to the items.

- group:

  numeric: binary vector of group membership. `"0"` for reference group,
  `"1"` for focal group.

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
  [`coef.ddfMLR`](https://adelahladka.github.io/difNLR/reference/coef.ddfMLR.md)
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

  log-likelihood of m0 model.

- `ll.m1`:

  log-likelihood of m1 model.

- `AIC.m0`:

  AIC of m0 model.

- `AIC.m1`:

  AIC of m1 model.

- `BIC.m0`:

  BIC of m0 model.

- `BIC.m1`:

  BIC of m1 model.

## Details

\$\$P(y = k) = exp(b_0k + b_1k \* x + b_2k \* g + b_3k \* x \* g) / (1 +
\sum exp(b_0l + b_1l \* x + b_2l \* g + b_3l \* x \* g)), \$\$ where
\\x\\ is by default standardized total score (also called Z-score) and
\\g\\ is a group membership. Probability of correct answer (specified in
argument `key`) is \$\$P(y = k) = 1/(1 + \sum exp(b_0l + b_1l \* x +
b_2l \* g + b_3l \* x \* g)). \$\$ Parameters are estimated via neural
networks. For more details see
[`multinom`](https://rdrr.io/pkg/nnet/man/multinom.html).

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
[`multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)

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
MLR(Data, group, key, type = "both")

# testing uniform DDF effects
MLR(Data, group, key, type = "udif")

# testing non-uniform DDF effects
MLR(Data, group, key, type = "nudif")
} # }
```
