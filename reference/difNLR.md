# DIF detection using non-linear regression method.

Performs DIF detection procedure in dichotomous data based on non-linear
regression model (generalized logistic regression) and either
likelihood-ratio test, F-test, or Wald's test of a submodel.

## Usage

``` r
difNLR(Data, group, focal.name, model, constraints, type = "all",
       method = "nls", match = "zscore", anchor = NULL, purify = FALSE,
       nrIter = 10, test = "LR", alpha = 0.05, p.adjust.method = "none", start,
       initboot = TRUE, nrBo = 20, sandwich = FALSE)
```

## Arguments

- Data:

  data.frame or matrix: dataset in which rows represent scored examinee
  answers (`"1"` correct, `"0"` incorrect) and columns correspond to the
  items. In addition, `Data` can hold the vector of group membership.

- group:

  numeric or character: a binary vector of the same length as
  `nrow(Data)` or a column identifier in the `Data`.

- focal.name:

  numeric or character: indicates the level of the `group` corresponding
  to the focal group.

- model:

  character: generalized logistic regression model to be fitted. See
  **Details**.

- constraints:

  character: which parameters should be the same for both groups.
  Possible values are any combinations of parameters `"a"`, `"b"`,
  `"c"`, and `"d"`. See **Details**.

- type:

  character: type of DIF to be tested. Possible values are `"all"` for
  detecting differences in any parameters (default), `"udif"` for
  uniform DIF only (i.e., difference in difficulty parameter `"b"`),
  `"nudif"` for non-uniform DIF only (i.e., difference in discrimination
  parameter `"a"`), `"both"` for uniform and non-uniform DIF (i.e.,
  difference in parameters `"a"` and `"b"`), or a combination of
  parameters `"a"`, `"b"`, `"c"`, and `"d"`. Can be specified as a
  single value (for all items) or as an item-specific vector.

- method:

  character: an estimation method to be applied. The options are `"nls"`
  for non-linear least squares (default), `"mle"` for the maximum
  likelihood method using the `"L-BFGS-B"` algorithm with constraints,
  `"em"` for the maximum likelihood estimation with the EM algorithm,
  `"plf"` for the maximum likelihood estimation with the algorithm based
  on parametric link function, and `"irls"` for the maximum likelihood
  estimation with the iteratively reweighted least squares algorithm
  (available for the `"2PL"` model only). See **Details**.

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

  logical: should the item purification be applied? (the default is
  `FALSE`). Item purification is not applied when set of anchor items in
  `anchor` is specified or when `match` is not `"zscore"`, `"score"`,
  `"restscore"`, or `"zrestscore"`.

- nrIter:

  numeric: the maximal number of iterations in the item purification
  (the default is 10).

- test:

  character: a statistical test to be performed for DIF detection. Can
  be either `"LR"` for the likelihood ratio test of a submodel
  (default), `"W"` for the Wald's test, or `"F"` for the F-test of a
  submodel.

- alpha:

  numeric: a significance level (the default is 0.05).

- p.adjust.method:

  character: a method for a multiple comparison correction. Possible
  values are `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`,
  `"BY"`, `"fdr"`, and `"none"` (default). For more details see
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- start:

  numeric: initial values for the estimation of item parameters. If not
  specified, starting values are calculated with the
  [`startNLR`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function. Otherwise, a list with as many elements as a number of
  items. Each element is a named numeric vector representing initial
  values for estimation of item parameters. Specifically, parameters
  `"a"`, `"b"`, `"c"`, and `"d"` are initial values for discrimination,
  difficulty, guessing, and inattention for the reference group.
  Parameters `"aDif"`, `"bDif"`, `"cDif"`, and `"dDif"` are then
  differences in these parameters between the reference and focal
  groups. For the `method = "irls"`, default initial values from the
  [`glm`](https://rdrr.io/r/stats/glm.html) function are used.

- initboot:

  logical: in the case of convergence issues, should starting values be
  re-calculated based on bootstrapped samples? (the default is `TRUE`;
  newly calculated initial values are applied only to items/models with
  convergence issues).

- nrBo:

  numeric: the maximal number of iterations for the calculation of
  starting values using bootstrapped samples (the default is 20).

- sandwich:

  logical: should the sandwich estimator be applied for computation of
  the covariance matrix of item parameters when using `method = "nls"`?
  (the default is `FALSE`).

## Value

The `difNLR()` function returns an object of class `"difNLR"`. The
output, including values of the test statistics, p-values, and items
detected as function differently, is displayed by the
[`print()`](https://rdrr.io/r/base/print.html) method.

Object of class `"difNLR"` is a list with the following components:

- `Sval`:

  the values of the `test` statistics.

- `nlrPAR`:

  the item parameter estimates of the final model.

- `nlrSE`:

  the standard errors of the item parameter estimates of the final
  model.

- `parM0`:

  the item parameter estimates of the null (smaller) model.

- `seM0`:

  the standard errors of item parameter estimates of the null (smaller)
  model.

- `covM0`:

  the covariance matrices of the item parameter estimates of the null
  (smaller) model.

- `llM0`:

  the log-likelihood values of the null (smaller) model.

- `parM1`:

  the item parameter estimates of the alternative (larger) model.

- `seM1`:

  the standard errors of the item parameter estimates of the alternative
  (larger) model.

- `covM1`:

  the covariance matrices of the item parameter estimates of alternative
  (larger) model.

- `llM1`:

  the log-likelihood values of the alternative (larger) model.

- `DIFitems`:

  either the column identifiers of the items which were detected as DIF,
  or `"No DIF item detected"` in the case no item was detected as
  function differently.

- `model`:

  fitted model.

- `constraints`:

  constraints for the `model`.

- `type`:

  character: type of DIF that was tested. If a combination of the item
  parameters was specified, the value is `"other"`.

- `types`:

  character: the parameters (specified by user, `type` has value
  `"other"`) which were tested for difference.

- `p.adjust.method`:

  character: a method for the multiple comparison correction which was
  applied.

- `pval`:

  the p-values by the `test`.

- `adjusted.pval`:

  adjusted p-values by the `p.adjust.method`.

- `df`:

  the degrees of freedom of the `test`.

- `test`:

  used test.

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

  logical: indicating whether item purification process converged before
  the maximal number `nrIter` of iterations. Returned only if `purify`
  is `TRUE`.

- `method`:

  used estimation method.

- `conv.fail`:

  numeric: number of convergence issues.

- `conv.fail.which`:

  the identifiers of the items which did not converge.

- `alpha`:

  numeric: significance level.

- `Data`:

  the data matrix.

- `group`:

  the vector of group membership.

- `group.names`:

  names of groups.

- `match`:

  matching criterion.

- `match.name`:

  Name of the matching criterion.

Several methods are available for an object of the `"difNLR"` class
(e.g., `methods(class = "difNLR")`).

## Details

DIF detection procedure based on non-linear regression is the extension
of the logistic regression procedure (Swaminathan & Rogers, 1990)
accounting for possible guessing and/or inattention when responding
(Drabinova & Martinkova, 2017; Hladka & Martinkova, 2020).

The unconstrained form of the 4PL generalized logistic regression model
for probability of correct answer (i.e., \\Y\_{pi} = 1\\) using IRT
parameterization is \$\$P(Y\_{pi} = 1\|X_p, G_p) = (c\_{i} +
c\_{i\text{DIF}} \cdot G_p) + (d\_{i} + d\_{i\text{DIF}} \cdot G_p -
c\_{i} - c\_{i\text{DIF}} \cdot G_p) / (1 + \exp(-(a_i +
a\_{i\text{DIF}} \cdot G_p) \cdot (X_p - b_p - b\_{i\text{DIF}} \cdot
G_p))), \$\$ where \\X_p\\ is the matching criterion (e.g., standardized
total score) and \\G_p\\ is a group membership variable for respondent
\\p\\. Parameters \\a_i\\, \\b_i\\, \\c_i\\, and \\d_i\\ are
discrimination, difficulty, guessing, and inattention for the reference
group for item \\i\\. Terms \\a\_{i\text{DIF}}\\, \\b\_{i\text{DIF}}\\,
\\c\_{i\text{DIF}}\\, and \\d\_{i\text{DIF}}\\ then represent
differences between the focal and reference groups in discrimination,
difficulty, guessing, and inattention for item \\i\\, respectively.

Alternatively, intercept-slope parameterization may be applied:
\$\$P(Y\_{pi} = 1\|X_p, G_p) = (c\_{i} + c\_{i\text{DIF}} \cdot G_p) +
(d\_{i} + d\_{i\text{DIF}} \cdot G_p - c\_{i} - c\_{i\text{DIF}} \cdot
G_p) / (1 + \exp(-(\beta\_{i0} + \beta\_{i1} \cdot X_p + \beta\_{i2}
\cdot G_p + \beta\_{i3} \cdot X_p \cdot G_p))), \$\$ where parameters
\\\beta\_{i0}, \beta\_{i1}, \beta\_{i2}, \beta\_{i3}\\ are intercept,
effect of the matching criterion, effect of the group membership, and
their mutual interaction, respectively.

The `model` and `constraints` arguments can further constrain the 4PL
model. The arguments `model` and `constraints` can also be combined.
Both arguments can be specified as a single value (for all items) or as
an item-specific vector (where each element corresponds to one item).

The `model` argument offers several predefined models. The options are
as follows: `Rasch` for 1PL model with discrimination parameter fixed on
value 1 for both groups, `1PL` for 1PL model with discrimination
parameter set the same for both groups, `2PL` for logistic regression
model, `3PLcg` for 3PL model with fixed guessing for both groups,
`3PLdg` for 3PL model with fixed inattention for both groups, `3PLc`
(alternatively also `3PL`) for 3PL regression model with guessing
parameter, `3PLd` for 3PL model with inattention parameter, `4PLcgdg`
for 4PL model with fixed guessing and inattention parameter for both
groups, `4PLcgd` (alternatively also `4PLd`) for 4PL model with fixed
guessing for both groups, `4PLcdg` (alternatively also `4PLc`) for 4PL
model with fixed inattention for both groups, or `4PL` for 4PL model.

The underlying generalized logistic regression model can be further
specified in more detail with the `constraints` argument which specifies
what parameters should be fixed for both groups. For example, a choice
`"ad"` means that discrimination (parameter `"a"`) and inattention
(parameter `"d"`) are fixed (and estimated for) both groups and other
parameters (`"b"` and `"c"`) are not. The `NA` value for `constraints`
means no constraints.

Missing values are allowed but discarded for an item estimation. They
must be coded as `NA` for both, the `Data` and `group` arguments.

The function uses intercept-slope parameterization for the estimation
via the
[`estimNLR`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
function. Item parameters are then re-calculated into the IRT
parameterization using the delta method.

The function offers either the non-linear least squares estimation via
the [`nls`](https://rdrr.io/r/stats/nls.html) function (Drabinova &
Martinkova, 2017; Hladka & Martinkova, 2020), the maximum likelihood
method with the `"L-BFGS-B"` algorithm with constraints via the
[`optim`](https://rdrr.io/r/stats/optim.html) function (Hladka &
Martinkova, 2020), the maximum likelihood method with the EM algorithm
(Hladka, Martinkova, & Brabec, 2025), the maximum likelihood method with
the algorithm based on parametric link function (Hladka, Martinkova, &
Brabec, 2025), or the maximum likelihood method with the iteratively
reweighted least squares algorithm via the
[`glm`](https://rdrr.io/r/stats/glm.html) function.

## References

Drabinova, A. & Martinkova, P. (2017). Detection of differential item
functioning with nonlinear regression: A non-IRT approach accounting for
guessing. Journal of Educational Measurement, 54(4), 498–517,
[doi:10.1111/jedm.12158](https://doi.org/10.1111/jedm.12158) .

Hladka, A. (2021). Statistical models for detection of differential item
functioning. Dissertation thesis. Faculty of Mathematics and Physics,
Charles University.

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

Hladka, A., Martinkova, P., & Brabec, M. (2025). New iterative
algorithms for estimation of item functioning. Journal of Educational
and Behavioral Statistics. Online first,
[doi:10.3102/10769986241312354](https://doi.org/10.3102/10769986241312354)
.

Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item
functioning using logistic regression procedures. Journal of Educational
Measurement, 27(4), 361–370,
[doi:10.1111/j.1745-3984.1990.tb00754.x](https://doi.org/10.1111/j.1745-3984.1990.tb00754.x)

## See also

[`plot.difNLR`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
for a graphical representation of item characteristic curves and DIF
statistics.  
[`coef.difNLR`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
for an extraction of item parameters with their standard errors in
various parameterizations.  
[`predict.difNLR`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md)
for prediction.  
[`fitted.difNLR`](https://adelahladka.github.io/difNLR/reference/residuals.difNLR.md)
and
[`residuals.difNLR`](https://adelahladka.github.io/difNLR/reference/residuals.difNLR.md)
for an extraction of fitted values and residuals.  
[`logLik.difNLR`](https://adelahladka.github.io/difNLR/reference/logLik.difNLR.md),
[`AIC.difNLR`](https://adelahladka.github.io/difNLR/reference/logLik.difNLR.md),
[`BIC.difNLR`](https://adelahladka.github.io/difNLR/reference/logLik.difNLR.md)
for an extraction of log-likelihood values and information criteria.  

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html) for multiple
comparison corrections.  
[`nls`](https://rdrr.io/r/stats/nls.html) for a nonlinear least squares
estimation.  
[`startNLR`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
for a calculation of initial values of fitting algorithms in `difNLR()`.

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
# loading data
data(GMAT)
Data <- GMAT[, 1:20] # items
group <- GMAT[, "group"] # group membership variable

# testing both DIF effects using likelihood-ratio test and
# 3PL model with fixed guessing for groups
(x <- difNLR(Data, group, focal.name = 1, model = "3PLcg"))
#> Detection of all types of differential item functioning
#> using the generalized logistic regression model
#> 
#> Generalized logistic regression likelihood ratio chi-square statistics
#> based on 3PL model with fixed guessing for groups 
#> 
#> Parameters were estimated using non-linear least squares
#> 
#> Item purification was not applied
#> No p-value adjustment for multiple comparisons
#> 
#>        Chisq-value P-value    
#> Item1  82.0689      0.0000 ***
#> Item2  28.3232      0.0000 ***
#> Item3   0.6845      0.7102    
#> Item4   3.3055      0.1915    
#> Item5   1.1984      0.5492    
#> Item6   0.1573      0.9244    
#> Item7   8.3032      0.0157 *  
#> Item8   2.8660      0.2386    
#> Item9   0.4549      0.7966    
#> Item10  1.3507      0.5090    
#> Item11  1.2431      0.5371    
#> Item12  1.0537      0.5905    
#> Item13  4.4139      0.1100    
#> Item14  1.4940      0.4738    
#> Item15  1.3079      0.5200    
#> Item16  0.1424      0.9313    
#> Item17  3.1673      0.2052    
#> Item18  2.0206      0.3641    
#> Item19  6.2546      0.0438 *  
#> Item20  3.4871      0.1749    
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Detection thresholds: 5.9915 (significance level: 0.05)
#> 
#> Items detected as DIF items:
#>  Item1
#>  Item2
#>  Item7
#>  Item19
if (FALSE) { # \dontrun{
# graphical devices
plot(x, item = x$DIFitems)
plot(x, item = "Item1")
plot(x, item = 1, group.names = c("Group 1", "Group 2"))
plot(x, plot.type = "stat")

# coefficients
coef(x)
coef(x, SE = TRUE)
coef(x, SE = TRUE, simplify = TRUE)
coef(x, item = 1, CI = 0)

# fitted values
fitted(x)
fitted(x, item = 1)

# residuals
residuals(x)
residuals(x, item = 1)

# predicted values
predict(x)
predict(x, item = 1)

# predicted values for new subjects
predict(x, item = 1, match = 0, group = c(0, 1))

# AIC, BIC, log-likelihood
AIC(x)
BIC(x)
logLik(x)

# AIC, BIC, log-likelihood for the first item
AIC(x, item = 1)
BIC(x, item = 1)
logLik(x, item = 1)

# testing both DIF effects using Wald test and
# 3PL model with fixed guessing for groups
difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "W")

# testing both DIF effects using F test and
# 3PL model with fixed guessing for groups
difNLR(Data, group, focal.name = 1, model = "3PLcg", test = "F")

# testing both DIF effects using
# 3PL model with fixed guessing for groups and sandwich estimator
# of the covariance matrices
difNLR(Data, group, focal.name = 1, model = "3PLcg", sandwich = TRUE)

# testing both DIF effects using LR test,
# 3PL model with fixed guessing for groups
# and Benjamini-Hochberg correction
difNLR(Data, group, focal.name = 1, model = "3PLcg", p.adjust.method = "BH")

# testing both DIF effects using LR test,
# 3PL model with fixed guessing for groups
# and item purification
difNLR(Data, group, focal.name = 1, model = "3PLcg", purify = TRUE)

# testing both DIF effects using 3PL model with fixed guessing for groups
# and different matching criteria
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "score")
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "restscore")
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = "zrestscore")
match <- rowSums(Data)
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)
match <- replicate(nrow(Data), match)
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)
match <- as.data.frame(match)
difNLR(Data, group, focal.name = 1, model = "3PLcg", match = match)

# testing uniform DIF effects using 4PL model with the same
# guessing and inattention
difNLR(Data, group, focal.name = 1, model = "4PLcgdg", type = "udif")

# testing non-uniform DIF effects using 2PL model
difNLR(Data, group, focal.name = 1, model = "2PL", type = "nudif")

# testing difference in parameter b using 4PL model with fixed
# a and c parameters
difNLR(Data, group, focal.name = 1, model = "4PL", constraints = "ac", type = "b")

# testing both DIF effects using LR test,
# 3PL model with fixed guessing for groups
# using maximum likelihood estimation with
# the L-BFGS-B algorithm, the EM algorithm, and the PLF algorithm
difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "mle")
difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "em")
difNLR(Data, group, focal.name = 1, model = "3PLcg", method = "plf")

# testing both DIF effects using LR test and 2PL model
# using maximum likelihood estimation with iteratively reweighted least squares algorithm
difNLR(Data, group, focal.name = 1, model = "2PL", method = "irls")
} # }
```
