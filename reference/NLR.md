# DIF statistics for non-linear regression models.

Calculates likelihood ratio test statistics, F-test statistics, or
Wald's test statistics for DIF detection among dichotomous items using
non-linear regression models (generalized logistic regression models).

## Usage

``` r
NLR(Data, group, model, constraints = NULL, type = "all", method = "nls",
    match = "zscore", anchor = 1:ncol(Data), start, p.adjust.method = "none",
    test = "LR", alpha = 0.05, initboot = TRUE, nrBo = 20, sandwich = FALSE)
```

## Arguments

- Data:

  data.frame or matrix: dataset in which rows represent scored examinee
  answers (`"1"` correct, `"0"` incorrect) and columns correspond to the
  items.

- group:

  numeric: a binary vector of a group membership (`"0"` for the
  reference group, `"1"` for the focal group).

- model:

  character: generalized logistic regression model to be fitted. See
  **Details**.

- constraints:

  character: which parameters should be the same for both groups.
  Possible values are any combinations of parameters `"a"`, `"b"`,
  `"c"`, and `"d"`. Default value is `NULL`. See **Details**.

- type:

  character: type of DIF to be tested. Possible values are `"all"` for
  detecting difference in any parameter (default), `"udif"` for uniform
  DIF only (i.e., difference in difficulty parameter `"b"`), `"nudif"`
  for non-uniform DIF only (i.e., difference in discrimination parameter
  `"a"`), `"both"` for uniform and non-uniform DIF (i.e., difference in
  parameters `"a"` and `"b"`), or any combination of parameters `"a"`,
  `"b"`, `"c"`, and `"d"`. Can be specified as a single value (for all
  items) or as an item-specific vector.

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
  the trait. It can be either `"zscore"` (default, standardized total
  score), `"score"` (total test score), or a numeric vector of the same
  length as a number of observations in the `Data`.

- anchor:

  character or numeric: specification of DIF free items. A vector of
  item identifiers (integers specifying the column number) specifying
  which items are currently considered as anchor (DIF free) items.
  Argument is ignored if the `match` is not `"zscore"` or `"score"`.

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

- p.adjust.method:

  character: a method for a multiple comparison correction. Possible
  values are `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`,
  `"BY"`, `"fdr"`, and `"none"` (default). For more details see
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- test:

  character: a statistical test to be performed for DIF detection. Can
  be either `"LR"` for the likelihood ratio test of a submodel
  (default), `"W"` for the Wald's test, or `"F"` for the F-test of a
  submodel.

- alpha:

  numeric: a significance level (the default is 0.05).

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

A list with the following arguments:

- `Sval`:

  the values of the `test` statistics.

- `pval`:

  the p-values by the `test`.

- `adjusted.pval`:

  adjusted p-values by the `p.adjust.method`.

- `df`:

  the degrees of freedom of the `test`.

- `test`:

  used test.

- `par.m0`:

  the matrix of estimated item parameters for the null model.

- `se.m0`:

  the matrix of standard errors of item parameters for the null model.

- `cov.m0`:

  list of covariance matrices of item parameters for the null model.

- `par.m1`:

  the matrix of estimated item parameters for the alternative model.

- `se.m1`:

  the matrix of standard errors of item parameters for the alternative
  model.

- `cov.m1`:

  list of covariance matrices of item parameters for the alternative
  model.

- `cf`:

  numeric: a number of convergence issues.

- `cf.which`:

  the indicators of the items that did not converge.

- `ll.m0`:

  log-likelihood of null model.

- `ll.m1`:

  log-likelihood of alternative model.

- `startBo0`:

  the binary matrix. Columns represent iterations of initial values
  re-calculations, rows represent items. The value of 0 means no
  convergence issue in the null model, 1 means convergence issue in the
  null model.

- `startBo1`:

  the binary matrix. Columns represent iterations of initial values
  re-calculations, rows represent items. The value of 0 means no
  convergence issue in the alternative model, 1 means convergence issue
  in the alternative model.

## Details

The function calculates test statistics using a DIF detection procedure
based on non-linear regression models (i.e., extensions of the logistic
regression procedure; Swaminathan & Rogers, 1990; Drabinova &
Martinkova, 2017).

The unconstrained form of the 4PL generalized logistic regression model
for probability of correct answer (i.e., \\Y\_{pi} = 1\\) using IRT
parameterization is \$\$P(Y\_{pi} = 1\|X_p, G_p) = (c\_{iR} \cdot G_p +
c\_{iF} \cdot (1 - G_p)) + (d\_{iR} \cdot G_p + d\_{iF} \cdot (1 -
G_p) - c\_{iR} \cdot G_p - c\_{iF} \cdot (1 - G_p)) / (1 + \exp(-(a_i +
a\_{i\text{DIF}} \cdot G_p) \cdot (X_p - b_p - b\_{i\text{DIF}} \cdot
G_p))), \$\$ where \\X_p\\ is the matching criterion (e.g., standardized
total score) and \\G_p\\ is a group membership variable for respondent
\\p\\. Parameters \\a_i\\, \\b_i\\, \\c\_{iR}\\, and \\d\_{iR}\\ are
discrimination, difficulty, guessing, and inattention for the reference
group for item \\i\\. Terms \\a\_{i\text{DIF}}\\ and
\\b\_{i\text{DIF}}\\ then represent differences between the focal and
reference groups in discrimination and difficulty for item \\i\\. Terms
\\c\_{iF}\\, and \\d\_{iF}\\ are guessing and inattention parameters for
the focal group for item \\i\\. In the case that there is no assumed
difference between the reference and focal group in the guessing or
inattention parameters, the terms \\c_i\\ and \\d_i\\ are used.

Alternatively, intercept-slope parameterization may be applied:
\$\$P(Y\_{pi} = 1\|X_p, G_p) = (c\_{iR} \cdot G_p + c\_{iF} \cdot (1 -
G_p)) + (d\_{iR} \cdot G_p + d\_{iF} \cdot (1 - G_p) - c\_{iR} \cdot
G_p - c\_{iF} \cdot (1 - G_p)) / (1 + \exp(-(\beta\_{i0} + \beta\_{i1}
\cdot X_p + \beta\_{i2} \cdot G_p + \beta\_{i3} \cdot X_p \cdot G_p))),
\$\$ where parameters \\\beta\_{i0}, \beta\_{i1}, \beta\_{i2},
\beta\_{i3}\\ are intercept, effect of the matching criterion, effect of
the group membership, and their mutual interaction, respectively.

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

Hladka, A., Martinkova, P., & Brabec, M. (2026). New iterative
algorithms for estimation of item functioning. Journal of Educational
and Behavioral Statistics, 51(1), 175–205,
[doi:10.3102/10769986241312354](https://doi.org/10.3102/10769986241312354)
.

Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item
functioning using logistic regression procedures. Journal of Educational
Measurement, 27(4), 361–370,
[doi:10.1111/j.1745-3984.1990.tb00754.x](https://doi.org/10.1111/j.1745-3984.1990.tb00754.x)

## See also

[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

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

# testing both DIF effects using the LR test (default)
# and the model with fixed guessing for both groups
NLR(Data, group, model = "3PLcg")

# using the F test and Wald's test
NLR(Data, group, model = "3PLcg", test = "F")
NLR(Data, group, model = "3PLcg", test = "W")

# using the Benjamini-Hochberg correction
NLR(Data, group, model = "3PLcg", p.adjust.method = "BH")

# 4PL model with the same guessing and inattention
# to test uniform DIF
NLR(Data, group, model = "4PLcgdg", type = "udif")

# 2PL model to test non-uniform DIF
NLR(Data, group, model = "2PL", type = "nudif")

# 4PL model with fixed a and c parameters
# to test difference in parameter b
NLR(Data, group, model = "4PL", constraints = "ac", type = "b")

# using various estimation algorithms
NLR(Data, group, model = "3PLcg", method = "nls")
NLR(Data, group, model = "3PLcg", method = "mle")
NLR(Data, group, model = "3PLcg", method = "em")
NLR(Data, group, model = "3PLcg", method = "plf")
NLR(Data, group, model = "2PL", method = "irls")
} # }
```
