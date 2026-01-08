# Changelog

## difNLR 1.5.3 (2026-01-08)

### Major updates

- A webpage <https://adelahladka.github.io/difNLR/> was created.
- The first vignette for a quick start with the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was created.

------------------------------------------------------------------------

## difNLR 1.5.2-1-2 (2025-11-19)

***THIS IS A CRAN VERSION***

### Minor updates

- Tests skipped on CRAN.

------------------------------------------------------------------------

## difNLR 1.5.2 (2025-11-18)

### Major updates

- `match` argument of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  now handles a numeric matrix where each column represents the matching
  criterion for each item of `Data`.
- `match` argument of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  now can take also values of `"restscore"` and `"zrestscore"`
  representing total score without item being currently tested and its
  standardized version, respectively.
- Argument `constraints` is now used in the
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function when computing starting values for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.

### Minor updates

- Tests extended and updated.
- Checking inputs of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions was updated.
- Documentation of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions was updated; specifically for `match`, `anchor`, and
  `purify` arguments. New output `match.name` describing name of the
  matching criterion for plotting was added.

------------------------------------------------------------------------

## difNLR 1.5.1-4 (2025-06-30)

CRAN release: 2025-06-30

***THIS IS A CRAN VERSION***

*It includes versions 1.5.1-2 - 1.5.1-4*

### Bug fixes

- Minor bug in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) S3 method for
  the
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  was fixed. Categories are now correctly ordered for lines and points.

### Minor updates

- Tests extended and updated.
- Checking inputs of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md)
  functions was updated.
- [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  now handles parameters for dichotomous items as matrices with a single
  column.
- Legend of plots moved back inside of the plots.

------------------------------------------------------------------------

## difNLR 1.5.1-3 (2025-06-08)

### Bug fixes

- Typos in equations of generalized logistic model in the `difNLR`
  function were fixed.
- Parametrization for the parameters “c” and “d” in the `predict.difNLR`
  was fixed, fixing also issues in the `plot.difNLR`.
- Bug in degrees of freedom in the `df` output of the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was fixed. Degrees of freedom are now correctly returned for
  the LR and Wald tests.
- When `purify = TRUE` or `anchor` argument is used, matching criterion
  is now correctly computed in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html) S3 methods for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.
- Bug in the [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) S3
  method for the `difNLR` was fixed.

### Minor updates

- References were updated.
- Tests extended and updated.
- Check of the `start` input in the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was updated.
- Check of inputs was updated in the
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md),
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions.
- Error messages were updated for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md),
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md),
  and
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  functions.
- Output `anchor` was added into the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md),
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions, specifying anchoring items from the `anchor` argument. This
  also excludes DIF items when item purification is applied with the
  `purify = TRUE` argument.

------------------------------------------------------------------------

## difNLR 1.5.1-2 (2025-03-05)

### Bug fixes

- Bug in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) when
  `method = "irls"` fixed. Parameters are now labelled as b0-b3 and
  estimates are correctly printed with the
  [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
  S3 method.
- Bug in
  [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
  S3 method fixed when only one parameter is estimated. Thanks to Jan
  Netik.
- Bug in
  [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
  S3 method fixed when multiple items did not converged.
- Bug in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  handling items that failed to converge and their estimates.

### Minor updates

- Tests updated.

------------------------------------------------------------------------

## difNLR 1.5.1-1 (2025-03-03)

CRAN release: 2025-03-03

***THIS IS A CRAN VERSION***

*It includes versions 1.5.0-1 - 1.5.0-2*

------------------------------------------------------------------------

## difNLR 1.5.0-2 (2025-02-27)

### Bug fixes

- Bug in
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  when `type = "b"` fixed.
- Bug in
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  when `method = "plf"` fixed.
- Bug in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  calculating SEs when no item is converged fixed.

### Major updates

- Testing with the `testthat` package has been started.

### Minor updates

- References updated.

------------------------------------------------------------------------

## difNLR 1.5.0-1 (2025-02-17)

### Bug fixes

- Bug in re-calculation of starting values in the
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function was fixed.
- Bugs in
  [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
  when convergence issues are present were fixed. Thanks to Jan Netik.

### Minor updates

- Warnings and messages in the
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) and
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  functions were updated.
- Documentation of the
  [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md)
  was updated.
- Some typos were fixed.
- README file was updated.
- Description of the package was updated.

------------------------------------------------------------------------

## difNLR 1.5.0 (2024-12-14)

CRAN release: 2024-12-19

***THIS IS A CRAN VERSION***

### Bug fixes

- Bug caused by mixing two parametrization in
  [`predict.difNLR()`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md)
  was fixed.
- Bug in calculation of starting values with
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  was fixed.

### Major updates

- Options `"em"` and `"plf"` were added for the `method` argument in the
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function to estimate item parameters with either the EM algorithm or
  algorithm based on parametric link function (PLF). “plf” is now
  default option. This is also the default option for the
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function.
- Options for the `parameterization` argument of the
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  and
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function were updated (renamed).
- Intercept-slope parameterization is now used for model fitting, so
  slots of fitted objects like `parM0` are returned with this
  parameterization.
- Argument `constraints` were added into the
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function.

### Minor updates

- References were updated.

------------------------------------------------------------------------

## difNLR 1.4.3 (2023-05-03)

### Major updates

- `"likelihood"` option for maximum likelihood estimation in the
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function was renamed to `"mle"`.
- S3 methods for the
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function were extended and improved.

------------------------------------------------------------------------

## difNLR 1.4.2-1 (2023-05-03)

CRAN release: 2023-05-03

***THIS IS A CRAN VERSION***

### Minor updates

- CITATION file was updated.

------------------------------------------------------------------------

## difNLR 1.4.2 (2023-05-02)

### Bug fixes

- [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  now correctly plots ordinal data.  
- Option `test = "W"` was fixed for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  functions.
- Constraints handling was fixed for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  functions.

### Minor updates

- `startNLR` now handles missing values. Returns error when not enough
  complete observations are provided.
- Arguments in `ggplot2` plotting methods were updated to follow changes
  in the `ggplot2` package.
- Linetype and its legend appearance in `ggplot2` plotting methods were
  updated.
- `ggplot2` v.3.4.0 is now imported.
- Examples for the
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md)
  and [`ORD()`](https://adelahladka.github.io/difNLR/reference/ORD.md)
  functions were updated. Now using the `Anxiety` dataset from the
  `ShinyItemAnalysis` package.

------------------------------------------------------------------------

## difNLR 1.4.1 (2022-04-18)

CRAN release: 2022-04-18

### Minor updates

- Some typos fixed.
- `class` handling was updated.

------------------------------------------------------------------------

## difNLR 1.4.0 (2022-04-16)

*It includes versions 1.3.7-1 - 1.3.7-3*

------------------------------------------------------------------------

## difNLR 1.3.7-3 (2022-02-11)

### Bug fixes

- Option `parameterization = "logistic"` was fixed in
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  function.

### Major updates

- First version of iteratively reweighted least squares algorithm was
  implemented in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md), and
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  functions.
- [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md),
  [`coef.difORD()`](https://adelahladka.github.io/difNLR/reference/coef.difORD.md),
  and
  [`coef.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/coef.ddfMLR.md)
  methods now include delta method for IRT and logistic
  parameterizations.
- [`coef.difNLR()`](https://adelahladka.github.io/difNLR/reference/coef.difNLR.md),
  [`coef.difORD()`](https://adelahladka.github.io/difNLR/reference/coef.difORD.md),
  and
  [`coef.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/coef.ddfMLR.md)
  methods now include calculation of confidence intervals.

### Minor updates

- Some typos fixed.
- Examples in functions were updated.
- References were updated.
- Output of
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function is now unified via
  [`print()`](https://rdrr.io/r/base/print.html) method.
- Packages CTT, grDevices, methods are no longer imported.

------------------------------------------------------------------------

## difNLR 1.3.7-2 (2021-10-05)

### Major updates

- First version of `predicted.difORD()` to compute predicted values for
  `difORD` object was implemented.

------------------------------------------------------------------------

## difNLR 1.3.7-1 (2021-02-16)

### Bug fixes

- Bug in plotting empirical probabilities in
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  fixed.

------------------------------------------------------------------------

## difNLR 1.3.6/1.3.7 (2021-01-07)

***THIS IS A CRAN VERSION***

### Minor updates

- doi in DESCRIPTION file updated.
- doi in help pages were updated.
- CITATION file updated.
- References were updated.

------------------------------------------------------------------------

## difNLR 1.3.5-2 (2020-11-24)

### Bug fixes

- Empty factor levels were removed from `Data` in
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  to fix bug when plotting.

------------------------------------------------------------------------

## difNLR 1.3.5-1 (2020-09-03)

### Major updates

- Sandwich estimator for covariance matrix in case that `method = "nls"`
  was implemented into the [`vcov()`](https://rdrr.io/r/stats/vcov.html)
  method for the output of the
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function.
- Wald test for implemented for the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.
- Sandwich estimator for covariance matrix in case that `method = "nls"`
  was implemented into the
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function via an argument `sandwich = TRUE`.

------------------------------------------------------------------------

## difNLR 1.3.4/1.3.5 (2020-08-24)

***THIS IS A CRAN VERSION***

### Bug fixes

- Error when covariance matrix cannot be computed for some items in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was fixed.

##### DOCUMENTATION

- URLs for GH were updated.

------------------------------------------------------------------------

## difNLR 1.3.3 (2020-04-29)

CRAN release: 2020-05-04

***THIS IS A CRAN VERSION***

### Bug fixes

- Bug in calculation of standard errors for estimates of
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  was fixed.
- Bug in coefficients of
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  for non-converged items including naming of parameters was fixed
  (Reported by Jan Netik).
- In case that covariance matrix cannot be computed in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md),
  function gives warning and `NA` values for covariance matrix and
  vector of standard errors are returned.

### Major updates

- Confidence intervals were added into
  [`predict.difNLR()`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md)
  method.
- Delta method for alternative parametrization is now applied for whole
  covariance matrix in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md).
- Unnecessary arguments of
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md),
  [`plot.difORD()`](https://adelahladka.github.io/difNLR/reference/plot.difORD.md)
  and
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  were removed. Change of colours/linetypes/shapes/title can be managed
  using standard `ggplot2` syntax.
- [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  now offers possibility to turn off drawing of empirical probabilities
  using argument `draw.empirical = FALSE`.
- [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  now offers possibility to plot confidence intervals for predicted
  values as offered in
  [`predict.difNLR()`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md)
  using argument `draw.CI = TRUE`.
- Starting values calculated via
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  were improved for `score` as matching criterion using argument
  `match`.

##### DOCUMENTATION

- Documentation of the package was updated.
- Some typos were fixed.
- Documentation of main functions was updated:
  - S3 methods are now referenced in See Also sections.
  - S3 methods are now documented in seperate files.
- CITATION file was updated.
- Formatting was improved.

### Minor updates

- Legends in
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md),
  [`plot.difORD()`](https://adelahladka.github.io/difNLR/reference/plot.difORD.md)
  and
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  were unified.
- Colors in
  [`plot.difORD()`](https://adelahladka.github.io/difNLR/reference/plot.difORD.md)
  and
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  were changed to blind-color friendly palettes.

------------------------------------------------------------------------

## difNLR 1.3.2 (2020-01-28)

CRAN release: 2020-01-28

***THIS IS A CRAN VERSION***

### Bug fixes

- Bug in
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  was fixed.

------------------------------------------------------------------------

## difNLR 1.3.1 (2020-01-27)

CRAN release: 2020-01-27

***THIS IS A CRAN VERSION***

*It includes versions 1.3.0-1 - 1.3.0-6 and following changes:*

### Bug fixes

- Method
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  now correctly uses matching criterion when item purification is
  applied.

##### DOCUMENTATION

- Documentation of the package was updated.
- Some typos were fixed.

### Minor updates

- NEWS file is now generated using `markdown`.
- README file was updated.

------------------------------------------------------------------------

## difNLR 1.3.0-6 (2020-01-22)

### Bug fixes

- Mismatch in null and alternative models was fixed for all functions.
- [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md)
  function now returns correct value of log-likelihood for alternative
  model.

### Major updates

- Default option of argument type in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function was set to `"all"` instead of `"both"`.
- Input `Data` in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function can be also a vector now.

------------------------------------------------------------------------

## difNLR 1.3.0-5 (2020-01-20)

### Bug fixes

- Function
  [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md) was
  fixed for binary data and IRT parametrization.
- Typo fixed in `print.difORD()` method.
- Method
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  was fixed for binary data.

------------------------------------------------------------------------

## difNLR 1.3.0-4 (2020-01-17)

### Major updates

- Function `ddfORD()` was renamed to
  [`difORD()`](https://adelahladka.github.io/difNLR/reference/difORD.md).

### Minor updates

- Function
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  with an option `itemtype = "nominal"` returns nominal items as factors
  with levels presented by capital letters.
- Legend for
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  was updated to show P(Y = option) instead of option alone.
- README file updated.
- Typos fixed.

------------------------------------------------------------------------

## difNLR 1.3.0-3 (2020-01-07)

##### DOCUMENTATION

- Documentation was updated.
- Authors’ details were updated.

### Major updates

- Seed was added for re-calculation of bootstrapped initial values in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  estimation.

### Minor updates

- Argument `item` for S3 methods of `difNLR` class can be now name of
  the column in `Data`.
- Legends in
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  and `plot.ddfORD()` were updated.
- Some typos were fixed.

------------------------------------------------------------------------

## difNLR 1.3.0-2 (2020-01-03)

### Major updates

- Default option of argument type in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was set to `"all"` instead of `"both"`.

### Minor updates

- Package `styler` was used to improve formatting of the code.
- Package `ShinyItemAnalysis` was added into Suggests.
- Figures for README were updated.

##### DOCUMENTATION

- Documentation for all data was updated. Format of data was fixed.
- Documentation of
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  was improved.

------------------------------------------------------------------------

## difNLR 1.3.0-1 (2019-12-16)

### Bug fixes

- Legend in `plot.ddfORD()` is now correctly displayed.

------------------------------------------------------------------------

## difNLR 1.3.0 (2019-08-19)

CRAN release: 2019-08-20

***THIS IS A CRAN VERSION***

*It includes versions 1.2.3 - 1.2.8-4 and following changes:*

### Minor updates

- Some typos fixed in `print.difNLR()`
- Authors’ details were updated.
- CITATION file was updated.

------------------------------------------------------------------------

## difNLR 1.2.8-4 (2019-08-09)

### Minor updates

- Typos fixed in `print.ddfORD()` and print.ddfMLR().
- Matching criterion for `plot.ddfORD()` uses anchor items.

------------------------------------------------------------------------

## difNLR 1.2.8-3 (2019-08-08)

### Bug fixes

- `plot.ddfORD()` now works when Data is factor.

### Minor updates

- [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  now generates ordinal data using adjacent category logit model with
  argument `itemtype = "ordinal"`.

------------------------------------------------------------------------

## difNLR 1.2.8-2 (2019-08-07)

### Bug fixes

- `plot.ddfORD()` now works when items have different scales.
- Argument `anchor` is now used for calculation of matching criterion in
  function
  [`ORD()`](https://adelahladka.github.io/difNLR/reference/ORD.md).
- IRT parametrization was fixed for `ddfORD()`.
- [`logLik.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/logLik.ddfMLR.md)
  now works properly.
- anchor items are now used for calculation of matching criterion in
  `plot.ddfORD()` and
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md).

### Major updates

- Names of reference and focal group in
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  can be changed with `group.name` argument.

### Minor updates

- Help pages for
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md),
  `ddfORD()`,
  [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md), and
  [`ORD()`](https://adelahladka.github.io/difNLR/reference/ORD.md)
  functions were updated.

------------------------------------------------------------------------

## difNLR 1.2.8-1 (2019-08-06)

### Minor updates

- IRT parametrization now available in
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  function with argument `parametrization`. SE calculated with delta
  method.
- Names of reference and focal group in
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  can be changed with `group.name` argument.

------------------------------------------------------------------------

## difNLR 1.2.8 (2019-08-05)

### Major updates

- `ddfORD()` function was renamed. Now `ddfORD()`.
- IRT parametrization now available in `ddfORD()` function with argument
  `parametrization`. SE calculated with delta method.
- Names of reference and focal group in `plot.ddfORD()` can be changed
  with `group.name` argument.

### Minor updates

- Help page for `ddfORD()` was updated.
- Reference for `ddfORD()` was added.

------------------------------------------------------------------------

## difNLR 1.2.7 (2019-07-04)

### Bug fixes

- Check for input `item` in S3 methods for
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md),
  and `ddfORD()` was fixed.

##### MAJOR UDPATES

- S3 methods [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  outputs for
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md),
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md),
  and `ddfORD()` functions were unified.

### Minor updates

- Help pages were updated.
- README file was updated.

------------------------------------------------------------------------

## difNLR 1.2.6 (2019-07-03)

##### MAJOR UDPATES

- S3 method [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  `ddfORD()` was implemented.

------------------------------------------------------------------------

## difNLR 1.2.5 (2019-07-02)

### Major updates

- S3 methods [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`coef()`](https://rdrr.io/r/stats/coef.html) for `ddfORD()` were
  implemented.
- S3 methods [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) for
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  objects now handle column names as `item` argument.
- S3 method [`coef()`](https://rdrr.io/r/stats/coef.html) for `difNLR`
  and `ddfMLR` objects were updated. Their now includes arguments `SE`
  (logical) to print standard errors and `simplify` (logical) whether
  list of estimates should be simplified into a matrix.

### Minor updates

- CITATION was updated.
- All static DOI links were updated.

------------------------------------------------------------------------

## difNLR 1.2.4 (2019-07-02)

### Major updates

- New functions `ddfORD()` and
  [`ORD()`](https://adelahladka.github.io/difNLR/reference/ORD.md) for
  DDF detection for ordinal data with adjacent and cumulative logistic
  regression models were added. Output is displayed via S3 method
  `print.ddfORD()`

### Minor updates

- Authors’ details were updated.
- Some typos were fixed.
- Helps for
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md),
  [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md), and
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  were updated.

------------------------------------------------------------------------

## difNLR 1.2.3 (2019-06-20)

### Bug fixes

- [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  now handles also binary data.
- [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  returns consistently `"No DDF item detected"` when no DDF item was
  detected.

### Major updates

- Matching criterion for
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  was improved for displaying more smooth curves.

### Minor updates

- Authors’ details were updated.

------------------------------------------------------------------------

## difNLR 1.2.2 (2018-05-03)

CRAN release: 2018-05-04

***THIS IS A CRAN VERSION***

*It includes versions 1.2.1-1 - 1.2.1-3*

------------------------------------------------------------------------

## difNLR 1.2.1-3 (2018-04-26)

### Minor updates

- S3 methods [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html) of
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  are now item specific.

------------------------------------------------------------------------

## difNLR 1.2.1-2 (2018-03-19)

### Bug fixes

- [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  - Check for constraints was fixed.
- [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  - `initboot = FALSE` now works properly.

------------------------------------------------------------------------

## difNLR 1.2.1-1 (2018-03-16)

### Bug fixes

- [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md):
  - P-value adjustment is now performed in the last iteration of
    purification as described.
  - In difPur output columns are properly named.
- [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md):
  - P-value adjustment is now performed in the last iteration of
    purification as described.
  - In difPur output columns are properly named.

### Minor updates

- Warning messages do not include the call.

------------------------------------------------------------------------

## difNLR 1.2.1 (2018-03-01)

CRAN release: 2018-03-05

***THIS IS A CRAN VERSION***

*It includes versions 1.2.0-1 - 1.2.0-7*

------------------------------------------------------------------------

## difNLR 1.2.0-7 (2018-02-27)

### Major updates

- Argument `start` in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function is now item-specific. The input is correctly checked.
- In case that some items do not converge, starting values are
  recalculated from bootstrapped sample and problematic models are
  fitted again. This is done 20 times at most. The options were added
  into
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  functions.
- Argument `constraints` in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function is now item-specific.

### Minor updates

- Minor typos were fixed in
  [`print()`](https://rdrr.io/r/base/print.html) method for `difNLR`
  class.
- Title was shorten.
- Description of package was updated.
- Description file was updated, reference was added.
- README file was updated.
- CITATION file was updated.

------------------------------------------------------------------------

## difNLR 1.2.0-6 (2018-02-26)

### Major updates

- S3 methods for `difNLR` class are now properly described, especially,
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  and
  [`predict.difNLR()`](https://adelahladka.github.io/difNLR/reference/predict.difNLR.md).
- [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  documentation was improved.

### Bug fixes

- S3 methods for class `difNLR` can now properly handle items with
  convergence issues.
- [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) now
  detects DIF correctly with F test.

### Minor updates

- Typos were fixed.

------------------------------------------------------------------------

## difNLR 1.2.0-5 (2018-02-20)

### Major updates

- [`print()`](https://rdrr.io/r/base/print.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),[`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) for `difNLR`
  class now handles item specific arguments (`model`, `type` and
  `constraints`).
- `residuals` for `difNLR` class now uses argument `item`.

### Bug fixes

- Checking inputs in `difNLR` was fixed and improved.
- Fixing degrees of freedom and p-values calculations in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md).
- Fixing parameters, SE and covariances calculations in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md).
- S3 methods for `difNLR` class can now handle convergence issues.

### Minor updates

- Documentation of `difNLR-package` was updated.
- Syntax in [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) for `difNLR`
  was slightly improved.
- [`logLik()`](https://rdrr.io/r/stats/logLik.html) for `difNLR` now
  returns list of `logLik` class values.

------------------------------------------------------------------------

## difNLR 1.2.0-4 (2018-02-19)

### Major updates

- Function
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  now handles item-specific arguments (`model` and `parameterization`).
  Its output is now in the form of list. It can be simplified with
  argument `simplify` into table when all parameterizations are the
  same.
- Function
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) now
  handles item-specific arguments (`model`, `type` and `constraints`).
- Function
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  now handles item-specific arguments (`model`, `type` and
  `constraints`).

### Minor updates

- README file was updated.

------------------------------------------------------------------------

## difNLR 1.2.0-3 (2018-02-19)

### Bug fixes

- Starting values in input of
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  in [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  are now properly named.
- Bug in alternative parameterization for testing differences in
  parameters c and d in function
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  was fixed.

### Minor updates

- Descriptions of
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  and
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  were improved.

------------------------------------------------------------------------

## difNLR 1.2.0-2 (2018-02-15)

### Major updates

- Function
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  can now also generate nominal data based on model specified in
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md).
- Argument `parameters` in
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  is no longer applicable.
- Arguments `a`, `b`, `c`, `d` were added into
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  as parameters - discrimination, difficulty, guessing, inattention
- Function
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  can now also generate different underlying distributions for reference
  and focal group with arguments `mu` and `sigma`.

### Minor updates

- Email address of maintainer was changed.

------------------------------------------------------------------------

## difNLR 1.2.0-1 (2018-02-14)

### Major updates

- New function
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  to estimate parameters of NLR models was added. This function uses
  non-linear least squares or maximum likelihood method.
- Function
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) now
  uses
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  for estimation of models parameters.
- Function
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  can now estimate models parameters with also maximum likelihood
  method.
- Iteratively reweighted least squares (IRLS) method was added into
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  function. This option is not fully functional.

### Bug fixes

- Bug in [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  `ddfMLR` class in matching criterion was fixed.
- Bug in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) was
  fixed. User-specified starting values are now available.
- Bug in
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  was fixed. Function runs even if there are not unique cuts for total
  scores/match.
- Bug in log-likelihood calculation in
  [`estimNLR()`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)
  was fixed.

### Minor updates

- Some preparation for new estimation methods for F test in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) was
  done.
- Convergence failure warning is now item specific.
- Warning and error messages were improved.

------------------------------------------------------------------------

## difNLR 1.1.3 (2018-02-06)

### Bug fixes

- Bug in delta method in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function was fixed.
- Bug in `match` argument in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was fixed.
- Bug in one dimensional `Data` in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was fixed.

### Major updates

- Specification of upper and lower asymptotes in
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function was improved.
- Functions
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  and [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md)
  can now handle also total score or other user-specified matching
  criterion.
- S3 functions [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  for class `ddfMLR` can also handle total score or other user-specified
  matching criterion.

### Minor updates

- New auxiliary function `checkInterval()` was added.
- Size of labs and title was unified in graphical outputs of functions
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md).

------------------------------------------------------------------------

## difNLR 1.1.2 (2017-12-12)

### Major updates

- CITATION file was added with reference to relevant paper.
- Bug when loading group by group identificator was fixed.
- Condition to check dimension of complete cases data was added.
- Function
  [`residuals.difNLR()`](https://adelahladka.github.io/difNLR/reference/residuals.difNLR.md)
  was added.
- S3 functions [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) for `difNLR` class were
  updated.
- S3 functions [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html) for `difNLR` class
  can now handle also other matching criteria than `zscore`.

### Minor updates

- Reference lists were updated.
- README file was updated.

------------------------------------------------------------------------

## difNLR 1.1.1 (2017-08-28)

CRAN release: 2017-08-29

***THIS IS A CRAN VERSION***

### Major updates

- Bug in
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function for missing values was fixed.

### Minor updates

- Graphical representation for
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions was mildly updated and unified.

------------------------------------------------------------------------

## difNLR 1.1.0 (2017-08-21)

CRAN release: 2017-08-22

***THIS IS A CRAN VERSION***

### Minor updates

- Bug in
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  was fixed.
- README file was updated.
- Package documentation was updated.
- Default value for `constraints` arguments in
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) and
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  functions were set to `NULL`.
- Default starting values were added into
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function by
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  function.

------------------------------------------------------------------------

## difNLR 1.0.8 (2017-08-14)

### Bug fixes

- Several bugs were fixed:
  - [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
    function can handle `Data` with one column.
  - [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
    now works when `match` argument is set.
  - Check input condition was fixed in
    [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
    function.
  - Delta method in
    [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
    function.

### Minor updates

- Function
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  was mildly updated.

------------------------------------------------------------------------

## difNLR 1.0.7 (2017-08-10)

### Major updates

- Item purification was implemented into
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  function.
- Anchor items were implemented into
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  function.
- Anchor items were implemented into
  [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md)
  function.

### Minor updates

- Minor bug in
  [`logLik.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/logLik.ddfMLR.md)
  function was fixed.
- Documentation of
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  was updated.

------------------------------------------------------------------------

## difNLR 1.0.6 (2017-08-09)

### Major updates

- Item purification was implemented into
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.
- Anchor items were implemented into
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.
- Anchor items were implemented into
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function.

### Minor updates

- README file was updated.

------------------------------------------------------------------------

## difNLR 1.0.5 (2017-08-09)

### Major updates

- Datasets `difMedical`, `difMedicaltest`, and `difMedicalkey` were
  renamed. Now they are `MSATB`, `MSATBtest`, and `MSATBkey`. from
  Medical School Admission Test in Biology.

### Minor updates

- LazyData is now available.
- References were updated.
- README file updated.

------------------------------------------------------------------------

## difNLR 1.0.4 (2017-08-08)

### Major updates

- New function
  [`formulaNLR()`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)
  was implemented. Function returns formula for NLR model for 11
  predefined models and 4 predefined DIF types to test. Model and DIF
  type can be also specified with constraints on parameters a, b, c and
  d.
- Function
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) now
  handles 11 predefined models and 4 predefined DIF types to test. Model
  and DIF type can be also specified with constraints on parameters a,
  b, c and d.
- Function
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md)
  was edited to return starting parameters with different
  parameterization. It was also mildly changed to correspond to new
  version of
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md)
  function.
- Function
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  can now handle also total score or other user-specified matching
  score.
- Function `constrNLR()` is no longer part of the `difNLR` package.

### Minor updates

- References were updated.
- Some minor bugs were fixed:
  - Items are no longer renamed by
    [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
    and
    [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
    functions.
  - Starting values are now correctly checked in
    [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
    function.
- `msm` package is now used for delta method in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function.

------------------------------------------------------------------------

## difNLR 1.0.3 (2017-06-15)

CRAN release: 2017-06-16

***THIS IS A CRAN VERSION***

### Major updates

- Bug of
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  for non-uniform DDF was fixed.
- References were updated.

------------------------------------------------------------------------

## difNLR 1.0.2 (2017-06-06)

CRAN release: 2017-06-06

***THIS IS A CRAN VERSION***

### Major updates

- Bug of dimensions for parameter estimates of
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was fixed.
- Datasets `GMAT` and `GMATtest` were extended by `criterion` variable
  which is intended to be predicted by test.
- `coef`, `logLik`, `AIC` and `BIC` S3 methods were added for class
  `ddfMLR`.

### Minor updates

- Functions
  [`plot.ddfMLR()`](https://adelahladka.github.io/difNLR/reference/plot.ddfMLR.md)
  and
  [`plot.difNLR()`](https://adelahladka.github.io/difNLR/reference/plot.difNLR.md)
  were slightly improved.
- Updated error and warning handling in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  and
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  functions.
- Description file was updated.
- Item names are now the same as in original data set.
- README file updated.

------------------------------------------------------------------------

## difNLR 1.0.0 (2017-01-10)

CRAN release: 2017-01-24

***THIS IS A CRAN VERSION***

### Major updates

- New function
  [`ddfMLR()`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)
  to detect Differential Distractor Functioning (DDF) with Multinomial
  Log-linear Regression (MLR) model. S3 methods for class `ddfMLR` also
  added - `print` and `plot`.
- New function
  [`MLR()`](https://adelahladka.github.io/difNLR/reference/MLR.md) to
  calculate likelihood ratio statistic for detecting DDF with MLR model.
- The
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function can handle 6 generalized logistic regression models with
  option `model`.
- Functions
  [`startNLR()`](https://adelahladka.github.io/difNLR/reference/startNLR.md),
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  and S3 methods for class `difNLR` were changed according
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function. S3 method `coef` was created.
- New functions
  [`NLR()`](https://adelahladka.github.io/difNLR/reference/NLR.md) and
  `constrNLR()` can now calculates DIF detection statistics and specify
  constraints for generalized logistic regression model.
- Function
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  was edited to response to `difR` package and its DIF detection
  functions.
- Function
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  was changed to generate dataset from generalized logistic regression
  model with 8 parameters.

### Minor updates

- The CITATION file was updated.
- Several typos were fixed.
- Some default options of input were changed.
- [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html), and
  [`logLik()`](https://rdrr.io/r/stats/logLik.html) S3 methods added to
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md).

------------------------------------------------------------------------

## difNLR 0.2.0 (2016-11-09)

CRAN release: 2016-11-09

***THIS IS A CRAN VERSION***

### Major updates

- S3 method `plot` for class `difNLR` was updated.
- New option of `test` in
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function was added. Possible choices are now `F` for F-test and `LR`
  for likelihood ratio test.
- Choice of significant level `alpha` was added into
  [`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
  function with default option 0.05.
- Six new data sets were added - scored `GMAT` data, its unscored
  version `GMATtest` and its key `GMATkey`. Scored `difMedical` data
  set, its unscored version `difMedicaltest` and key `difMedicalkey`.
- New function
  [`genNLR()`](https://adelahladka.github.io/difNLR/reference/genNLR.md)
  was added to generate scored (binary) data with model by `difNLR`.

### Minor updates

- Several typos were fixed.
