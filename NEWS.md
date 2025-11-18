
# difNLR 1.5.2 (2025-11-18)

**_THIS IS A CRAN VERSION_**

## Major updates
  * `match` argument of the `difNLR()`, `difORD()`, and `ddfMLR()` now handles 
    a numeric matrix where each column represents the matching criterion for 
    each item of `Data`.
  * `match` argument of the `difNLR()`, `difORD()`, and `ddfMLR()` now can take
    also values of `"restscore"` and `"zrestscore"` representing total score
    without item being currently tested and its standardized version,
    respectively.
  * Argument `constraints` is now used in the `startNLR()` function when 
    computing starting values for the `difNLR()` function.
    
## Minor updates
  * Tests extended and updated. 
  * Checking inputs of the `difNLR()`, `difORD()`, and `ddfMLR()` functions was 
    updated.
  * Documentation of the `difNLR()`, `difORD()`, and `ddfMLR()` functions was
    updated; specifically for `match`, `anchor`, and `purify` arguments. New
    output `match.name` describing name of the matching criterion for plotting 
    was added.

------

# difNLR 1.5.1-4 (2025-06-30)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.5.1-2 - 1.5.1-4*

## Bug fixes
  * Minor bug in the `plot()` S3 method for the `ddfMLR()` was fixed. Categories
    are now correctly ordered for lines and points.
    
## Minor updates
  * Tests extended and updated. 
  * Checking inputs of the `difNLR()` and `difORD()` functions was updated.
  * `genNLR()` now handles parameters for dichotomous items as matrices with 
    a single column.
  * Legend of plots moved back inside of the plots. 

------

# difNLR 1.5.1-3 (2025-06-08)

## Bug fixes
  * Typos in equations of generalized logistic model in the `difNLR` function 
    were fixed.
  * Parametrization for the parameters "c" and "d" in the `predict.difNLR` was 
    fixed, fixing also issues in the `plot.difNLR`.
  * Bug in degrees of freedom in the `df` output of the `difNLR()` function was
    fixed. Degrees of freedom are now correctly returned for the LR and Wald 
    tests. 
  * When `purify = TRUE` or `anchor` argument is used, matching criterion is now
    correctly computed in the `plot()` and `predict()` S3 methods for the 
    `difNLR()` function.
  * Bug in the `fitted()` S3 method for the `difNLR` was fixed.

## Minor updates
  * References were updated.
  * Tests extended and updated.
  * Check of the `start` input in the `difNLR()` function was updated.
  * Check of inputs was updated in the `genNLR()`, `difNLR()`, `difORD()`, and
    `ddfMLR()` functions.
  * Error messages were updated for the `difNLR()`, `difORD()`, `ddfMLR()`, 
    `formulaNLR()`, and `genNLR()` functions. 
  * Output `anchor` was added into the `difNLR()`, `difORD()`, and `ddfMLR()` 
    functions, specifying anchoring items from the `anchor` argument. This also 
    excludes DIF items when item purification is applied with the `purify = TRUE` 
    argument.
  
------

# difNLR 1.5.1-2 (2025-03-05)

## Bug fixes
  * Bug in `NLR()` when `method = "irls"` fixed. Parameters are now labelled as
    b0-b3 and estimates are correctly printed with the `coef.difNLR()` S3 method. 
  * Bug in `coef.difNLR()` S3 method fixed when only one parameter is
    estimated. Thanks to Jan Netik.
  * Bug in `coef.difNLR()` S3 method fixed when multiple items did not converged. 
  * Bug in `difNLR()` handling items that failed to converge and their estimates.

## Minor updates
  * Tests updated.

------

# difNLR 1.5.1-1 (2025-03-03)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.5.0-1 - 1.5.0-2*

------

# difNLR 1.5.0-2 (2025-02-27)

## Bug fixes
  * Bug in `formulaNLR()` when `type = "b"` fixed. 
  * Bug in `estimNLR()` when `method = "plf"` fixed. 
  * Bug in `NLR()` calculating SEs when no item is converged fixed. 
  
## Major updates
  * Testing with the `testthat` package has been started. 
  
## Minor updates
  * References updated.
  
------

# difNLR 1.5.0-1 (2025-02-17)

## Bug fixes
  * Bug in re-calculation of starting values in the `NLR()` function was fixed.
  * Bugs in `coef.difNLR()` when convergence issues are present were fixed. 
    Thanks to Jan Netik.

## Minor updates
  * Warnings and messages in the `NLR()` and `difNLR()` functions were updated.
  * Documentation of the `coef.difNLR()` was updated.
  * Some typos were fixed.
  * README file was updated.
  * Description of the package was updated. 

------

# difNLR 1.5.0 (2024-12-14)

**_THIS IS A CRAN VERSION_**

## Bug fixes
  * Bug caused by mixing two parametrization in `predict.difNLR()` was fixed. 
  * Bug in calculation of starting values with `startNLR()` was fixed. 
  
## Major updates
  * Options `"em"` and `"plf"` were added for the `method` argument in the 
    `estimNLR()` function to estimate item parameters with either the EM 
    algorithm or algorithm based on parametric link function (PLF). "plf" is now
    default option. This is also the default option for the `NLR()` function.
  * Options  for the `parameterization` argument of the `formulaNLR()` and 
    `startNLR()` function were updated (renamed). 
  * Intercept-slope parameterization is now used for model fitting, so slots of
    fitted objects like `parM0` are returned with this parameterization.
  * Argument `constraints` were added into the `startNLR()` function. 
  
## Minor updates
  * References were updated.

------

# difNLR 1.4.3 (2023-05-03)

## Major updates
  * `"likelihood"` option for maximum likelihood estimation in the `estimNLR()`
    function was renamed to `"mle"`.
  * S3 methods for the `estimNLR()` function were extended and improved. 

------

# difNLR 1.4.2-1 (2023-05-03)

**_THIS IS A CRAN VERSION_**

## Minor updates
  * CITATION file was updated.

------

# difNLR 1.4.2 (2023-05-02)

## Bug fixes
  * `plot.ddfMLR()` now correctly plots ordinal data.  
  * Option `test = "W"` was fixed for the `difNLR()` and `NLR()` functions.
  * Constraints handling was fixed for the `difNLR()` and `NLR()` functions.
    
## Minor updates
  * `startNLR` now handles missing values. Returns error when not enough complete 
    observations are provided. 
  * Arguments in `ggplot2` plotting methods were updated to follow changes in 
    the `ggplot2` package.
  * Linetype and its legend appearance in `ggplot2` plotting methods were
    updated. 
  * `ggplot2` v.3.4.0 is now imported. 
  * Examples for the `difORD()` and `ORD()` functions were updated. Now using 
    the `Anxiety` dataset from the `ShinyItemAnalysis` package.

------

# difNLR 1.4.1 (2022-04-18)

## Minor updates
  * Some typos fixed.
  * `class` handling was updated. 

------

# difNLR 1.4.0 (2022-04-16)

*It includes versions 1.3.7-1 - 1.3.7-3*

------

# difNLR 1.3.7-3 (2022-02-11)

## Bug fixes
  * Option `parameterization = "logistic"` was fixed in `formulaNLR()` 
    function. 
    
## Major updates
  * First version of iteratively reweighted least squares algorithm 
    was implemented in `difNLR()`, `NLR()`, and `estimNLR()` functions.
  * `coef.difNLR()`, `coef.difORD()`, and `coef.ddfMLR()` methods now 
    include delta method for IRT and logistic parameterizations. 
  * `coef.difNLR()`, `coef.difORD()`, and `coef.ddfMLR()` methods now 
    include calculation of confidence intervals. 

## Minor updates
  * Some typos fixed.
  * Examples in functions were updated. 
  * References were updated.
  * Output of `estimNLR()` function is now unified via `print()` method.
  * Packages CTT, grDevices, methods are no longer imported. 
  
------

# difNLR 1.3.7-2 (2021-10-05)

## Major updates
  * First version of `predicted.difORD()` to compute predicted values for 
    `difORD` object was implemented. 

------  

# difNLR 1.3.7-1 (2021-02-16)

## Bug fixes
  * Bug in plotting empirical probabilities in `plot.difNLR()` fixed. 

------  

# difNLR 1.3.6/1.3.7 (2021-01-07)

**_THIS IS A CRAN VERSION_**

## Minor updates
  * doi in DESCRIPTION file updated.
  * doi in help pages were updated. 
  * CITATION file updated.
  * References were updated.
  
------  

# difNLR 1.3.5-2 (2020-11-24)

## Bug fixes
  * Empty factor levels were removed from `Data` in `ddfMLR()` to fix bug
    when plotting.

------  

# difNLR 1.3.5-1 (2020-09-03)

## Major updates
  * Sandwich estimator for covariance matrix in case that `method = "nls"` was 
    implemented into the `vcov()` method for the output of the `estimNLR()` 
    function.
  * Wald test for implemented for the `difNLR()` function.
  * Sandwich estimator for covariance matrix in case that `method = "nls"` was 
    implemented into the `difNLR()` function via an argument `sandwich = TRUE`.

------  

# difNLR 1.3.4/1.3.5 (2020-08-24)

**_THIS IS A CRAN VERSION_**

## Bug fixes
  * Error when covariance matrix cannot be computed for some items in `difNLR()`
    function was fixed.

#### DOCUMENTATION
  * URLs for GH were updated.
  
------  
  
# difNLR 1.3.3 (2020-04-29)

**_THIS IS A CRAN VERSION_**

## Bug fixes
  * Bug in calculation of standard errors for estimates of `difNLR()` 
    was fixed.
  * Bug in coefficients of `difNLR()` for non-converged items including naming 
    of parameters was fixed (Reported by Jan Netik).
  * In case that covariance matrix cannot be computed in `NLR()`, function 
    gives warning and `NA` values for covariance matrix and vector of standard
    errors are returned.
    
## Major updates
  * Confidence intervals were added into `predict.difNLR()` method.
  * Delta method for alternative parametrization is now applied for whole 
    covariance matrix in `difNLR()`.
  * Unnecessary arguments of `plot.difNLR()`, `plot.difORD()` and `plot.ddfMLR()`
    were removed. Change of colours/linetypes/shapes/title can be managed using
    standard `ggplot2` syntax.
  * `plot.difNLR()` now offers possibility to turn off drawing of empirical 
    probabilities using argument `draw.empirical = FALSE`.
  * `plot.difNLR()` now offers possibility to plot confidence intervals for 
    predicted values as offered in `predict.difNLR()` using argument 
    `draw.CI = TRUE`.
  * Starting values calculated via `startNLR()` were improved for `score` as 
    matching criterion using argument `match`.

#### DOCUMENTATION
  * Documentation of the package was updated.
  * Some typos were fixed.
  * Documentation of main functions was updated:
    - S3 methods are now referenced in See Also sections.
    - S3 methods are now documented in seperate files.
  * CITATION file was updated.
  * Formatting was improved.

## Minor updates
  * Legends in `plot.difNLR()`, `plot.difORD()` and `plot.ddfMLR()` were unified.
  * Colors in `plot.difORD()` and `plot.ddfMLR()` were changed to blind-color
    friendly palettes.

------

# difNLR 1.3.2 (2020-01-28)

**_THIS IS A CRAN VERSION_**

## Bug fixes
  * Bug in `plot.difNLR()` was fixed.
    
------

# difNLR 1.3.1 (2020-01-27)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.3.0-1 - 1.3.0-6 and following changes:*

## Bug fixes
  * Method `plot.difNLR()` now correctly uses matching criterion when item purification
    is applied.
    
#### DOCUMENTATION
  * Documentation of the package was updated.
  * Some typos were fixed.

## Minor updates
  * NEWS file is now generated using `markdown`.
  * README file was updated.
  
------

# difNLR 1.3.0-6 (2020-01-22)

## Bug fixes
  * Mismatch in null and alternative models was fixed for all functions.
  * `MLR()` function now returns correct value of log-likelihood for
    alternative model.

## Major updates
  * Default option of argument type in `NLR()` function was set
    to `"all"` instead of `"both"`.
  * Input `Data` in `difNLR()` function can be also a vector now.

------

# difNLR 1.3.0-5 (2020-01-20)

## Bug fixes
  * Function `MLR()` was fixed for binary data and IRT parametrization.
  * Typo fixed in `print.difORD()` method.
  * Method `plot.ddfMLR()` was fixed for binary data.

------

# difNLR 1.3.0-4 (2020-01-17)

## Major updates
  * Function `ddfORD()` was renamed to `difORD()`.

## Minor updates
  * Function `genNLR()` with an option `itemtype = "nominal"` returns
    nominal items as factors with levels presented by capital letters.
  * Legend for `plot.ddfMLR()` was updated to show P(Y = option) instead
    of option alone.
  * README file updated.
  * Typos fixed.

------

# difNLR 1.3.0-3 (2020-01-07)

#### DOCUMENTATION
  * Documentation was updated.
  * Authors' details were updated.

## Major updates
  * Seed was added for re-calculation of bootstrapped initial values
    in `NLR()` estimation.
    
## Minor updates
  * Argument `item` for S3 methods of `difNLR` class can be now
    name of the column in `Data`.
  * Legends in `plot.ddfMLR()` and `plot.ddfORD()` were updated.
  * Some typos were fixed.

------

# difNLR 1.3.0-2 (2020-01-03)

## Major updates
  * Default option of argument type in `difNLR()` function was set
    to `"all"` instead of `"both"`.

## Minor updates
  * Package `styler` was used to improve formatting of the code.
  * Package `ShinyItemAnalysis` was added into Suggests.
  * Figures for README were updated.

#### DOCUMENTATION
  * Documentation for all data was updated. Format of data was fixed.
  * Documentation of `estimNLR()` was improved.
  
------

# difNLR 1.3.0-1 (2019-12-16)

## Bug fixes
  * Legend in `plot.ddfORD()` is now correctly displayed.

------

# difNLR 1.3.0 (2019-08-19)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.3 - 1.2.8-4 and following changes:*

## Minor updates
  * Some typos fixed in `print.difNLR()`
  * Authors' details were updated.
  * CITATION file was updated.

------

# difNLR 1.2.8-4 (2019-08-09)

## Minor updates
  * Typos fixed in `print.ddfORD()` and print.ddfMLR().
  * Matching criterion for `plot.ddfORD()` uses anchor items.

------

# difNLR 1.2.8-3 (2019-08-08)

## Bug fixes
  * `plot.ddfORD()` now works when Data is factor.

## Minor updates
  * `genNLR()` now generates ordinal data using adjacent category
    logit model with argument `itemtype = "ordinal"`.

------

# difNLR 1.2.8-2 (2019-08-07)

## Bug fixes
  * `plot.ddfORD()` now works when items have different scales.
  * Argument `anchor` is now used for calculation of matching
    criterion in function `ORD()`.
  * IRT parametrization was fixed for `ddfORD()`.
  * `logLik.ddfMLR()` now works properly.
  * anchor items are now used for calculation of matching
    criterion in `plot.ddfORD()` and `plot.ddfMLR()`.

## Major updates
  * Names of reference and focal group in `plot.difNLR()` can be
    changed with `group.name` argument.

## Minor updates
  * Help pages for `difNLR()`, `ddfMLR()`, `ddfORD()`, `MLR()`, and `ORD()`
    functions were updated.

------

# difNLR 1.2.8-1 (2019-08-06)

## Minor updates
  * IRT parametrization now available in `ddfMLR()` function with
    argument `parametrization`. SE calculated with delta method.
  * Names of reference and focal group in `plot.ddfMLR()` can be
    changed with `group.name` argument.

------

# difNLR 1.2.8 (2019-08-05)

## Major updates
  * `ddfORD()` function was renamed. Now `ddfORD()`.
  * IRT parametrization now available in `ddfORD()` function with
    argument `parametrization`. SE calculated with delta method.
  * Names of reference and focal group in `plot.ddfORD()` can be
    changed with `group.name` argument.

## Minor updates
  * Help page for `ddfORD()` was updated.
  * Reference for `ddfORD()` was added.

------

# difNLR 1.2.7 (2019-07-04)

## Bug fixes
  * Check for input `item` in S3 methods for `difNLR()`, `ddfMLR()`, and `ddfORD()`
    was fixed.

#### MAJOR UDPATES
  * S3 methods `plot()` outputs for `difNLR()`, `ddfMLR()`, and `ddfORD()` functions 
  were unified.

## Minor updates
  * Help pages were updated.
  * README file was updated.

------

# difNLR 1.2.6 (2019-07-03)

#### MAJOR UDPATES
  * S3 method `plot()` for `ddfORD()` was implemented.

------

# difNLR 1.2.5 (2019-07-02)

## Major updates
  * S3 methods `AIC()`, `BIC()`, `logLik()`, `coef()` for `ddfORD()` were implemented.
  * S3 methods `AIC()`, `BIC()`, `logLik()`, `residuals()` for `difNLR()` and `ddfMLR()`
    objects now handle column names as `item` argument.
  * S3 method `coef()` for `difNLR` and `ddfMLR` objects were updated. Their now
    includes arguments `SE` (logical) to print standard errors and `simplify`
    (logical) whether list of estimates should be simplified into a matrix.

## Minor updates
  * CITATION was updated.
  * All static DOI links were updated.

------

# difNLR 1.2.4 (2019-07-02)

## Major updates
  * New functions `ddfORD()` and `ORD()` for DDF detection for ordinal data
    with adjacent and cumulative logistic regression models were added.
    Output is displayed via S3 method `print.ddfORD()`

## Minor updates
  * Authors' details were updated.
  * Some typos were fixed.
  * Helps for `ddfMLR()`, `MLR()`, and `difNLR()` were updated.

------

# difNLR 1.2.3 (2019-06-20)

## Bug fixes
  * `plot.ddfMLR()` now handles also binary data.
  * `ddfMLR()` returns consistently `"No DDF item detected"` when no DDF
    item was detected.

## Major updates
  * Matching criterion for `plot.ddfMLR()` was improved for displaying
    more smooth curves.

## Minor updates
  * Authors' details were updated.

------

# difNLR 1.2.2 (2018-05-03)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.1-1 - 1.2.1-3*

------

# difNLR 1.2.1-3 (2018-04-26)

## Minor updates
  * S3 methods `AIC()`, `BIC()`, `logLik()` of `ddfMLR()` are now item specific.

------

# difNLR 1.2.1-2 (2018-03-19)

## Bug fixes
  * `difNLR()`
    - Check for constraints was fixed.
  * `NLR()`
    - `initboot = FALSE` now works properly.

------

# difNLR 1.2.1-1 (2018-03-16)

## Bug fixes
  * `difNLR()`:
    - P-value adjustment is now performed in the last iteration of purification
      as described.
    - In difPur output columns are properly named.
  * `ddfMLR()`:
    - P-value adjustment is now performed in the last iteration of purification
      as described.
    - In difPur output columns are properly named.

## Minor updates
  * Warning messages do not include the call.

------

# difNLR 1.2.1 (2018-03-01)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.0-1 - 1.2.0-7*

------

# difNLR 1.2.0-7 (2018-02-27)

## Major updates
  * Argument `start` in `difNLR()` function is now item-specific. The input is
    correctly checked.
  * In case that some items do not converge, starting values are recalculated
    from bootstrapped sample and problematic models are fitted again. This is done
    20 times at most.
    The options were added into `difNLR()` and `NLR()` functions.
  * Argument `constraints` in `difNLR()` function is now item-specific.

## Minor updates
  * Minor typos were fixed in `print()` method for `difNLR` class.
  * Title was shorten.
  * Description of package was updated.
  * Description file was updated, reference was added.
  * README file was updated.
  * CITATION file was updated.

------

# difNLR 1.2.0-6 (2018-02-26)

## Major updates
  * S3 methods for `difNLR` class are now properly described, especially,
    `plot.difNLR()` and `predict.difNLR()`.
  * `difNLR()` documentation was improved.

## Bug fixes
  * S3 methods for class `difNLR` can now properly handle items with convergence 
    issues.
  * `NLR()` now detects DIF correctly with F test.

## Minor updates
  * Typos were fixed.

------

# difNLR 1.2.0-5 (2018-02-20)

## Major updates
  * `print()`, `plot()`,`fitted()`, `predict()`, `logLik()`, `AIC()`, `BIC()`
    and `residuals()` for `difNLR` class now handles item specific arguments
    (`model`, `type` and `constraints`).
  * `residuals` for `difNLR` class now uses argument `item`.

## Bug fixes
  * Checking inputs in `difNLR` was fixed and improved.
  * Fixing degrees of freedom and p-values calculations in `NLR()`.
  * Fixing parameters, SE and covariances calculations in `NLR()`.
  * S3 methods for `difNLR` class can now handle convergence issues.

## Minor updates
  * Documentation of `difNLR-package` was updated.
  * Syntax in `plot()` and `residuals()` for `difNLR` was slightly improved.
  * `logLik()` for `difNLR` now returns list of `logLik` class values.

------

# difNLR 1.2.0-4 (2018-02-19)

## Major updates
  * Function `startNLR()` now handles item-specific arguments (`model` and
    `parameterization`). Its output is now in the form of list. It can be
    simplified with argument `simplify` into table when all parameterizations
    are the same.
  * Function `NLR()` now handles item-specific arguments (`model`, `type` and
    `constraints`).
  * Function `difNLR()` now handles item-specific arguments (`model`, `type`
    and `constraints`).
    
## Minor updates
  * README file was updated.

------

# difNLR 1.2.0-3 (2018-02-19)

## Bug fixes
  * Starting values in input of `estimNLR()` in `NLR()` are now properly named.
  * Bug in alternative parameterization for testing differences in parameters c and d in function `formulaNLR()` was fixed.
      
## Minor updates
  * Descriptions of `formulaNLR()` and `estimNLR()` were improved.

------

# difNLR 1.2.0-2 (2018-02-15)

## Major updates
  * Function `genNLR()` can now also generate nominal data based on
    model specified in `ddfMLR()`.
  * Argument `parameters` in `genNLR()` is no longer applicable.
  * Arguments `a`, `b`, `c`, `d` were added into `genNLR()` as parameters -
    discrimination, difficulty, guessing, inattention
  * Function `genNLR()` can now also generate different underlying
    distributions for reference and focal group with arguments `mu` and `sigma`.
    
## Minor updates
  * Email address of maintainer was changed.

------

# difNLR 1.2.0-1 (2018-02-14)

## Major updates
  * New function `estimNLR()` to estimate parameters of NLR models
    was added. This function uses non-linear least squares or maximum
    likelihood method.
  * Function `NLR()` now uses `estimNLR()` for estimation of models
    parameters.
  * Function `difNLR()` can now estimate models parameters with also maximum
    likelihood method.
  * Iteratively reweighted least squares (IRLS) method was added into `estimNLR()`
    function. This option is not fully functional.

## Bug fixes
  * Bug in `plot()` for `ddfMLR` class in matching criterion was fixed.
  * Bug in `NLR()` was fixed. User-specified starting values are now available.
  * Bug in `startNLR()` was fixed. Function runs even if there are not unique cuts for total scores/match.
  * Bug in log-likelihood calculation in `estimNLR()` was fixed.
    
## Minor updates
  * Some preparation for new estimation methods for F test in `NLR()` was done.
  * Convergence failure warning is now item specific.
  * Warning and error messages were improved.

------

# difNLR 1.1.3 (2018-02-06)

## Bug fixes
  * Bug in delta method in `NLR()` function was fixed.
  * Bug in `match` argument in `difNLR()` function was fixed.
  * Bug in one dimensional `Data` in `difNLR()` function was fixed.
    
## Major updates
  * Specification of upper and lower asymptotes in
    `startNLR()` function was improved.
  * Functions `ddfMLR()` and `MLR()` can now handle also total score
    or other user-specified matching criterion.
  * S3 functions `plot()` for class `ddfMLR` can also handle total score
    or other user-specified matching criterion.

## Minor updates
  * New auxiliary function `checkInterval()` was added.
  * Size of labs and title was unified in graphical outputs of functions
    `difNLR()` and `ddfMLR()`.

------

# difNLR 1.1.2 (2017-12-12)

## Major updates
  * CITATION file was added with reference to relevant paper.
  * Bug when loading group by group identificator was fixed.
  * Condition to check dimension of complete cases data was added.
  * Function `residuals.difNLR()` was added.
  * S3 functions `AIC()` and `BIC()` for `difNLR` class were
    updated.
  * S3 functions `plot()`, `fitted()` and `predict()` for `difNLR` class
    can now handle also other matching criteria than `zscore`.

## Minor updates
  * Reference lists were updated.
  * README file was updated.

------

# difNLR 1.1.1 (2017-08-28)

**_THIS IS A CRAN VERSION_**

## Major updates
  * Bug in `startNLR()` function for missing values was fixed.

## Minor updates
  * Graphical representation for `difNLR()` and `ddfMLR()` functions was
    mildly updated and unified.

------

# difNLR 1.1.0 (2017-08-21)

**_THIS IS A CRAN VERSION_**

## Minor updates
  * Bug in `plot.difNLR()` was fixed.
  * README file was updated.
  * Package documentation was updated.
  * Default value for `constraints` arguments in `NLR()` and `formulaNLR()`
    functions were set to `NULL`.
  * Default starting values were added into `NLR()` function
    by `startNLR()` function.

------

# difNLR 1.0.8 (2017-08-14)

## Bug fixes
  * Several bugs were fixed:
    - `difNLR()` function can handle `Data` with one column.
    - `startNLR()` now works when `match` argument is set.
    - Check input condition was fixed in `formulaNLR()` function.
    - Delta method in `NLR()` function.
    
## Minor updates
  * Function `startNLR()` was mildly updated.

------

# difNLR 1.0.7 (2017-08-10)

## Major updates
  * Item purification was implemented into `ddfMLR()` function.
  * Anchor items were implemented into `ddfMLR()` function.
  * Anchor items were implemented into `MLR()` function.

## Minor updates
  * Minor bug in `logLik.ddfMLR()` function was fixed.
  * Documentation of `difNLR()` was updated.
  
------

# difNLR 1.0.6 (2017-08-09)

## Major updates
  * Item purification was implemented into `difNLR()` function.
  * Anchor items were implemented into `difNLR()` function.
  * Anchor items were implemented into `NLR()` function.
  
## Minor updates
  * README file was updated.

------

# difNLR 1.0.5 (2017-08-09)

## Major updates
  * Datasets `difMedical`, `difMedicaltest`, and `difMedicalkey`
    were renamed. Now they are `MSATB`, `MSATBtest`, and `MSATBkey`.
    from Medical School Admission Test in Biology.
    
## Minor updates
  * LazyData is now available.
  * References were updated.
  * README file updated.

------

# difNLR 1.0.4 (2017-08-08)

## Major updates
  * New function `formulaNLR()` was implemented. Function
    returns formula for NLR model for 11 predefined models and 4
    predefined DIF types to test. Model and DIF type can be also
    specified with constraints on parameters a, b, c and d.
  * Function `NLR()` now handles 11 predefined models and 4
    predefined DIF types to test. Model and DIF type can be also
    specified with constraints on parameters a, b, c and d.
  * Function `startNLR()` was edited to return starting
    parameters with different parameterization. It was also mildly
    changed to correspond to new version of `NLR()` function.
  * Function `difNLR()` can now handle also total score or other
    user-specified matching score.
  * Function `constrNLR()` is no longer part of the `difNLR`
    package.

## Minor updates
  * References were updated.
  * Some minor bugs were fixed:
      - Items are no longer renamed by `difNLR()` and `ddfMLR()` functions.
      - Starting values are now correctly checked in `difNLR()` function.
  * `msm` package is now used for delta method in `difNLR()` function.

------

# difNLR 1.0.3 (2017-06-15)

**_THIS IS A CRAN VERSION_**

## Major updates
  * Bug of `plot.ddfMLR()` for non-uniform DDF was fixed.
  * References were updated.

------

# difNLR 1.0.2 (2017-06-06)

**_THIS IS A CRAN VERSION_**

## Major updates
  * Bug of dimensions for parameter estimates of `difNLR()` function was fixed.
  * Datasets `GMAT` and `GMATtest` were extended by `criterion` variable which is intended to be predicted by test.
  * `coef`, `logLik`, `AIC` and `BIC` S3 methods were added for class `ddfMLR`.

## Minor updates
  * Functions `plot.ddfMLR()` and `plot.difNLR()` were slightly improved.
  * Updated error and warning handling in `difNLR()` and `ddfMLR()` functions.
  * Description file was updated.
  * Item names are now the same as in original data set.
  * README file updated.

------

# difNLR 1.0.0 (2017-01-10)

**_THIS IS A CRAN VERSION_**

## Major updates
  * New function `ddfMLR()` to detect Differential Distractor Functioning (DDF) with Multinomial Log-linear Regression (MLR) model. S3 methods for class `ddfMLR` also added - `print` and `plot`.
  * New function `MLR()` to calculate likelihood ratio statistic
     for detecting DDF with MLR model.
  * The `difNLR()` function can handle 6 generalized logistic
     regression models with option `model`.
  * Functions `startNLR()`, `genNLR()` and S3 methods for class
     `difNLR` were changed according `difNLR()` function. S3 method
     `coef` was created.
  * New functions `NLR()` and `constrNLR()` can now calculates DIF
     detection statistics and specify constraints for generalized
     logistic regression model.
  * Function `difNLR()` was edited to response to `difR` package
     and its DIF detection functions.
  * Function `genNLR()` was changed to generate dataset from
     generalized logistic regression model with 8 parameters.

## Minor updates
  * The CITATION file was updated.
  * Several typos were fixed.
  * Some default options of input were changed.
  * `AIC()`, `BIC()`, and `logLik()` S3 methods added to `difNLR()`.
  
------

# difNLR 0.2.0 (2016-11-09) 

**_THIS IS A CRAN VERSION_**

## Major updates
  * S3 method `plot` for class `difNLR` was updated.
  * New option of `test` in `difNLR()` function was added. Possible
    choices are now `F` for F-test and `LR` for likelihood ratio test. 
  * Choice of significant level `alpha` was added into `difNLR()`
    function with default option 0.05.
  * Six new data sets were added - scored `GMAT` data, its unscored version
    `GMATtest` and its key `GMATkey`. Scored `difMedical` data set, its
    unscored version `difMedicaltest` and key `difMedicalkey`.
  * New function `genNLR()` was added to generate scored (binary) data with
    model by `difNLR`.

## Minor updates
  *  Several typos were fixed.
