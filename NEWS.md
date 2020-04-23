## Changes and developments in the difNLR package

### Changes in version 1.3.3 (2020-04-23)

**_THIS IS A CRAN VERSION_**

#### BUGFIXING
  * Bug in calculation of standard errors for estimates of `difNLR()` 
    was fixed.
    
#### MAJOR UPDATES
  * Confidence intervals were added into `predict.difNLR()` method.
  * Delta method for alternative parametrization is now applied for whole 
    covariance matrix in `difNLR()`.

#### DOCUMENTATION
  * Documentation of the package was updated.
  * Some typos were fixed.
  * Documentation of main functions was updated:
    - S3 methods are now referenced in See Also sections.
    - S3 methods are now documented in seperate files.
  * CITATION file was updated.
  * Formatting was improved.

#### MINOR UPDATES
  * Legends in `plot.difNLR()`, `plot.difORD()` and `plot.ddfMLR()` were unified.
  * Colors in `plot.difORD()` and `plot.ddfMLR()` were changed to blind-color
    friendly palettes.

------

### Changes in version 1.3.2 (2020-01-28)

**_THIS IS A CRAN VERSION_**

#### BUGFIXING
  * Bug in `plot.difNLR()` was fixed.
    
------

### Changes in version 1.3.1 (2020-01-27)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.3.0-1 - 1.3.0-6 and following changes:*

#### BUGFIXING
  * Method `plot.difNLR()` now correctly uses matching criterion when item purification
    is applied.
    
#### DOCUMENTATION
  * Documentation of the package was updated.
  * Some typos were fixed.

#### MINOR UPDATES
  * NEWS file is now generated using `markdown`.
  * README file was updated.
  
------

### Changes in version 1.3.0-6 (2020-01-22)
#### BUGFIXING
  * Mismatch in null and alternative models was fixed for all functions.
  * `MLR()` function now returns correct value of log-likelihood for
    alternative model.

#### MAJOR UPDATES
  * Default option of argument type in `NLR()` function was set
    to `"all"` instead of `"both"`.
  * Input `Data` in `difNLR()` function can be also a vector now.

------

### Changes in version 1.3.0-5 (2020-01-20)
#### BUGFIXING
  * Function `MLR()` was fixed for binary data and IRT parametrization.
  * Typo fixed in `print.difORD()` method.
  * Method `plot.ddfMLR()` was fixed for binary data.

------

### Changes in version 1.3.0-4 (2020-01-17)
#### MAJOR UPDATES
  * Function `ddfORD()` was renamed to `difORD()`.

#### MINOR UPDATES
  * Function `genNLR()` with an option `itemtype = "nominal"` returns
    nominal items as factors with levels presented by capital letters.
  * Legend for `plot.ddfMLR()` was updated to show P(Y = option) instead
    of option alone.
  * README file updated.
  * Typos fixed.

------

### Changes in version 1.3.0-3 (2020-01-07)
#### DOCUMENTATION
  * Documentation was updated.
  * Authors' details were updated.

#### MAJOR UPDATES
  * Seed was added for re-calculation of bootstrapped initial values
    in `NLR()` estimation.
    
#### MINOR UPDATES
  * Argument `item` for S3 methods of `difNLR` class can be now
    name of the column in `Data`.
  * Legends in `plot.ddfMLR()` and `plot.ddfORD()` were updated.
  * Some typos were fixed.

------

### Changes in version 1.3.0-2 (2020-01-03)
#### MAJOR UPDATES
  * Default option of argument type in `difNLR()` function was set
    to `"all"` instead of `"both"`.

#### MINOR UPDATES
  * Package `styler` was used to improve formatting of the code.
  * Package `ShinyItemAnalysis` was added into Suggests.
  * Figures for README were updated.

#### DOCUMENTATION
  * Documentation for all data was updated. Format of data was fixed.
  * Documentation of `estimNLR()` was improved.
  
------

### Changes in version 1.3.0-1 (2019-12-16)
#### BUGFIXING
  * Legend in `plot.ddfORD()` is now correctly displayed.

------

### Changes in version 1.3.0 (2019-08-19)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.3 - 1.2.8-4 and following changes:*

#### MINOR UPDATES
  * Some typos fixed in `print.difNLR()`
  * Authors' details were updated.
  * CITATION file was updated.

------

### Changes in version 1.2.8-4 (2019-08-09)
#### MINOR UPDATES
  * Typos fixed in `print.ddfORD()` and print.ddfMLR().
  * Matching criterion for `plot.ddfORD()` uses anchor items.

------

### Changes in version 1.2.8-3 (2019-08-08)
#### BUGFIXING
  * `plot.ddfORD()` now works when Data is factor.

#### MINOR UPDATES
  * `genNLR()` now generates ordinal data using adjacent category
    logit model with argument `itemtype = "ordinal"`.

------

### Changes in version 1.2.8-2 (2019-08-07)
#### BUGFIXING
  * `plot.ddfORD()` now works when items have different scales.
  * Argument `anchor` is now used for calculation of matching
    criterion in function `ORD()`.
  * IRT parametrization was fixed for `ddfORD()`.
  * `logLik.ddfMLR()` now works properly.
  * anchor items are now used for calculation of matching
    criterion in `plot.ddfORD()` and `plot.ddfMLR()`.

#### MAJOR UPDATES
  * Names of reference and focal group in `plot.difNLR()` can be
    changed with `group.name` argument.

#### MINOR UPDATES
  * Help pages for `difNLR()`, `ddfMLR()`, `ddfORD()`, `MLR()`, and `ORD()`
    functions were updated.

------

### Changes in version 1.2.8-1 (2019-08-06)
#### MINOR UPDATES
  * IRT parametrization now available in `ddfMLR()` function with
    argument `parametrization`. SE calculated with delta method.
  * Names of reference and focal group in `plot.ddfMLR()` can be
    changed with `group.name` argument.

### Changes in version 1.2.8 (2019-08-05)
#### MAJOR UPDATES
  * `ddfORD()` function was renamed. Now `ddfORD()`.
  * IRT parametrization now available in `ddfORD()` function with
    argument `parametrization`. SE calculated with delta method.
  * Names of reference and focal group in `plot.ddfORD()` can be
    changed with `group.name` argument.

#### MINOR UPDATES
  * Help page for `ddfORD()` was updated.
  * Reference for `ddfORD()` was added.

------

### Changes in version 1.2.7 (2019-07-04)
#### BUGFIXING
  * Check for input `item` in S3 methods for `difNLR()`, `ddfMLR()`, and `ddfORD()`
    was fixed.

#### MAJOR UDPATES
  * S3 methods `plot()` outputs for `difNLR()`, `ddfMLR()`, and `ddfORD()` functions 
  were unified.

#### MINOR UPDATES
  * Help pages were updated.
  * README file was updated.

------

### Changes in version 1.2.6 (2019-07-03)
#### MAJOR UDPATES
  * S3 method `plot()` for `ddfORD()` was implemented.

------

### Changes in version 1.2.5 (2019-07-02)
#### MAJOR UPDATES
  * S3 methods `AIC()`, `BIC()`, `logLik()`, `coef()` for `ddfORD()` were implemented.
  * S3 methods `AIC()`, `BIC()`, `logLik()`, `residuals()` for `difNLR()` and `ddfMLR()`
    objects now handle column names as `item` argument.
  * S3 method `coef()` for `difNLR` and `ddfMLR` objects were updated. Their now
    includes arguments `SE` (logical) to print standard errors and `simplify`
    (logical) whether list of estimates should be simplified into a matrix.

#### MINOR UPDATES
  * CITATION was updated.
  * All static DOI links were updated.

------

### Changes in version 1.2.4 (2019-07-02)
#### MAJOR UPDATES
  * New functions `ddfORD()` and `ORD()` for DDF detection for ordinal data
    with adjacent and cumulative logistic regression models were added.
    Output is displayed via S3 method `print.ddfORD()`

#### MINOR UPDATES
  * Authors' details were updated.
  * Some typos were fixed.
  * Helps for `ddfMLR()`, `MLR()`, and `difNLR()` were updated.

------

### Changes in version 1.2.3 (2019-06-20)
#### BUGFIXING
  * `plot.ddfMLR()` now handles also binary data.
  * `ddfMLR()` returns consistently `"No DDF item detected"` when no DDF
    item was detected.

#### MAJOR UPDATES
  * Matching criterion for `plot.ddfMLR()` was improved for displaying
    more smooth curves.

#### MINOR UPDATES
  * Authors' details were updated.

------

### Changes in version 1.2.2 (2018-05-03)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.1-1 - 1.2.1-3*

------

### Changes in version 1.2.1-3 (2018-04-26)
#### MINOR UPDATES
  * S3 methods `AIC()`, `BIC()`, `logLik()` of `ddfMLR()` are now item specific.

------

### Changes in version 1.2.1-2 (2018-03-19)
#### BUGFIXING
  * difNLR
    - Check for constraints was fixed.
  * NLR
    - initboot = F now works properly.

------

### Changes in version 1.2.1-1 (2018-03-16)
#### BUGFIXING
  * difNLR:
    - P-value adjustment is now performed in the last iteration of purification
      as described.
    - In difPur output columns are properly named.
  * ddfMLR:
    - P-value adjustment is now performed in the last iteration of purification
      as described.
    - In difPur output columns are properly named.

#### MINOR UPDATES
  * Warning messages do not include the call.

------

### Changes in version 1.2.1 (2018-03-01)

**_THIS IS A CRAN VERSION_**

*It includes versions 1.2.0-1 - 1.2.0-7*

------

### Changes in version 1.2.0-7 (2018-02-27)
#### MAJOR UPDATES
  * Argument `start` in `difNLR()` function is now item-specific. The input is
    correctly checked.
  * In case that some items do not converge, starting values are recalculated
    from bootstraped sample and problematic models are fitted again. This is done
    20 times at most.
    The options were added into `difNLR()` and `NLR()` functions.
  * Argument `constraints` in `difNLR()` function is now item-specific.

#### MINOR UPDATES
  * Minor #### BUGFIXING:
    - Typos were fixed in `print()` method for `difNLR` class.
  * Title was shorten.
  * Description of package was updated.
  * Description file was updated, reference was added.
  * README file was updated.
  * CITATION file was updated.

------

### Changes in version 1.2.0-6 (2018-02-26)
#### MAJOR UPDATES
  * S3 methods for `difNLR` class are now properly describedm, especially,
    `plot.difNLR()` and `predict.difNLR()`.
  * `difNLR()` documentation was improved.

#### BUGFIXING:
    - S3 methods for class `difNLR` can now properly handle items with
      convergence issues.
    - `NLR()` now detects DIF correctly with F test.

#### MINOR UPDATES
  * Typos were fixed.

------

### Changes in version 1.2.0-5 (2018-02-20)
#### MAJOR UPDATES
  * `print()`, `plot()`,`fitted()`, `predict()`, `logLik()`, `AIC()`, `BIC()`
    and `residuals()` for `difNLR` class now handles item specific arguments
    (`model`, `type` and `constraints`).
  * `residuals` for `difNLR` class now uses argument `item`.

#### BUGFIXING
  * Checking inputs in `difNLR` was fixed and improved.
  * Fixing degrees of freedom and p-values calculations in `NLR()`.
  * Fixing parameters, SE and covariances calculations in `NLR()`.
  * S3 methods for `difNLR` class can now handle convergence issues.

#### MINOR UPDATES
  * Documentation of `difNLR-package` was updated.
  * Syntax in `plot()` and `residuals()` for `difNLR` was slightly improved.
  * `logLik()` for `difNLR` now returns list of `logLik` class values.

------

### Changes in version 1.2.0-4 (2018-02-19)
#### MAJOR UPDATES
  * Function `startNLR()` now handles item-specific arguments (`model` and
    `parameterization`). Its output is now in the form of list. It can be
    simplified with argument `simplify` into table when all parameterizations
    are the same.
  * Function `NLR()` now handles item-specific arguments (`model`, `type` and
    `constraints`).
  * Function `difNLR()` now handles item-specific arguments (`model`, `type`
    and `constraints`).
    
#### MINOR UPDATES
  * README file was updated.

------

### Changes in version 1.2.0-3 (2018-02-19)
#### BUGFIXING
  * Starting values in input of `estimNLR()` in `NLR()` are now properly named.
  * Bug in alternative parameterization for testing differences in parameters c and d in function `formulaNLR()` was fixed.
      
#### MINOR UPDATES
  * Descriptions of `formulaNLR()` and `estimNLR()` were improved.

------

### Changes in version 1.2.0-2 (2018-02-15)
#### MAJOR UPDATES
  * Function `genNLR()` can now also generate nominal data based on
    model specified in `ddfMLR()`.
  * Argument `parameters` in `genNLR()` is no longer applicable.
  * Arguments `a`, `b`, `c`, `d` were added into `genNLR()` as parameters -
    discrimination, difficulty, guessing, inattention
  * Function `genNLR()` can now also generate different underlying
    distributions for reference and focal group with arguments `mu` and `sigma`.
    
#### MINOR UPDATES
  * Email address of maintainer was changed.

------

### Changes in version 1.2.0-1 (2018-02-14)
#### MAJOR UPDATES
  * New function `estimNLR()` to estimate parameters of NLR models
    was added. This function uses non-linear least squares or maximum
    likelihood method.
  * Function `NLR()` now uses `estimNLR()` for estimation of models
    parameters.
  * Function `difNLR()` can now estimate models parameters with also maximum
    likelihood method.
  * Iteratively reweighted least squares (IRLS) method was added into `estimNLR()`
    function. This option is not fully functional.

#### BUGFIXING
  * Bug in `plot()` for `ddfMLR` class in matching criterion was fixed.
  * Bug in `NLR()` was fixed. User-specified starting values are now available.
  * Bug in `startNLR()` was fixed. Function runs even if there are not unique cuts for total scores/match.
  * Bug in log-likelihood calculation in `estimNLR()` was fixed.
    
#### MINOR UPDATES
  * Some preparation for new estimation methods for F test in `NLR()` was done.
  * Convergence failure warning is now item specific.
  * Warning and error messages were improved.

------

### Changes in version 1.1.3 (2018-02-06)
#### BUGFIXING
  * Bug in delta method in `NLR()` function was fixed.
  * Bug in `match` argument in `difNLR()` function was fixed.
  * Bug in one dimensional `Data` in `difNLR()` function was fixed.
    
#### MAJOR UPDATES
  * Specification of upper and lower asymptotes in
    `startNLR()` function was improved.
  * Functions `ddfMLR()` and `MLR()` can now handle also total score
    or other user-specified matching criterion.
  * S3 functions `plot()` for class `ddfMLR` can also handle total score
    or other user-specified matching criterion.

#### MINOR UPDATES
  * New auxiliary function `checkInterval()` was added.
  * Size of labs and title was unified in graphical outputs of functions
    `difNLR()` and `ddfMLR()`.

------

### Changes in version 1.1.2 (2017-12-12)
#### MAJOR UPDATES
  * CITATION file was added with reference to relevant paper.
  * Bug when loading group by group identificator was fixed.
  * Condition to check dimension of complete cases data was added.
  * Function `residuals.difNLR()` was added.
  * S3 functions `AIC()` and `BIC()` for `difNLR` class were
    updated.
  * S3 functions `plot()`, `fitted()` and `predict()` for `difNLR` class
    can now handle also other matching criterions than `zscore`.

#### MINOR UPDATES
  * Reference lists were updated.
  * README file was updated.

------

### Changes in version 1.1.1 (2017-08-28)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  * Bug in `startNLR()` function for missing values was fixed.

#### MINOR UPDATES
  * Graphical representation for `difNLR()` and `ddfMLR()` functions was
    mildly updated and unified.

------

### Changes in version 1.1.0 (2017-08-21)

**_THIS IS A CRAN VERSION_**

#### MINOR UPDATES
  * Bug in `plot.difNLR()` was fixed.
  * README file was updated.
  * Package documentation was updated.
  * Default value for `constraints` arguments in `NLR()` and `formulaNLR()`
    functions were set to `NULL`.
  * Default starting values were added into `NLR()` function
    by `startNLR()` function.

------

### Changes in version 1.0.8 (2017-08-14)
#### BUGFIXING
  * Several bugs were fixed:
    - `difNLR()` function can handle `Data` with one column.
    - `startNLR()` now works when `match` argument is set.
    - Check input condition was fixed in `formulaNLR()` function.
    - Delta method in `NLR()` function.
    
#### MINOR UPDATES
  * Function `startNLR()` was mildly updated.

------

### Changes in version 1.0.7 (2017-08-10)
#### MAJOR UPDATES
  * Item purification was implemented into `ddfMLR()` function.
  * Anchor items were implemented into `ddfMLR()` function.
  * Anchor items were implemented into `MLR()` function.

#### MINOR UPDATES
  * Minor bug in `logLik.ddfMLR()` function was fixed.
  * Documentation of `difNLR()` was updated.
  
------

### Changes in version 1.0.6 (2017-08-09)
#### MAJOR UPDATES
  * Item purification was implemented into `difNLR()` function.
  * Anchor items were implemented into `difNLR()` function.
  * Anchor items were implemented into `NLR()` function.
  
#### MINOR UPDATES
  * README file was updated.

------

### Changes in version 1.0.5 (2017-08-09)
#### MAJOR UPDATES
  * Datasets `difMedical`, `difMedicaltest`, and `difMedicalkey`
    were renamed. Now they are `MSATB`, `MSATBtest`, and `MSATBkey`.
    from Medical School Admission Test in Biology.
    
#### MINOR UPDATES
  * LazyData is now available.
  * References were updated.
  * README file updated.

------

### Changes in version 1.0.4 (2017-08-08)
#### MAJOR UPDATES
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

#### MINOR UPDATES
  * References were updated.
  * Some minor bugs were fixed:
      - Items are no longer renamed by `difNLR()` and `ddfMLR()` functions.
      - Starting values are now correctly checked in `difNLR()` function.
  * `msm` package is now used for delta method in `difNLR()` function.

------

### Changes in version 1.0.3 (2017-06-15)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  * Bug of `plot.ddfMLR()` for non-uniform DDF was fixed.
  * References were updated.

------

### Changes in version 1.0.2 (2017-06-06)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  * Bug of dimensions for parameter estimates of `difNLR()` function was fixed.
  * Datasets `GMAT` and `GMATtest` were extended by `criterion` variable which is intended to be predicted by test.
  * `coef`, `logLik`, `AIC` and `BIC` S3 methods were added for class `ddfMLR`.

#### MINOR UPDATES
  * Functions `plot.ddfMLR()` and `plot.difNLR()` were slightly improved.
  * Updated error and warning handling in `difNLR()` and `ddfMLR()` functions.
  * Description file was updated.
  * Item names are now the same as in original data set.
  * README file updated.

------

### Changes in version 1.0.0 (2017-01-10)

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
  * New function ddfMLR() to detect Differential Distractor Functioning (DDF) with Multinomial Log-linear Regression (MLR) model. S3 methods for class `ddfMLR` also added - `print` and `plot`.
  * New function MLR() to calculate likelihood ratio statistic
     for detecting DDF with MLR model.
  * The `difNLR()` function can handle 6 generalized logistic
     regression models with option `model`.
  * Functions `startNLR()`, `genNLR()` ans S3 methods for class
     `difNLR` were changed according `difNLR()` function. S3 method
     `coef` was created.
  * New functions NLR() and constrNLR() can now calculates DIF
     detection statistics and specify constraints for generalized
     logistic regression model.
  * Function `difNLR()` was edited to response to `difR` package
     and its DIF detection functions.
  * Function `genNLR()` was changed to generate dataset from
     generalized logistic regression model with 8 parameters.

#### MINOR UPDATES
  * The CITATION file was updated.
  * Several typos were fixed.
  * Some default options of input were changed.
  * `AIC()`, `BIC()`, and `logLik()` S3 methods added to `difNLR()`.
  
------

### Changes in version 0.2.0 (2016-11-09) 

**_THIS IS A CRAN VERSION_**

#### MAJOR UPDATES
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

#### MINOR UPDATES
  *  Several typos were fixed.
