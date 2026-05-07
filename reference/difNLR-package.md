# DIF and DDF Detection by Non-Linear Regression Models.

The difNLR package provides methods for detecting differential item
functioning (DIF) using non-linear regression models. Both uniform and
non-uniform DIF effects can be detected when considering a single focal
group. Additionally, the method allows for testing differences in
guessing or inattention parameters between the reference and focal
group. DIF detection is performed using either a likelihood-ratio test,
an F-test, or Wald's test of a submodel. The software offers a variety
of algorithms for estimating item parameters.

Furthermore, the difNLR package includes methods for detecting
differential distractor functioning (DDF) using multinomial log-linear
regression model. It also introduces DIF detection approaches for
ordinal data via adjacent category logit and cumulative logit regression
models.

## Details

Package: difNLR  
Type: Package  
Version: 1.5.3-1  
Date: 2026-05-07  
Depends: R (\>= 4.0.0)  
Imports: calculus, ggplot2 (\>= 3.4.0), msm, nnet, plyr, stats, VGAM  
Suggests: knitr, pkgdown, rmarkdown, ShinyItemAnalysis, testthat (\>=
3.0.0), vdiffr  
VignetteBuilder: knitr License: GPL-3  
BugReports: <https://github.com/adelahladka/difNLR/issues>  
Encoding: UTF-8  
Url: <https://adelahladka.github.io/difNLR/>

## Note

This package was supported by grant funded by Czech Science foundation
under number GJ15-15856Y.

## Functions

- [`ddfMLR`](https://adelahladka.github.io/difNLR/reference/ddfMLR.md)

- [`difNLR`](https://adelahladka.github.io/difNLR/reference/difNLR.md)

- [`difORD`](https://adelahladka.github.io/difNLR/reference/difORD.md)

- [`estimNLR`](https://adelahladka.github.io/difNLR/reference/estimNLR.md)

- [`formulaNLR`](https://adelahladka.github.io/difNLR/reference/formulaNLR.md)

- [`MLR`](https://adelahladka.github.io/difNLR/reference/MLR.md)

- [`NLR`](https://adelahladka.github.io/difNLR/reference/NLR.md)

- [`ORD`](https://adelahladka.github.io/difNLR/reference/ORD.md)

- [`startNLR`](https://adelahladka.github.io/difNLR/reference/startNLR.md)

## Datasets

- [`GMAT`](https://adelahladka.github.io/difNLR/reference/GMAT.md)

- [`GMAT2`](https://adelahladka.github.io/difNLR/reference/GMAT2.md)

- [`MSATB`](https://adelahladka.github.io/difNLR/reference/MSATB.md)

## References

Agresti, A. (2010). Analysis of ordinal categorical data. Second
edition. John Wiley & Sons.

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

Kingston, N., Leary, L., & Wightman, L. (1985). An exploratory study of
the applicability of item response theory methods to the Graduate
Management Admission Test. ETS Research Report Series, 1985(2): 1–64.

Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland,
J. L., & Price, R. M. (2017). Checking equity: Why differential item
functioning analysis should be a routine part of developing conceptual
assessments. CBE–Life Sciences Education, 16(2), rm2,
[doi:10.1187/cbe.16-10-0307](https://doi.org/10.1187/cbe.16-10-0307) .

Swaminathan, H. & Rogers, H. J. (1990). Detecting differential item
functioning using logistic regression procedures. Journal of Educational
Measurement, 27(4), 361–370,
[doi:10.1111/j.1745-3984.1990.tb00754.x](https://doi.org/10.1111/j.1745-3984.1990.tb00754.x)

Vlckova, K. (2014). Test and item fairness. Master's thesis. Faculty of
Mathematics and Physics, Charles University.

## See also

Useful links:

- <https://adelahladka.github.io/difNLR/>

- Report bugs at <https://github.com/adelahladka/difNLR/issues>

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  
