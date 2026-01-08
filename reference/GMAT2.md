# Dichotomous dataset based on GMAT.

The `GMAT2` is a generated dataset based on parameters from Graduate
Management Admission Test (GMAT, Kingston et al., 1985). First two items
were considered to function differently in uniform and non-uniform way
respectively. The dataset represents responses of 1,000 subjects to
multiple-choice test of 20 items. A correct answer is coded as 1 and
incorrect answer as 0. The column `group` represents group membership,
where 0 indicates reference group and 1 indicates focal group. Groups
are the same size (i.e. 500 per group).

## Usage

``` r
data(GMAT2)
```

## Format

A `GMAT2` data frame consists of 1,000 observations on the following 21
variables:

- Item1-Item20:

  dichotomously scored items of the test

- group:

  group membership vector, `"0"` reference group, `"1"` focal group

## References

Kingston, N., Leary, L., & Wightman, L. (1985). An exploratory study of
the applicability of item response theory methods to the Graduate
Management Admission Test. ETS Research Report Series, 1985(2): 1–64.

Martinkova, P., Drabinova, A., Liaw, Y. L., Sanders, E. A., McFarland,
J. L., & Price, R. M. (2017). Checking equity: Why differential item
functioning analysis should be a routine part of developing conceptual
assessments. CBE–Life Sciences Education, 16(2), rm2,
[doi:10.1187/cbe.16-10-0307](https://doi.org/10.1187/cbe.16-10-0307) .

## See also

[`GMAT2test`](https://adelahladka.github.io/difNLR/reference/GMAT2test.md),
[`GMAT2key`](https://adelahladka.github.io/difNLR/reference/GMAT2key.md)

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
Faculty of Mathematics and Physics, Charles University  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  
