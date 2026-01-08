# Calculates starting values for non-linear regression DIF models.

Calculates starting values for the
[`difNLR()`](https://adelahladka.github.io/difNLR/reference/difNLR.md)
function based on linear approximation.

## Usage

``` r
startNLR(Data, group, model, constraints = NULL, match = "zscore",
         parameterization = "irt", simplify = FALSE)
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

  character: generalized logistic regression model for which starting
  values should be estimated. See **Details**.

- constraints:

  character: which parameters should be the same for both groups.
  Possible values are any combinations of parameters `"a"`, `"b"`,
  `"c"`, and `"d"`. Default value is `NULL`.

- match:

  character or numeric: matching criterion to be used as an estimate of
  the trait. It can be either `"zscore"` (default, standardized total
  score), `"score"` (total test score), or a numeric vector of the same
  length as a number of observations in the `Data`.

- parameterization:

  character: parameterization of regression coefficients. Possible
  options are `"irt"` (IRT parameterization, default), `"is"`
  (intercept-slope), and `"logistic"` (logistic regression as in the
  [`glm`](https://rdrr.io/r/stats/glm.html) function, available for the
  `"2PL"` model only). See **Details**.

- simplify:

  logical: should initial values be simplified into the matrix? It is
  only applicable when parameterization is the same for all items.

## Value

A list containing elements representing items. Each element is a named
numeric vector with initial values for the chosen generalized logistic
regression model.

## Details

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

Three possible parameterizations can be specified in the
`"parameterization"` argument: `"irt"` returns the IRT parameters of the
reference group and differences in these parameters between the
reference and focal group. Parameters of asymptotes are printed
separately for the reference and focal groups. `"is"` returns
intercept-slope parameterization. Parameters of asymptotes are again
printed separately for the reference and focal groups. `"logistic"`
returns parameters in logistic regression parameterization as in the
[`glm`](https://rdrr.io/r/stats/glm.html) function, and it is available
only for the 2PL model.

## References

Drabinova, A. & Martinkova, P. (2017). Detection of differential item
functioning with nonlinear regression: A non-IRT approach accounting for
guessing. Journal of Educational Measurement, 54(4), 498–517,
[doi:10.1111/jedm.12158](https://doi.org/10.1111/jedm.12158) .

Hladka, A. & Martinkova, P. (2020). difNLR: Generalized logistic
regression models for DIF and DDF detection. The R Journal, 12(1),
300–323,
[doi:10.32614/RJ-2020-014](https://doi.org/10.32614/RJ-2020-014) .

Hladka, A. (2021). Statistical models for detection of differential item
functioning. Dissertation thesis. Faculty of Mathematics and Physics,
Charles University.

## See also

[`difNLR`](https://adelahladka.github.io/difNLR/reference/difNLR.md)

## Author

Adela Hladka (nee Drabinova)  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  

## Examples

``` r
# loading data
data(GMAT)
Data <- GMAT[, 1:20] # items
group <- GMAT[, "group"] # group membership variable

# 3PL model with the same guessing for both groups
startNLR(Data, group, model = "3PLcg")
#> [[1]]
#>          a          b c        aDif      bDif
#> 1 0.803528 -0.5545696 0 -0.04546579 0.7790496
#> 
#> [[2]]
#>           a          b c    aDif      bDif
#> 2 0.6070957 -0.7381788 0 0.31033 0.7303649
#> 
#> [[3]]
#>           a          b         c         aDif       bDif
#> 3 0.6146679 -0.6717698 0.2105793 -0.003272895 -0.1338688
#> 
#> [[4]]
#>           a         b         c       aDif       bDif
#> 4 0.6206673 -0.639168 0.3786497 0.01282108 -0.3413823
#> 
#> [[5]]
#>           a         b         c        aDif       bDif
#> 5 0.7330905 -1.150193 0.3007038 -0.03438126 -0.1170534
#> 
#> [[6]]
#>           a          b c       aDif       bDif
#> 6 0.6841399 -0.7566681 0 0.01923415 0.05428855
#> 
#> [[7]]
#>           a          b          c       aDif       bDif
#> 7 0.6918365 -0.4661765 0.08011013 -0.1486659 -0.6729622
#> 
#> [[8]]
#>           a          b          c     aDif        bDif
#> 8 0.5111858 -0.7294573 0.02894087 0.198776 0.006989234
#> 
#> [[9]]
#>           a          b          c        aDif        bDif
#> 9 0.6083599 -0.4645629 0.01517289 -0.07240124 -0.08734126
#> 
#> [[10]]
#>            a          b          c        aDif       bDif
#> 10 0.5769053 -0.2063425 0.04501444 -0.05441014 -0.3289015
#> 
#> [[11]]
#>            a         b          c        aDif      bDif
#> 11 0.6646585 -1.007692 0.01199162 -0.01910439 0.1210262
#> 
#> [[12]]
#>            a          b c       aDif      bDif
#> 12 0.5908041 -0.4892599 0 0.08303696 0.2559915
#> 
#> [[13]]
#>            a          b         c       aDif       bDif
#> 13 0.6300507 -0.9142994 0.2650514 0.01723558 0.08843438
#> 
#> [[14]]
#>            a         b c        aDif       bDif
#> 14 0.7505623 0.3536455 0 -0.08479531 -0.1338582
#> 
#> [[15]]
#>            a           b c       aDif       bDif
#> 15 0.5639624 -0.09464822 0 0.01804995 -0.2923393
#> 
#> [[16]]
#>            a         b c        aDif      bDif
#> 16 0.8258482 0.2526813 0 -0.02662637 -0.107213
#> 
#> [[17]]
#>           a         b c       aDif       bDif
#> 17 0.512145 0.4708969 0 0.01194957 -0.4098997
#> 
#> [[18]]
#>            a        b c        aDif        bDif
#> 18 0.8310156 0.402433 0 -0.04855186 0.005960168
#> 
#> [[19]]
#>          a          b c       aDif        bDif
#> 19 0.84519 0.08076388 0 -0.2252835 -0.03191637
#> 
#> [[20]]
#>            a         b c       aDif      bDif
#> 20 0.6197988 0.3135336 0 0.07998677 0.1259236
#> 
startNLR(Data, group, model = "3PLcg", parameterization = "is")
#> [[1]]
#>         b1        b0 c          b3       b2
#> 1 0.803528 0.4456122 0 -0.04546579 0.615782
#> 
#> [[2]]
#>          b1        b0 c      b3        b2
#> 2 0.6070957 0.4481452 0 0.31033 0.4409765
#> 
#> [[3]]
#>          b1        b0         c           b3          b2
#> 3 0.6146679 0.4129153 0.2105793 -0.003272895 -0.07964808
#> 
#> [[4]]
#>          b1        b0         c         b3         b2
#> 4 0.6206673 0.3967107 0.3786497 0.01282108 -0.2244566
#> 
#> [[5]]
#>          b1        b0         c          b3          b2
#> 5 0.7330905 0.8431955 0.3007038 -0.03438126 -0.04224121
#> 
#> [[6]]
#>          b1        b0 c         b3         b2
#> 6 0.6841399 0.5176669 0 0.01923415 0.02363129
#> 
#> [[7]]
#>          b1        b0          c         b3         b2
#> 7 0.6918365 0.3225179 0.08011013 -0.1486659 -0.2962288
#> 
#> [[8]]
#>          b1        b0          c       b3         b2
#> 8 0.5111858 0.3728882 0.02894087 0.198776 -0.1400365
#> 
#> [[9]]
#>          b1        b0          c          b3          b2
#> 9 0.6083599 0.2826214 0.01517289 -0.07240124 -0.01317637
#> 
#> [[10]]
#>           b1        b0          c          b3         b2
#> 10 0.5769053 0.1190401 0.04501444 -0.05441014 -0.1606223
#> 
#> [[11]]
#>           b1        b0          c          b3         b2
#> 11 0.6646585 0.6697711 0.01199162 -0.01910439 0.09738032
#> 
#> [[12]]
#>           b1        b0 c         b3        b2
#> 12 0.5908041 0.2890568 0 0.08303696 0.1318709
#> 
#> [[13]]
#>           b1       b0         c         b3         b2
#> 13 0.6300507 0.576055 0.2650514 0.01723558 0.04148388
#> 
#> [[14]]
#>           b1        b0 c          b3         b2
#> 14 0.7505623 -0.265433 0 -0.08479531 -0.1191059
#> 
#> [[15]]
#>           b1         b0 c         b3         b2
#> 15 0.5639624 0.05337804 0 0.01804995 -0.1718535
#> 
#> [[16]]
#>           b1         b0 c          b3          b2
#> 16 0.8258482 -0.2086764 0 -0.02662637 -0.09241496
#> 
#> [[17]]
#>          b1         b0 c         b3         b2
#> 17 0.512145 -0.2411675 0 0.01194957 -0.2091992
#> 
#> [[18]]
#>           b1         b0 c          b3          b2
#> 18 0.8310156 -0.3344281 0 -0.04855186 -0.01487526
#> 
#> [[19]]
#>         b1          b0 c         b3          b2
#> 19 0.84519 -0.06826083 0 -0.2252835 -0.03797994
#> 
#> [[20]]
#>           b1         b0 c         b3       b2
#> 20 0.6197988 -0.1943277 0 0.07998677 0.113198
#> 
# simplified into a single table
startNLR(Data, group, model = "3PLcg", simplify = TRUE)
#>            a           b          c         aDif         bDif
#> 1  0.8035280 -0.55456959 0.00000000 -0.045465788  0.779049635
#> 2  0.6070957 -0.73817885 0.00000000  0.310330020  0.730364950
#> 3  0.6146679 -0.67176982 0.21057931 -0.003272895 -0.133868800
#> 4  0.6206673 -0.63916801 0.37864972  0.012821079 -0.341382325
#> 5  0.7330905 -1.15019290 0.30070384 -0.034381258 -0.117053386
#> 6  0.6841399 -0.75666812 0.00000000  0.019234146  0.054288549
#> 7  0.6918365 -0.46617649 0.08011013 -0.148665851 -0.672962198
#> 8  0.5111858 -0.72945727 0.02894087  0.198776015  0.006989234
#> 9  0.6083599 -0.46456293 0.01517289 -0.072401236 -0.087341259
#> 10 0.5769053 -0.20634246 0.04501444 -0.054410143 -0.328901482
#> 11 0.6646585 -1.00769206 0.01199162 -0.019104392  0.121026227
#> 12 0.5908041 -0.48925994 0.00000000  0.083036964  0.255991496
#> 13 0.6300507 -0.91429945 0.26505136  0.017235577  0.088434376
#> 14 0.7505623  0.35364553 0.00000000 -0.084795310 -0.133858213
#> 15 0.5639624 -0.09464822 0.00000000  0.018049954 -0.292339298
#> 16 0.8258482  0.25268128 0.00000000 -0.026626373 -0.107213007
#> 17 0.5121450  0.47089694 0.00000000  0.011949572 -0.409899740
#> 18 0.8310156  0.40243297 0.00000000 -0.048551864  0.005960168
#> 19 0.8451900  0.08076388 0.00000000 -0.225283518 -0.031916372
#> 20 0.6197988  0.31353356 0.00000000  0.079986773  0.125923571
startNLR(Data, group, model = "3PLcg", parameterization = "is", simplify = TRUE)
#>           b1          b0          c           b3          b2
#> 1  0.8035280  0.44561218 0.00000000 -0.045465788 -0.61578200
#> 2  0.6070957  0.44814518 0.00000000  0.310330020 -0.44097650
#> 3  0.6146679  0.41291535 0.21057931 -0.003272895  0.07964808
#> 4  0.6206673  0.39671068 0.37864972  0.012821079  0.22445656
#> 5  0.7330905  0.84319553 0.30070384 -0.034381258  0.04224121
#> 6  0.6841399  0.51766685 0.00000000  0.019234146 -0.02363129
#> 7  0.6918365  0.32251791 0.08011013 -0.148665851  0.29622878
#> 8  0.5111858  0.37288822 0.02894087  0.198776015  0.14003652
#> 9  0.6083599  0.28262144 0.01517289 -0.072401236  0.01317637
#> 10 0.5769053  0.11904007 0.04501444 -0.054410143  0.16062232
#> 11 0.6646585  0.66977110 0.01199162 -0.019104392 -0.09738032
#> 12 0.5908041  0.28905676 0.00000000  0.083036964 -0.13187091
#> 13 0.6300507  0.57605497 0.26505136  0.017235577 -0.04148388
#> 14 0.7505623 -0.26543300 0.00000000 -0.084795310  0.11910586
#> 15 0.5639624  0.05337804 0.00000000  0.018049954  0.17185347
#> 16 0.8258482 -0.20867637 0.00000000 -0.026626373  0.09241496
#> 17 0.5121450 -0.24116752 0.00000000  0.011949572  0.20919922
#> 18 0.8310156 -0.33442806 0.00000000 -0.048551864  0.01487526
#> 19 0.8451900 -0.06826083 0.00000000 -0.225283518  0.03797994
#> 20 0.6197988 -0.19432773 0.00000000  0.079986773 -0.11319804

# 2PL model
startNLR(Data, group, model = "2PL")
#> [[1]]
#>          a          b        aDif      bDif
#> 1 0.803528 -0.5545696 -0.04546579 0.7790496
#> 
#> [[2]]
#>           a          b    aDif      bDif
#> 2 0.6070957 -0.7381788 0.31033 0.7303649
#> 
#> [[3]]
#>           a         b         aDif       bDif
#> 3 0.4852316 -1.539724 -0.002583691 -0.1385151
#> 
#> [[4]]
#>           a         b        aDif       bDif
#> 4 0.3856518 -2.602855 0.007966381 -0.3016395
#> 
#> [[5]]
#>           a         b        aDif       bDif
#> 5 0.5126474 -2.323334 -0.02404268 -0.1747799
#> 
#> [[6]]
#>           a          b       aDif       bDif
#> 6 0.6841399 -0.7566681 0.01923415 0.05428855
#> 
#> [[7]]
#>           a          b       aDif       bDif
#> 7 0.6364134 -0.7179315 -0.1367562 -0.7418676
#> 
#> [[8]]
#>           a          b      aDif       bDif
#> 8 0.4963917 -0.8460623 0.1930233 0.03963645
#> 
#> [[9]]
#>           a          b       aDif        bDif
#> 9 0.5991293 -0.5152127 -0.0713027 -0.09418341
#> 
#> [[10]]
#>            a          b       aDif       bDif
#> 10 0.5509363 -0.3697532 -0.0519609 -0.3459183
#> 
#> [[11]]
#>            a         b       aDif      bDif
#> 11 0.6566882 -1.044214 -0.0188753 0.1199454
#> 
#> [[12]]
#>            a          b       aDif      bDif
#> 12 0.5908041 -0.4892599 0.08303696 0.2559915
#> 
#> [[13]]
#>            a         b       aDif      bDif
#> 13 0.4630549 -2.059094 0.01266726 0.1189173
#> 
#> [[14]]
#>            a         b        aDif       bDif
#> 14 0.7505623 0.3536455 -0.08479531 -0.1338582
#> 
#> [[15]]
#>            a           b       aDif       bDif
#> 15 0.5639624 -0.09464822 0.01804995 -0.2923393
#> 
#> [[16]]
#>            a         b        aDif      bDif
#> 16 0.8258482 0.2526813 -0.02662637 -0.107213
#> 
#> [[17]]
#>           a         b       aDif       bDif
#> 17 0.512145 0.4708969 0.01194957 -0.4098997
#> 
#> [[18]]
#>            a        b        aDif        bDif
#> 18 0.8310156 0.402433 -0.04855186 0.005960168
#> 
#> [[19]]
#>          a          b       aDif        bDif
#> 19 0.84519 0.08076388 -0.2252835 -0.03191637
#> 
#> [[20]]
#>            a         b       aDif      bDif
#> 20 0.6197988 0.3135336 0.07998677 0.1259236
#> 
startNLR(Data, group, model = "2PL", parameterization = "is")
#> [[1]]
#>         b1        b0          b3       b2
#> 1 0.803528 0.4456122 -0.04546579 0.615782
#> 
#> [[2]]
#>          b1        b0      b3        b2
#> 2 0.6070957 0.4481452 0.31033 0.4409765
#> 
#> [[3]]
#>          b1        b0           b3          b2
#> 3 0.4852316 0.7471225 -0.002583691 -0.06287585
#> 
#> [[4]]
#>          b1       b0          b3         b2
#> 4 0.3856518 1.003796 0.007966381 -0.1394661
#> 
#> [[5]]
#>          b1       b0          b3          b2
#> 5 0.5126474 1.191051 -0.02404268 -0.02953911
#> 
#> [[6]]
#>          b1        b0         b3         b2
#> 6 0.6841399 0.5176669 0.01923415 0.02363129
#> 
#> [[7]]
#>          b1        b0         b3         b2
#> 7 0.6364134 0.4569012 -0.1367562 -0.2724979
#> 
#> [[8]]
#>          b1        b0        b3         b2
#> 8 0.4963917 0.4199783 0.1930233 -0.1359837
#> 
#> [[9]]
#>          b1       b0         b3          b2
#> 9 0.5991293 0.308679 -0.0713027 -0.01297645
#> 
#> [[10]]
#>           b1        b0         b3        b2
#> 10 0.5509363 0.2037104 -0.0519609 -0.153392
#> 
#> [[11]]
#>           b1        b0         b3         b2
#> 11 0.6566882 0.6857227 -0.0188753 0.09621258
#> 
#> [[12]]
#>           b1        b0         b3        b2
#> 12 0.5908041 0.2890568 0.08303696 0.1318709
#> 
#> [[13]]
#>           b1        b0         b3         b2
#> 13 0.4630549 0.9534735 0.01266726 0.03048852
#> 
#> [[14]]
#>           b1        b0          b3         b2
#> 14 0.7505623 -0.265433 -0.08479531 -0.1191059
#> 
#> [[15]]
#>           b1         b0         b3         b2
#> 15 0.5639624 0.05337804 0.01804995 -0.1718535
#> 
#> [[16]]
#>           b1         b0          b3          b2
#> 16 0.8258482 -0.2086764 -0.02662637 -0.09241496
#> 
#> [[17]]
#>          b1         b0         b3         b2
#> 17 0.512145 -0.2411675 0.01194957 -0.2091992
#> 
#> [[18]]
#>           b1         b0          b3          b2
#> 18 0.8310156 -0.3344281 -0.04855186 -0.01487526
#> 
#> [[19]]
#>         b1          b0         b3          b2
#> 19 0.84519 -0.06826083 -0.2252835 -0.03797994
#> 
#> [[20]]
#>           b1         b0         b3       b2
#> 20 0.6197988 -0.1943277 0.07998677 0.113198
#> 
startNLR(Data, group, model = "2PL", parameterization = "logistic")
#> [[1]]
#>         b1        b0          b3       b2
#> 1 0.803528 0.4456122 -0.04546579 0.615782
#> 
#> [[2]]
#>          b1        b0      b3        b2
#> 2 0.6070957 0.4481452 0.31033 0.4409765
#> 
#> [[3]]
#>          b1        b0           b3          b2
#> 3 0.4852316 0.7471225 -0.002583691 -0.06287585
#> 
#> [[4]]
#>          b1       b0          b3         b2
#> 4 0.3856518 1.003796 0.007966381 -0.1394661
#> 
#> [[5]]
#>          b1       b0          b3          b2
#> 5 0.5126474 1.191051 -0.02404268 -0.02953911
#> 
#> [[6]]
#>          b1        b0         b3         b2
#> 6 0.6841399 0.5176669 0.01923415 0.02363129
#> 
#> [[7]]
#>          b1        b0         b3         b2
#> 7 0.6364134 0.4569012 -0.1367562 -0.2724979
#> 
#> [[8]]
#>          b1        b0        b3         b2
#> 8 0.4963917 0.4199783 0.1930233 -0.1359837
#> 
#> [[9]]
#>          b1       b0         b3          b2
#> 9 0.5991293 0.308679 -0.0713027 -0.01297645
#> 
#> [[10]]
#>           b1        b0         b3        b2
#> 10 0.5509363 0.2037104 -0.0519609 -0.153392
#> 
#> [[11]]
#>           b1        b0         b3         b2
#> 11 0.6566882 0.6857227 -0.0188753 0.09621258
#> 
#> [[12]]
#>           b1        b0         b3        b2
#> 12 0.5908041 0.2890568 0.08303696 0.1318709
#> 
#> [[13]]
#>           b1        b0         b3         b2
#> 13 0.4630549 0.9534735 0.01266726 0.03048852
#> 
#> [[14]]
#>           b1        b0          b3         b2
#> 14 0.7505623 -0.265433 -0.08479531 -0.1191059
#> 
#> [[15]]
#>           b1         b0         b3         b2
#> 15 0.5639624 0.05337804 0.01804995 -0.1718535
#> 
#> [[16]]
#>           b1         b0          b3          b2
#> 16 0.8258482 -0.2086764 -0.02662637 -0.09241496
#> 
#> [[17]]
#>          b1         b0         b3         b2
#> 17 0.512145 -0.2411675 0.01194957 -0.2091992
#> 
#> [[18]]
#>           b1         b0          b3          b2
#> 18 0.8310156 -0.3344281 -0.04855186 -0.01487526
#> 
#> [[19]]
#>         b1          b0         b3          b2
#> 19 0.84519 -0.06826083 -0.2252835 -0.03797994
#> 
#> [[20]]
#>           b1         b0         b3       b2
#> 20 0.6197988 -0.1943277 0.07998677 0.113198
#> 

# 4PL model with a total score as the matching criterion
startNLR(Data, group, model = "4PL", match = "score")
#> [[1]]
#>           a        b cR dR        aDif   bDif cF dF
#> 1 0.2587156 9.880598  0  1 -0.01463883 2.4196  0  1
#> 
#> [[2]]
#>           a        b          cR dR       aDif     bDif cF dF
#> 2 0.1964099 9.359335 0.004788726  1 0.09897782 2.219396  0  1
#> 
#> [[3]]
#>           a        b        cR dR        aDif       bDif       cF dF
#> 3 0.1956394 9.399439 0.2014277  1 0.003523097 -0.1808384 0.219731  1
#> 
#> [[4]]
#>           a        b        cR dR       aDif       bDif        cF dF
#> 4 0.1956052 9.401226 0.3652006  1 0.01287445 -0.6314144 0.3920988  1
#> 
#> [[5]]
#>           a        b        cR dR          aDif       bDif        cF dF
#> 5 0.2308482 7.840253 0.2849871  1 -0.0007091662 0.02669693 0.3164206  1
#> 
#> [[6]]
#>           a        b cR dR        aDif      bDif cF dF
#> 6 0.2202756 9.252913  0  1 0.006192905 0.1686113  0  1
#> 
#> [[7]]
#>           a        b cR dR         aDif      bDif        cF dF
#> 7 0.2049089 9.373223  0  1 -0.008101338 -0.034463 0.1825676  1
#> 
#> [[8]]
#>           a        b        cR dR       aDif     bDif cF dF
#> 8 0.1792728 10.33273 0.1084787  1 0.04270131 -1.23436  0  1
#> 
#> [[9]]
#>           a        b cR dR        aDif      bDif         cF dF
#> 9 0.1929044 10.00283  0  1 -0.01354956 0.3247955 0.05245522  1
#> 
#> [[10]]
#>            a        b cR dR          aDif       bDif         cF dF
#> 10 0.1773875 10.45461  0  1 -0.0008068642 0.04821488 0.09017539  1
#> 
#> [[11]]
#>            a        b         cR dR         aDif      bDif         cF dF
#> 11 0.2145648 8.497741 0.01457818  1 -0.007255575 0.3262305 0.00940506  1
#> 
#> [[12]]
#>            a        b cR dR       aDif      bDif cF dF
#> 12 0.1902239 10.08344  0  1 0.02673579 0.7950675  0  1
#> 
#> [[13]]
#>            a        b        cR dR          aDif        bDif        cF dF
#> 13 0.2057001 8.899444 0.2751976  1 -0.0001283659 0.006071304 0.2549051  1
#> 
#> [[14]]
#>           a        b cR dR        aDif       bDif cF dF
#> 14 0.241662 12.70136  0  1 -0.02730193 -0.4157416  0  1
#> 
#> [[15]]
#>            a        b cR dR        aDif       bDif cF dF
#> 15 0.1815815 11.30904  0  1 0.005811626 -0.9079578  0  1
#> 
#> [[16]]
#>            a        b cR dR         aDif      bDif cF dF
#> 16 0.2659021 12.38779  0  1 -0.008573015 -0.332986  0  1
#> 
#> [[17]]
#>            a        b cR        dR         aDif       bDif cF dF
#> 17 0.1732152 12.48312  0 0.9519813 -0.004470116 -0.6906748  0  1
#> 
#> [[18]]
#>            a        b cR dR        aDif      bDif cF dF
#> 18 0.2675659 12.85289  0  1 -0.01563246 0.0185113  0  1
#> 
#> [[19]]
#>            a        b cR dR        aDif      bDif cF dF
#> 19 0.2721297 11.85384  0  1 -0.07253556 -0.099127  0  1
#> 
#> [[20]]
#>            a        b cR dR       aDif      bDif cF dF
#> 20 0.1995594 12.57678  0  1 0.02575371 0.3910979  0  1
#> 
startNLR(Data, group, model = "4PL", match = "score", parameterization = "is")
#> [[1]]
#>          b1        b0 cR dR          b3        b2 cF dF
#> 1 0.2587156 -2.556264  0  1 -0.01463883 0.4459277  0  1
#> 
#> [[2]]
#>          b1        b0          cR dR         b3       b2 cF dF
#> 2 0.1964099 -1.838266 0.004788726  1 0.09897782 1.581949  0  1
#> 
#> [[3]]
#>          b1        b0        cR dR          b3           b2       cF dF
#> 3 0.1956394 -1.838901 0.2014277  1 0.003523097 -0.002901096 0.219731  1
#> 
#> [[4]]
#>          b1        b0        cR dR         b3          b2        cF dF
#> 4 0.1956052 -1.838929 0.3652006  1 0.01287445 -0.01060147 0.3920988  1
#> 
#> [[5]]
#>          b1        b0        cR dR            b3           b2        cF dF
#> 5 0.2308482 -1.809908 0.2849871  1 -0.0007091662 0.0005839633 0.3164206  1
#> 
#> [[6]]
#>          b1        b0 cR dR          b3         b2 cF dF
#> 6 0.2202756 -2.038191  0  1 0.006192905 0.09548757  0  1
#> 
#> [[7]]
#>          b1        b0 cR dR           b3          b2        cF dF
#> 7 0.2049089 -1.920657  0  1 -0.008101338 -0.08271823 0.1825676  1
#> 
#> [[8]]
#>          b1        b0        cR dR         b3        b2 cF dF
#> 8 0.1792728 -1.852378 0.1084787  1 0.04270131 0.1672253  0  1
#> 
#> [[9]]
#>          b1        b0 cR dR          b3          b2         cF dF
#> 9 0.1929044 -1.929591  0  1 -0.01354956 -0.07728035 0.05245522  1
#> 
#> [[10]]
#>           b1        b0 cR dR            b3           b2         cF dF
#> 10 0.1773875 -1.854516  0  1 -0.0008068642 7.836328e-05 0.09017539  1
#> 
#> [[11]]
#>           b1        b0         cR dR           b3          b2         cF dF
#> 11 0.2145648 -1.823316 0.01457818  1 -0.007255575 0.005974607 0.00940506  1
#> 
#> [[12]]
#>           b1        b0 cR dR         b3        b2 cF dF
#> 12 0.1902239 -1.918111  0  1 0.02673579 0.4420863  0  1
#> 
#> [[13]]
#>           b1        b0        cR dR            b3          b2        cF dF
#> 13 0.2057001 -1.830616 0.2751976  1 -0.0001283659 0.000105703 0.2549051  1
#> 
#> [[14]]
#>          b1        b0 cR dR          b3         b2 cF dF
#> 14 0.241662 -3.069437  0  1 -0.02730193 -0.4358902  0  1
#> 
#> [[15]]
#>           b1        b0 cR dR          b3         b2 cF dF
#> 15 0.1815815 -2.053513  0  1 0.005811626 -0.1044212  0  1
#> 
#> [[16]]
#>           b1        b0 cR dR           b3         b2 cF dF
#> 16 0.2659021 -3.293938  0  1 -0.008573015 -0.1918876  0  1
#> 
#> [[17]]
#>           b1        b0 cR        dR           b3        b2 cF dF
#> 17 0.1732152 -2.162267  0 0.9519813 -0.004470116 -0.172349  0  1
#> 
#> [[18]]
#>           b1        b0 cR dR          b3         b2 cF dF
#> 18 0.2675659 -3.438995  0  1 -0.01563246 -0.1962587  0  1
#> 
#> [[19]]
#>           b1        b0 cR dR          b3       b2 cF dF
#> 19 0.2721297 -3.225781  0  1 -0.07253556 -0.87961  0  1
#> 
#> [[20]]
#>           b1        b0 cR dR         b3        b2 cF dF
#> 20 0.1995594 -2.509816  0  1 0.02575371 0.4120183  0  1
#> 

# starting values for model specified for each item
startNLR(Data, group,
  model = c(
    rep("1PL", 5), rep("2PL", 5),
    rep("3PL", 5), rep("4PL", 5)
  )
)
#> [[1]]
#>           a          b      bDif
#> 1 0.7809904 -0.5545696 0.7790496
#> 
#> [[2]]
#>           a          b      bDif
#> 2 0.7624514 -0.7381788 0.7303649
#> 
#> [[3]]
#>           a         b       bDif
#> 3 0.4840608 -1.539724 -0.1385151
#> 
#> [[4]]
#>           a         b       bDif
#> 4 0.3897325 -2.602855 -0.3016395
#> 
#> [[5]]
#>           a         b       bDif
#> 5 0.5007513 -2.323334 -0.1747799
#> 
#> [[6]]
#>           a          b       aDif       bDif
#> 6 0.6841399 -0.7566681 0.01923415 0.05428855
#> 
#> [[7]]
#>           a          b       aDif       bDif
#> 7 0.6364134 -0.7179315 -0.1367562 -0.7418676
#> 
#> [[8]]
#>           a          b      aDif       bDif
#> 8 0.4963917 -0.8460623 0.1930233 0.03963645
#> 
#> [[9]]
#>           a          b       aDif        bDif
#> 9 0.5991293 -0.5152127 -0.0713027 -0.09418341
#> 
#> [[10]]
#>            a          b       aDif       bDif
#> 10 0.5509363 -0.3697532 -0.0519609 -0.3459183
#> 
#> [[11]]
#>            a          b         cR        aDif      bDif         cF
#> 11 0.6664031 -0.9998145 0.01457818 -0.02253462 0.1050379 0.00940506
#> 
#> [[12]]
#>            a          b cR       aDif      bDif cF
#> 12 0.5908041 -0.4892599  0 0.08303696 0.2559915  0
#> 
#> [[13]]
#>            a          b        cR          aDif        bDif        cF
#> 13 0.6388705 -0.8704762 0.2751976 -0.0003986834 0.001954805 0.2549051
#> 
#> [[14]]
#>            a         b cR        aDif       bDif cF
#> 14 0.7505623 0.3536455  0 -0.08479531 -0.1338582  0
#> 
#> [[15]]
#>            a           b cR       aDif       bDif cF
#> 15 0.5639624 -0.09464822  0 0.01804995 -0.2923393  0
#> 
#> [[16]]
#>            a         b cR dR        aDif      bDif cF dF
#> 16 0.8258482 0.2526813  0  1 -0.02662637 -0.107213  0  1
#> 
#> [[17]]
#>           a         b cR        dR        aDif       bDif cF dF
#> 17 0.537978 0.2833769  0 0.9519813 -0.01388344 -0.2223797  0  1
#> 
#> [[18]]
#>            a        b cR dR        aDif        bDif cF dF
#> 18 0.8310156 0.402433  0  1 -0.04855186 0.005960168  0  1
#> 
#> [[19]]
#>          a          b cR dR       aDif        bDif cF dF
#> 19 0.84519 0.08076388  0  1 -0.2252835 -0.03191637  0  1
#> 
#> [[20]]
#>            a         b cR dR       aDif      bDif cF dF
#> 20 0.6197988 0.3135336  0  1 0.07998677 0.1259236  0  1
#> 

# 4PL model with fixed a and c parameters
startNLR(Data, group, model = "4PL", constraints = "ac", simplify = TRUE)
#>            a           b          c        dR         bDif dF
#> 1  0.7809904 -0.55456959 0.00000000 1.0000000  0.779049635  1
#> 2  0.7624514 -0.73817885 0.00000000 1.0000000  0.730364950  1
#> 3  0.6131848 -0.67176982 0.21057931 1.0000000 -0.133868800  1
#> 4  0.6272347 -0.63916801 0.37864972 1.0000000 -0.341382325  1
#> 5  0.7160790 -1.15019290 0.30070384 1.0000000 -0.117053386  1
#> 6  0.6939306 -0.75666812 0.00000000 1.0000000  0.054288549  1
#> 7  0.6176581 -0.46617649 0.08011013 1.0000000 -0.672962198  1
#> 8  0.6107266 -0.72945727 0.02894087 1.0000000  0.006989234  1
#> 9  0.5723024 -0.46456293 0.01517289 1.0000000 -0.087341259  1
#> 10 0.5498378 -0.20634246 0.04501444 1.0000000 -0.328901482  1
#> 11 0.6552702 -1.00769206 0.01199162 1.0000000  0.121026227  1
#> 12 0.6324808 -0.48925994 0.00000000 1.0000000  0.255991496  1
#> 13 0.6388283 -0.91429945 0.26505136 1.0000000  0.088434376  1
#> 14 0.7083418  0.35364553 0.00000000 1.0000000 -0.133858213  1
#> 15 0.5731307 -0.09464822 0.00000000 1.0000000 -0.292339298  1
#> 16 0.8127383  0.25268128 0.00000000 1.0000000 -0.107213007  1
#> 17 0.5443904  0.28337692 0.00000000 0.9519813 -0.222379724  1
#> 18 0.8069415  0.40243297 0.00000000 1.0000000  0.005960168  1
#> 19 0.7327315  0.08076388 0.00000000 1.0000000 -0.031916372  1
#> 20 0.6599573  0.31353356 0.00000000 1.0000000  0.125923571  1
```
