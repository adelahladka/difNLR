# Creates a formula for non-linear regression DIF models.

The function returns the formula of the non-linear regression DIF model
based on model specification and DIF type to be tested.

## Usage

``` r
formulaNLR(model, constraints = NULL, type = "all", parameterization = "irt",
           outcome)
```

## Arguments

- model:

  character: generalized logistic regression model for which starting
  values should be estimated. See **Details**.

- constraints:

  character: which parameters should be the same for both groups.
  Possible values are any combinations of parameters `"a"`, `"b"`,
  `"c"`, and `"d"`. Default value is `NULL`.

- type:

  character: type of DIF to be tested. Possible values are `"all"` for
  detecting difference in any parameter (default), `"udif"` for uniform
  DIF only (i.e., difference in difficulty parameter `"b"`), `"nudif"`
  for non-uniform DIF only (i.e., difference in discrimination parameter
  `"a"`), `"both"` for uniform and non-uniform DIF (i.e., difference in
  parameters `"a"` and `"b"`), or any combination of parameters `"a"`,
  `"b"`, `"c"`, and `"d"`. Can be specified as a single value (for all
  items) or as an item-specific vector.

- parameterization:

  character: parameterization of regression coefficients. Possible
  options are `"irt"` (IRT parameterization, default), `"is"`
  (intercept-slope), and `"logistic"` (logistic regression as in the
  [`glm`](https://rdrr.io/r/stats/glm.html) function, available for the
  `"2PL"` model only). See **Details**.

- outcome:

  character: name of outcome to be printed in formula. If not specified
  `"y"` is used.

## Value

A list of two models. Each includes a formula, parameters to be
estimated, and their lower and upper constraints.

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
# 3PL model with the same guessing parameter for both groups
# to test both types of DIF
formulaNLR(model = "3PLcg", type = "both")
#> $M0
#> $M0$formula
#> y ~ c + (1 - c)/(1 + exp(-a * (x - b)))
#> <environment: 0x55630badfe50>
#> 
#> $M0$parameters
#> [1] "a" "b" "c"
#> 
#> $M0$lower
#>    a    b    c 
#> -Inf -Inf    0 
#> 
#> $M0$upper
#>   a   b   c 
#> Inf Inf   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (1 - c)/(1 + exp(-(a + aDif * g) * (x - (b + bDif * g))))
#> <environment: 0x55630badfe50>
#> 
#> $M1$parameters
#> [1] "a"    "b"    "c"    "aDif" "bDif"
#> 
#> $M1$lower
#>    a    b    c aDif bDif 
#> -Inf -Inf    0 -Inf -Inf 
#> 
#> $M1$upper
#>    a    b    c aDif bDif 
#>  Inf  Inf    1  Inf  Inf 
#> 
#> 
formulaNLR(model = "3PLcg", type = "both", parameterization = "is")
#> $M0
#> $M0$formula
#> y ~ c + (1 - c)/(1 + exp(-(b0 + b1 * x)))
#> <environment: 0x55630ba7cea8>
#> 
#> $M0$parameters
#> [1] "b0" "b1" "c" 
#> 
#> $M0$lower
#>   b0   b1    c 
#> -Inf -Inf    0 
#> 
#> $M0$upper
#>  b0  b1   c 
#> Inf Inf   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (1 - c)/(1 + exp(-(b0 + b1 * x + b2 * g + b3 * x * g)))
#> <environment: 0x55630ba7cea8>
#> 
#> $M1$parameters
#> [1] "b0" "b1" "b2" "b3" "c" 
#> 
#> $M1$lower
#>   b0   b1   b2   b3    c 
#> -Inf -Inf -Inf -Inf    0 
#> 
#> $M1$upper
#>  b0  b1  b2  b3   c 
#> Inf Inf Inf Inf   1 
#> 
#> 

# 4PL model with the same guessing and inattention parameters
# to test uniform DIF
formulaNLR(model = "4PLcgdg", type = "udif")
#> $M0
#> $M0$formula
#> y ~ c + (d - c)/(1 + exp(-a * (x - b)))
#> <environment: 0x55630ba047c0>
#> 
#> $M0$parameters
#> [1] "a" "b" "c" "d"
#> 
#> $M0$lower
#>    a    b    c    d 
#> -Inf -Inf    0    0 
#> 
#> $M0$upper
#>   a   b   c   d 
#> Inf Inf   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (d - c)/(1 + exp(-a * (x - (b + bDif * g))))
#> <environment: 0x55630ba047c0>
#> 
#> $M1$parameters
#> [1] "a"    "b"    "c"    "d"    "bDif"
#> 
#> $M1$lower
#>    a    b    c    d bDif 
#> -Inf -Inf    0    0 -Inf 
#> 
#> $M1$upper
#>    a    b    c    d bDif 
#>  Inf  Inf    1    1  Inf 
#> 
#> 
formulaNLR(model = "4PLcgdg", type = "udif", parameterization = "is")
#> $M0
#> $M0$formula
#> y ~ c + (d - c)/(1 + exp(-(b0 + b1 * x)))
#> <environment: 0x55630b9a95b0>
#> 
#> $M0$parameters
#> [1] "b0" "b1" "c"  "d" 
#> 
#> $M0$lower
#>   b0   b1    c    d 
#> -Inf -Inf    0    0 
#> 
#> $M0$upper
#>  b0  b1   c   d 
#> Inf Inf   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (d - c)/(1 + exp(-(b0 + b1 * x + b2 * g)))
#> <environment: 0x55630b9a95b0>
#> 
#> $M1$parameters
#> [1] "b0" "b1" "b2" "c"  "d" 
#> 
#> $M1$lower
#>   b0   b1   b2    c    d 
#> -Inf -Inf -Inf    0    0 
#> 
#> $M1$upper
#>  b0  b1  b2   c   d 
#> Inf Inf Inf   1   1 
#> 
#> 

# 2PL model to test non-uniform DIF
formulaNLR(model = "2PL", type = "nudif")
#> $M0
#> $M0$formula
#> y ~ 1/(1 + exp(-a * (x - (b + bDif * g))))
#> <environment: 0x55630b93b808>
#> 
#> $M0$parameters
#> [1] "a"    "b"    "bDif"
#> 
#> $M0$lower
#>    a    b bDif 
#> -Inf -Inf -Inf 
#> 
#> $M0$upper
#>    a    b bDif 
#>  Inf  Inf  Inf 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ 1/(1 + exp(-(a + aDif * g) * (x - (b + bDif * g))))
#> <environment: 0x55630b93b808>
#> 
#> $M1$parameters
#> [1] "a"    "b"    "aDif" "bDif"
#> 
#> $M1$lower
#>    a    b aDif bDif 
#> -Inf -Inf -Inf -Inf 
#> 
#> $M1$upper
#>    a    b aDif bDif 
#>  Inf  Inf  Inf  Inf 
#> 
#> 
formulaNLR(model = "2PL", type = "nudif", parameterization = "is")
#> $M0
#> $M0$formula
#> y ~ 1/(1 + exp(-(b0 + b1 * x + b2 * g)))
#> <environment: 0x55630b8df238>
#> 
#> $M0$parameters
#> [1] "b0" "b1" "b2"
#> 
#> $M0$lower
#>   b0   b1   b2 
#> -Inf -Inf -Inf 
#> 
#> $M0$upper
#>  b0  b1  b2 
#> Inf Inf Inf 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ 1/(1 + exp(-(b0 + b1 * x + b2 * g + b3 * x * g)))
#> <environment: 0x55630b8df238>
#> 
#> $M1$parameters
#> [1] "b0" "b1" "b2" "b3"
#> 
#> $M1$lower
#>   b0   b1   b2   b3 
#> -Inf -Inf -Inf -Inf 
#> 
#> $M1$upper
#>  b0  b1  b2  b3 
#> Inf Inf Inf Inf 
#> 
#> 
formulaNLR(model = "2PL", type = "nudif", parameterization = "logistic")
#> $M0
#> $M0$formula
#> y ~ x + g
#> <environment: 0x55630b87c850>
#> 
#> $M0$parameters
#> [1] "(Intercept)" "x"           "g"          
#> 
#> $M0$lower
#> NULL
#> 
#> $M0$upper
#> NULL
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ x + g + x:g
#> <environment: 0x55630b87c850>
#> 
#> $M1$parameters
#> [1] "(Intercept)" "x"           "g"           "x:g"        
#> 
#> $M1$lower
#> NULL
#> 
#> $M1$upper
#> NULL
#> 
#> 

# 4PL model to test all possible DIF
formulaNLR(model = "4PL", type = "all", parameterization = "irt")
#> $M0
#> $M0$formula
#> y ~ c + (d - c)/(1 + exp(-a * (x - b)))
#> <environment: 0x55630b827d08>
#> 
#> $M0$parameters
#> [1] "a" "b" "c" "d"
#> 
#> $M0$lower
#>    a    b    c    d 
#> -Inf -Inf    0    0 
#> 
#> $M0$upper
#>   a   b   c   d 
#> Inf Inf   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ (cR * (1 - g) + cF * g) + (dR * (1 - g) + dF * g - (cR * 
#>     (1 - g) + cF * g))/(1 + exp(-(a + aDif * g) * (x - (b + bDif * 
#>     g))))
#> <environment: 0x55630b827d08>
#> 
#> $M1$parameters
#> [1] "a"    "b"    "cR"   "dR"   "aDif" "bDif" "cF"   "dF"  
#> 
#> $M1$lower
#>    a    b   cR   dR aDif bDif   cF   dF 
#> -Inf -Inf    0    0 -Inf -Inf    0    0 
#> 
#> $M1$upper
#>    a    b   cR   dR aDif bDif   cF   dF 
#>  Inf  Inf    1    1  Inf  Inf    1    1 
#> 
#> 
formulaNLR(model = "4PL", type = "all", parameterization = "is")
#> $M0
#> $M0$formula
#> y ~ c + (d - c)/(1 + exp(-(b0 + b1 * x)))
#> <environment: 0x55630b6de7a0>
#> 
#> $M0$parameters
#> [1] "b0" "b1" "c"  "d" 
#> 
#> $M0$lower
#>   b0   b1    c    d 
#> -Inf -Inf    0    0 
#> 
#> $M0$upper
#>  b0  b1   c   d 
#> Inf Inf   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ (cR * (1 - g) + cF * g) + (dR * (1 - g) + dF * g - (cR * 
#>     (1 - g) + cF * g))/(1 + exp(-(b0 + b1 * x + b2 * g + b3 * 
#>     x * g)))
#> <environment: 0x55630b6de7a0>
#> 
#> $M1$parameters
#> [1] "b0" "b1" "b2" "b3" "cR" "cF" "dR" "dF"
#> 
#> $M1$lower
#>   b0   b1   b2   b3   cR   cF   dR   dF 
#> -Inf -Inf -Inf -Inf    0    0    0    0 
#> 
#> $M1$upper
#>  b0  b1  b2  b3  cR  cF  dR  dF 
#> Inf Inf Inf Inf   1   1   1   1 
#> 
#> 

# 4PL model with fixed a and c parameters
# to test difference in b
formulaNLR(model = "4PL", constraints = "ac", type = "b")
#> $M0
#> $M0$formula
#> y ~ c + (dR * (1 - g) + dF * g - c)/(1 + exp(-a * (x - b)))
#> <environment: 0x55630b6695c8>
#> 
#> $M0$parameters
#> [1] "a"  "b"  "c"  "dR" "dF"
#> 
#> $M0$lower
#>    a    b    c   dR   dF 
#> -Inf -Inf    0    0    0 
#> 
#> $M0$upper
#>   a   b   c  dR  dF 
#> Inf Inf   1   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (dR * (1 - g) + dF * g - c)/(1 + exp(-a * (x - (b + bDif * 
#>     g))))
#> <environment: 0x55630b6695c8>
#> 
#> $M1$parameters
#> [1] "a"    "b"    "c"    "dR"   "bDif" "dF"  
#> 
#> $M1$lower
#>    a    b    c   dR bDif   dF 
#> -Inf -Inf    0    0 -Inf    0 
#> 
#> $M1$upper
#>    a    b    c   dR bDif   dF 
#>  Inf  Inf    1    1  Inf    1 
#> 
#> 
formulaNLR(model = "4PL", constraints = "ac", type = "b", parameterization = "is")
#> $M0
#> $M0$formula
#> y ~ c + (dR * (1 - g) + dF * g - c)/(1 + exp(-(b0 + b1 * x)))
#> <environment: 0x55630b5f02a0>
#> 
#> $M0$parameters
#> [1] "b0" "b1" "c"  "dR" "dF"
#> 
#> $M0$lower
#>   b0   b1    c   dR   dF 
#> -Inf -Inf    0    0    0 
#> 
#> $M0$upper
#>  b0  b1   c  dR  dF 
#> Inf Inf   1   1   1 
#> 
#> 
#> $M1
#> $M1$formula
#> y ~ c + (dR * (1 - g) + dF * g - c)/(1 + exp(-(b0 + b1 * x + 
#>     b2 * g)))
#> <environment: 0x55630b5f02a0>
#> 
#> $M1$parameters
#> [1] "b0" "b1" "b2" "c"  "dR" "dF"
#> 
#> $M1$lower
#>   b0   b1   b2    c   dR   dF 
#> -Inf -Inf -Inf    0    0    0 
#> 
#> $M1$upper
#>  b0  b1  b2   c  dR  dF 
#> Inf Inf Inf   1   1   1 
#> 
#> 
```
