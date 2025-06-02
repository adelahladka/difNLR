# S3 methods of estimNLR on GMAT data

    Code
      print(fit_nls)
    Output
      Nonlinear regression model 
      
       Model:  y ~ c + (1 - c)/(1 + exp(-(a + aDif * g) * (x - (b + bDif * g)))) 
      
      Coefficients:
            a       b       c    aDif    bDif 
       1.1211 -0.3037  0.1106 -0.0645  0.9497 
      
       Nonlinear least squares estimation 
      Converged after 8 iterations

