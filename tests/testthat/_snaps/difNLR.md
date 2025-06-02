# S3 methods on generated data

    Code
      print(fit1)
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 3PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
             Chisq-value P-value    
      Item1   4.9683      0.1741    
      Item2   0.2487      0.9694    
      Item3   2.7038      0.4396    
      Item4   5.8151      0.1210    
      Item5  46.2121      0.0000 ***
      Item6   6.0731      0.1081    
      Item7   3.2390      0.3562    
      Item8  16.7771      0.0008 ***
      Item9   1.8514      0.6038    
      Item10  4.5160      0.2109    
      Item11 70.9566      0.0000 ***
      Item12  8.1931      0.0422 *  
      Item13  2.5850      0.4601    
      Item14  2.9478      0.3997    
      Item15 20.9397      0.0001 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 7.8147 (significance level: 0.05)
      
      Items detected as DIF items:
       Item5
       Item8
       Item11
       Item12
       Item15

---

    Code
      print(fit2)
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 2PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      Multiple comparisons made with Holm adjustment of p-values
      
             Chisq-value P-value Adj. P-value    
      Item1   5.0935      0.0783  0.6267         
      Item2   0.2487      0.8831  1.0000         
      Item3   2.0373      0.3611  1.0000         
      Item4   5.8151      0.0546  0.5322         
      Item5  46.2121      0.0000  0.0000      ***
      Item6   5.8665      0.0532  0.5322         
      Item7   3.2390      0.1980  1.0000         
      Item8  16.7550      0.0002  0.0028      ** 
      Item9   1.8514      0.3963  1.0000         
      Item10  4.5160      0.1046  0.7319         
      Item11 70.9566      0.0000  0.0000      ***
      Item12  8.4855      0.0144  0.1580         
      Item13  2.3430      0.3099  1.0000         
      Item14  2.0090      0.3662  1.0000         
      Item15 20.9397      0.0000  0.0004      ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
       Item5
       Item8
       Item11
       Item15

---

    Code
      print(fit3)
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression likelihood ratio chi-square statistics
      based on 2PL model 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was not applied
      Multiple comparisons made with Holm adjustment of p-values
      
             Chisq-value P-value Adj. P-value 
      Item1  8.5724      0.0138  0.1513       
      Item2  1.1745      0.5559  1.0000       
      Item3  4.3910      0.1113  0.7791       
      Item4  2.2868      0.3187  1.0000       
      Item6  4.7702      0.0921  0.7366       
      Item7  0.8918      0.6403  1.0000       
      Item9  0.8713      0.6468  1.0000       
      Item10 1.5961      0.4502  1.0000       
      Item12 6.8857      0.0320  0.3197       
      Item13 5.2930      0.0709  0.6381       
      Item14 0.4120      0.8138  1.0000       
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Detection thresholds: 5.9915 (significance level: 0.05)
      None of items is detected as DIF 

---

    Code
      print(fit4)
    Output
      Detection of all types of differential item functioning
      using the generalized logistic regression model
      
      Generalized logistic regression F-test statistics 
      
      Parameters were estimated using non-linear least squares
      
      Item purification was applied with 1 iteration.
      No p-value adjustment for multiple comparisons
      
             F-value P-value    
      Item1   4.2049  0.0152 *  
      Item2   0.3323  0.7174    
      Item3   1.8126  0.1638    
      Item4   2.7726  0.0630 .  
      Item5  23.6069  0.0000 ***
      Item6   3.2081  0.0409 *  
      Item7   1.0844  0.3385    
      Item8   4.4006  0.0044 ** 
      Item9   0.2695  0.8474    
      Item10  0.8806  0.4506    
      Item11 15.6033  0.0000 ***
      Item12  2.3788  0.0683 .  
      Item13  1.6938  0.1667    
      Item14  1.6725  0.1713    
      Item15  7.5024  0.0001 ***
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      
      Items detected as DIF items:
       Item1
       Item5
       Item6
       Item8
       Item11
       Item15

