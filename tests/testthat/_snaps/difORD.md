# difORD - examples at help page

    Code
      (fit1 <- difORD(Data, group, focal.name = 1, model = "adjacent"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      AIC(fit1)
    Output
       [1]  868.7506  828.2355  762.8070 1172.1709  834.1942 1066.3088 1327.1609
       [8] 1222.1158 1355.7572  698.8149 1342.9936 1431.2480 1320.1761 1413.2859
      [15]  966.0284 1322.9039  511.0202 1571.5938  705.2756  912.1476 1287.1975
      [22] 1051.6305 1422.2094 1246.3794 1972.2709 1450.7519 1090.9833 1380.3714
      [29]  900.9954

---

    Code
      BIC(fit1)
    Output
       [1]  891.9565  851.4414  786.0129 1195.3768  857.4002 1098.7970 1359.6492
       [8] 1245.3217 1378.9631  722.0208 1366.1995 1454.4539 1343.3820 1436.4918
      [15]  989.2343 1346.1098  534.2261 1594.7997  737.7639  944.6358 1310.4034
      [22] 1074.8364 1445.4153 1269.5853 1995.4768 1473.9578 1114.1892 1403.5774
      [29]  924.2013

---

    Code
      logLik(fit1)
    Output
       [1] -429.3753 -409.1178 -376.4035 -581.0855 -412.0971 -526.1544 -656.5804
       [8] -606.0579 -672.8786 -344.4075 -666.4968 -710.6240 -655.0880 -701.6429
      [15] -478.0142 -656.4519 -250.5101 -780.7969 -345.6378 -449.0738 -638.5987
      [22] -520.8152 -706.1047 -618.1897 -981.1354 -720.3759 -540.4916 -685.1857
      [29] -445.4977

---

    Code
      AIC(fit1, item = 1)
    Output
      [1] 868.7506

---

    Code
      BIC(fit1, item = 1)
    Output
      [1] 891.9565

---

    Code
      logLik(fit1, item = 1)
    Output
      'log Lik.' -429.3753 (df=5)

---

    Code
      (fit2 <- difORD(Data, group, focal.name = 1, model = "adjacent",
        p.adjust.method = "BH"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      Multiple comparisons made with Benjamini-Hochberg adjustment of p-values
      
          Chisq-value P-value Adj. P-value  
      R1   1.2551      0.5339  0.7832       
      R2   5.9526      0.0510  0.2957       
      R3   0.0852      0.9583  0.9583       
      R4   0.1258      0.9390  0.9583       
      R5   1.0432      0.5936  0.7832       
      R6   9.8619      0.0072  0.0698      .
      R7   9.9535      0.0069  0.0698      .
      R8   1.0119      0.6029  0.7832       
      R9   2.8220      0.2439  0.5148       
      R10  5.2412      0.0728  0.3517       
      R11  2.5074      0.2855  0.5519       
      R12  4.0344      0.1330  0.4342       
      R13  1.6216      0.4445  0.7161       
      R14  0.5069      0.7761  0.9003       
      R15  1.6559      0.4370  0.7161       
      R16  3.9444      0.1391  0.4342       
      R17  1.7717      0.4124  0.7161       
      R18  0.1236      0.9401  0.9583       
      R19  9.1928      0.0101  0.0731      .
      R20 11.1244      0.0038  0.0698      .
      R21  3.0459      0.2181  0.5148       
      R22  3.7980      0.1497  0.4342       
      R23  2.7844      0.2485  0.5148       
      R24  0.5137      0.7735  0.9003       
      R25  1.0364      0.5956  0.7832       
      R26  0.9524      0.6211  0.7832       
      R27  0.2938      0.8634  0.9583       
      R28  4.3879      0.1115  0.4342       
      R29  3.4921      0.1745  0.4600       
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      None of items is detected as DIF

---

    Code
      (fit3 <- difORD(Data, group, focal.name = 1, model = "adjacent", purify = TRUE))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was applied with 2 iterations
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.6404      0.4403   
      R2   6.7384      0.0344 * 
      R3   0.0320      0.9841   
      R4   0.0008      0.9996   
      R5   0.8556      0.6520   
      R6   7.8654      0.0196 * 
      R7   9.9131      0.0070 **
      R8   0.7217      0.6971   
      R9   2.7545      0.2523   
      R10  4.2581      0.1190   
      R11  2.8458      0.2410   
      R12  3.7298      0.1549   
      R13  1.2787      0.5276   
      R14  0.4712      0.7901   
      R15  1.9018      0.3864   
      R16  4.1030      0.1285   
      R17  2.2937      0.3176   
      R18  0.1617      0.9223   
      R19  7.4481      0.0241 * 
      R20 11.2324      0.0036 **
      R21  2.7952      0.2472   
      R22  4.2428      0.1199   
      R23  2.1986      0.3331   
      R24  0.7738      0.6792   
      R25  0.5600      0.7558   
      R26  0.6024      0.7399   
      R27  0.7387      0.6912   
      R28  4.2304      0.1206   
      R29  3.1896      0.2030   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit4 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "udif"))
    Output
      Detection of uniform Differential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1  0.5126      0.4740    
      R2  5.9329      0.0149  * 
      R3  0.0688      0.7932    
      R4  0.0093      0.9231    
      R5  0.0026      0.9592    
      R6  9.7515      0.0018  **
      R7  5.2111      0.0224  * 
      R8  0.0037      0.9518    
      R9  2.7367      0.0981  . 
      R10 1.8363      0.1754    
      R11 2.0607      0.1511    
      R12 3.0119      0.0827  . 
      R13 1.1911      0.2751    
      R14 0.1871      0.6654    
      R15 0.7101      0.3994    
      R16 3.8040      0.0511  . 
      R17 1.5132      0.2186    
      R18 0.1219      0.7270    
      R19 6.9075      0.0086  **
      R20 9.7346      0.0018  **
      R21 2.4667      0.1163    
      R22 3.7512      0.0528  . 
      R23 1.0600      0.3032    
      R24 0.3149      0.5747    
      R25 0.3647      0.5459    
      R26 0.2691      0.6039    
      R27 0.0963      0.7563    
      R28 3.4448      0.0635  . 
      R29 3.2854      0.0699  . 
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as uniform DIF items:
       R2
       R6
       R7
       R19
       R20

---

    Code
      (fit5 <- difORD(Data, group, focal.name = 1, model = "adjacent", type = "nudif")
      )
    Output
      Detection of non-uniformDifferential Item Functioning for
      ordinal data using adjacent category logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value  
      R1  0.7425      0.3889   
      R2  0.0196      0.8886   
      R3  0.0164      0.8980   
      R4  0.1165      0.7329   
      R5  1.0406      0.3077   
      R6  0.1104      0.7397   
      R7  4.7424      0.0294  *
      R8  1.0083      0.3153   
      R9  0.0853      0.7702   
      R10 3.4049      0.0650  .
      R11 0.4467      0.5039   
      R12 1.0224      0.3119   
      R13 0.4305      0.5117   
      R14 0.3198      0.5717   
      R15 0.9458      0.3308   
      R16 0.1404      0.7079   
      R17 0.2585      0.6112   
      R18 0.0017      0.9672   
      R19 2.2852      0.1306   
      R20 1.3898      0.2384   
      R21 0.5792      0.4466   
      R22 0.0468      0.8287   
      R23 1.7244      0.1891   
      R24 0.1989      0.6557   
      R25 0.6718      0.4124   
      R26 0.6833      0.4085   
      R27 0.1974      0.6568   
      R28 0.9431      0.3315   
      R29 0.2067      0.6494   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as non-uniform DIF items:
       R7

---

    Code
      (fit6 <- difORD(Data, group, focal.name = 1, model = "adjacent", match = "score")
      )
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using adjacent category logit regression
      model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value   
      R1   1.2551      0.5339   
      R2   5.9526      0.0510 . 
      R3   0.0852      0.9583   
      R4   0.1258      0.9390   
      R5   1.0432      0.5936   
      R6   9.8619      0.0072 **
      R7   9.9535      0.0069 **
      R8   1.0119      0.6029   
      R9   2.8220      0.2439   
      R10  5.2412      0.0728 . 
      R11  2.5074      0.2855   
      R12  4.0344      0.1330   
      R13  1.6216      0.4445   
      R14  0.5069      0.7761   
      R15  1.6559      0.4370   
      R16  3.9444      0.1391   
      R17  1.7717      0.4124   
      R18  0.1236      0.9401   
      R19  9.1928      0.0101 * 
      R20 11.1244      0.0038 **
      R21  3.0459      0.2181   
      R22  3.7980      0.1497   
      R23  2.7844      0.2485   
      R24  0.5137      0.7735   
      R25  1.0364      0.5956   
      R26  0.9524      0.6211   
      R27  0.2938      0.8634   
      R28  4.3879      0.1115   
      R29  3.4921      0.1745   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R19
       R20

---

    Code
      (fit7 <- difORD(Data, group, focal.name = 1, model = "cumulative"))
    Output
      Detection of both types of Differential Item Functioning
      for ordinal data using cumulative logit regression model
      
      Likelihood-ratio Chi-square statistics
      
      Item purification was not applied
      No p-value adjustment for multiple comparisons
      
          Chisq-value P-value    
      R1   0.5461      0.7611    
      R2   4.0133      0.1344    
      R3   0.4022      0.8178    
      R4   0.1243      0.9397    
      R5   0.1602      0.9230    
      R6  13.8917      0.0010 ***
      R7   9.3795      0.0092 ** 
      R8   1.2370      0.5388    
      R9   4.3732      0.1123    
      R10  6.1645      0.0459 *  
      R11  1.9460      0.3779    
      R12  3.8974      0.1425    
      R13  1.8335      0.3998    
      R14  0.7203      0.6976    
      R15  0.8203      0.6636    
      R16  3.4129      0.1815    
      R17  1.0467      0.5925    
      R18  0.9115      0.6340    
      R19  9.0748      0.0107 *  
      R20 10.6796      0.0048 ** 
      R21  5.9576      0.0509 .  
      R22  3.7256      0.1552    
      R23  2.5638      0.2775    
      R24  0.2662      0.8754    
      R25  1.1370      0.5664    
      R26  0.7860      0.6750    
      R27  0.5704      0.7519    
      R28  2.9219      0.2320    
      R29  3.5553      0.1690    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Items detected as DIF items:
       R6
       R7
       R10
       R19
       R20

