# (PART\*) PART IV: Summary {-}



# Conclusions and recommendations {-}

Overall we can conclude that: 

  1. the assumptions of the statistical models frequently used are (highly) unlikely to be met for psychological data
  2. violating these assumptions has unpleasant and undesirable effects on the:
      * model parameters 
      * their associated standard errors, confidence intervals and
      * p-values
  3. traditional methods for dealing with violations of model assumptions are (often) ineffective
  4. there are numerous robust alternatives to the models
  5. they are straightforward to implement
  
## Recommendations {-}  
  
The possible **practical consequences** of violating assumptions include:

  * relatively low power
  * inaccurate confidence intervals
  * inaccurate measures of effect size (that miss important differences)

Given Micceri’s findings [@Micceri.1989] and according to the paper of Field and Wilcox [@FieldWilcox.2017] it seems highly improbable that every paper not explicitly demonstrating that model assumptions have been met have, in reality, met the model assumptions. Hence, following recommondations are given:

  * Scientists, reviewers and editors should assume that assumptions have not been met unless there is an explicit and compelling statement.
  * It must be backed up by evidence, that the assumptions of the models fit have been met, but it is not recommend that these statements are based upon significance tests of assumptions[^9]
  * **Currently the best way** of investigating the impact of violations of model assumptions is to **use a modern robust method and compare the results to the standard model**. If the assumptions are met, the expectation is that they will give consistent results. Otherwise, the conventional method is in doubt. 
  * Do not rely on large sample size! 
      - Heavy tail problems [@Micceri.1989][^10]
      - Skew problems[^11]
  * insist on sensitivity analysis for all frequentist analyses, i.e. non-robust estimators (such as OLS and ML) are compared to a robust variant. Where the two models yield ostensibly the same results then either model may be reported, where the models deviate substantially then the robust model should be reported unless a compelling evidence-based case can be made that model assumptions have been met. 
  * when for a certain problem a specific test is unavailable, it should be technically possible to bootstrap standard errors and confidence intervals from pretty much any model. 
  * statements along the lines of ‘ANOVA/regression/the t-test is robust’ should be banned!


[^9]: <font size="2"> because, under general conditions, such tests do not have enough power to detect violations of assumptions that have practical consequences [@Kesleman.2016]. </font>

[^10]: <font size="2"> he studied the distributional characteristics of 440 large-sample psychology-relevant measures. Remarkably, when looking at tail weight only 15.2% approximated a normal distribution and nearly 67% had at least one tail that was moderately to extremely heavy. In terms of symmetry, only 28.4% approximated a normal distribution with the remainder moderately to extremely skewed. Looking at both symmetry and tail weight together only 6.8% of the 440 distributions approximated normality. These data show that tail weight and symmetry consistent with a normal distribution is extremely rare in psychological data. </font>

[^11]: <font size="2"> poor control over the Type I error probability can occur even with large sample sizes. Imagine a two-sample Student's T, $N_1 = 400, sd = 1$, **sampling distribution = lognormal** distribution. $N_2= 1000, sd = 1$,  **sampling distribution = normal**. The Type I error probability is approximately **0.14** rather than the nominal 0.05 $\rightarrow$ regardless of how large the sample size might be, results can be misleading.</font>
