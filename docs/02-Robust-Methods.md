# (PART\*) PART II: Robust Methods {-}





# Definition of Robust {-}

If we look up Wikipedia we find the following description of [Robust Statistic](https://en.wikipedia.org/wiki/Robust_statistics){target="_blank"}:

*Robust statistics* are statistics with good performance for data drawn from a wide range of probability distributions, especially for distributions that are not normal. 

*Robust statistical methods* have been developed for many common problems, such as estimating location, scale, and regression parameters. One motivation is to produce statistical methods that are not unduly affected by outliers. 

Another motivation is to provide methods with good performance when there are small departures from parametric distribution. For example, robust methods work well for mixtures of two normal distributions with different standard-deviations; under this model, non-robust methods like a t-test work poorly. Another definition of robust measures frequently used is:

> Measures that characterize a distribution (such as location and scale) are said to be *robust*, if slight changes in a distribution have a relatively small effect on their value [@Wilcox.2012] (page 23).

The mathematical foundation of robust methods (dealing with quantitative, qualitative and infnitesimal robustness of parameters) makes no assumptions regarding the functional form of the probability distribution. 

The basic trick is to view parameters as functionals; expressions for the standard error follow from the influence function. Robust inferential methods are available that perform well with relatively small sample sizes, even in situations where classic methods based on means and variances perform poorly with relatively large sample sizes. Modern robust methods have the potential of substantially increasing power even under slight departures from normality. And perhaps more importantly, they can provide a deeper, more accurate and more nuanced understanding of data compared to classic techniques based on means.

# Solutions to violated assumptions {-}

The consequences of normality deviations such as:

  - skewed distributions
  - data with outliers, or
  - heavy-tailed distributions

can influence the results of any  classical (i.e. parametric) statistical analysis quite substantially. Seen from a purely descriptive angle, it is trivial that the mean can be heavily affected by outliers or highly skewed distributional shapes. 

## Alternative estimators {-}

Computing the mean on "ugly" data is just not a good location measure to characterize the sample. In this case one strategy is to use more robust measures such as the *median* or the *trimmed mean*, *winsorized mean*, etc. and perform tests based on the corresponding sampling distribution of such robust measures.

It is quite common in experimental psychopathology research to do manual trims of the data based on outlier detection techniques (e.g., standard deviation based trims[^6] or idiosyncratic deletion).

Other popular alternatives are the M-estimators. They determine whether a score is an outlier empirically and if it is, adjustments are made for it. The adjustment could be to completely ignore the observation or to down-weight it. Obvious advantages of M-estimators are that you can:

  1. down-weight rather than exclude observations.
  2. avoid over- or under-trimming your data.
  3. perform non-symmetric trimming [@Wilcox.2012]
  
When assumptions (independent, homoscedastic and normally-distributed errors) are not met, the ML estimator will yield different results to the OLS. The ML estimator is a lot more versatile than OLS and tends to be the default for more complex variants of the linear model (such as multilevel models, models with latent variables etc.). 

[^6]: <font size="2"> e.g. Ratcliff (1993), with reaction time data to use standard deviation based trims such as excluding scores greater than 2.5 standard deviations from the mean. Be aware that this approach is flawed because both the mean and standard deviation are highly influenced by outliers (whether overt ones, or covert ones such as in a mixed normal distribution! </font>

## Transformation of data {-}

<!-- see also [transform data to normal](https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/){target="_blank"} -->

Another strategy to deal with such violations (especially with skewed data) is to apply [transformations](https://en.wikipedia.org/wiki/Data_transformation_(statistics) "Data transformations"){target="_blank"}
such as the *logarithm* or more sophisticated [Box-Cox transformations](https://de.wikipedia.org/wiki/Box-Cox-Transformation "Variance stabilization method"){target="_blank"}. However, before working with transformed data, the following points should be considered [also see @Wilcox.2012]:

  1. transformations seldom improve the validity of probability statements.
  2. transforming changes the hypothesis being tested (log transformed means compare geometric, rather than arithmetic means).
  3. transforming the data also transforms the construct that it measures, so interpretation might become difficult.
  4. the consequences of applying the ‘wrong’ transformation must be less severe than the consequences of analyzing the untransformed scores.
  5. heavy tails matter more than skew, so a transformation would need to address (and not make worse) any problems related to tail weight!
  6. distributions often remain skewed after transformation.
  7. transformations generally do not deal effectively with (real) outliers.

## Nonparametric tests {-}

Another option is to switch into the nonparametric testing world [@Brunner.2002]. Prominent examples for classical nonparametric tests taught in most introductory statistics class are:

  - Mann-Whitney U-test (Mann and Whitney, 1947)
  - Wilcoxon signed-rank and rank-sum test (Wilcoxon, 1945)
  - Kruskal-Wallis ANOVA (Kruskal and Wallis 1952)
  - Friedmann ANOVA (Friedmann, 1937)

## Robust methods {-}

Developments of robust methods can be traced back to the 1960's with publications by Tukey (1960), Huber (1964), and Hampel (1968). 

Modern robust methods have the potential of:

  - substantially increasing power even under slight departures from normality. 
  - can provide a deeper, more accurate and more nuanced understanding of data 
  
compared to classic techniques based on means. 

## Adjusting the standard errors {-}

There are ways to adjust standard errors to be robust in the presence of heteroscedasticity. One is known as the *Eiker-White-Huber* heteroscedasticity-consistent stranded errors. The resulting robust standard errors can be used to compute confidence intervals, test-statistics (and associated p-values) that are robust to heteroscedasticity.

Another way to deal with bias in standard errors and (confidence intervals) is to estimate them empirically. The *Bootstrap* is a flexible and general empirical method to find standard errors and confidence intervals for any statistic that is usually more accurate than traditional approaches. As with heteroscedasticityconsistent standard errors, bootstrap standard errors (and associated test statistics and p-values) and confidence intervals should be robust to violations of the assumptions.

# Selected Robust measures of location {-}

Measures of location are core elements of robust methods. Such measurese are:

  - Trimmend mean
  - Winsorized mean
  - Huber M-estimator

## Trimmed mean {-}

The trimmed mean discards a certain percentage at both ends of the distribution. For instance, a 20% trimmed mean cuts off 20% at the low end and 20% the high end. In R, a trimmed mean can be computed via the basic mean function by setting the trim argument accordingly. The following code also shows in which way the trimmed mean is calculated when the argument `trim` of the mean function is set to a value $> 0$.


``` r
  set.seed(423)
  N       <- 20 # length of sample
  M       <- 100 # Mean of sample
  SD      <- 15  # standard deviation of sample
  TF      <- 0.1 # Trim-Faktor
  #=== Generate Data ===
  IQ      <- rnorm(N, mean = M, sd = SD)
  IQ[N]   <- 1240 # introduce outlier
  set.seed(2384)
  #=== Manual trim ===
  # IQ_Red  <- sort(na.omit(IQ)) # remove NA's and sort
  IQ_Sort <- sort(IQ) # sort Vector
  RedLgth <- N*TF
  TInd    <- (RedLgth+1):(N - RedLgth)
  IQ_Trim <- IQ_Sort[TInd]
  #=== Means ===
  M1      <- round(mean(IQ, na.rm = T), 3)
  M2      <- round(mean(IQ, na.rm = T, trim = TF), 3)
  M3      <- round(mean(IQ_Trim, na.rm = T), 3)
  Means   <- data.frame(Mean = M1, T_Mean = M2, Man_T_Mean = M3)
  knitr::kable(Means, booktabs = TRUE, caption = 'mean vs. trimmed mean')
```



Table: (\#tab:TrimmedMean)mean vs. trimmed mean

|    Mean|  T_Mean| Man_T_Mean|
|-------:|-------:|----------:|
| 159.085| 103.143|    103.143|

``` r
  #=== SDs ===
  SD1     <- round(sd(IQ) / sqrt(length(IQ)), 3)      # standard error
  SD2     <- round(sd(IQ_Trim)/ sqrt(length(IQ_Trim)), 3) # standard error trimmed
  SD3     <- round(trimse(IQ, tr = TF), 3)      # standard error from WRS2
  StdDevs <- data.frame(SD = SD1, T_SD = SD2, WRS2_SD = SD3)
  knitr::kable(StdDevs, booktabs = TRUE, caption = 'Standard deviations')
```



Table: (\#tab:TrimmedMean)Standard deviations

|     SD|  T_SD| WRS2_SD|
|------:|-----:|-------:|
| 56.988| 3.292|   4.295|

Note that if the trimming portion is set to $\gamma = 0.5$, the trimmed mean $\bar{x}_t$ results in the *median* $\tilde{x}$ (which by itself *reflects another robust location measure*).

## Winsorized mean {-}

A further robust location alternative to the mean is the *Winsorized mean*. 

> The process of giving less weight to observations in the tails of the distribution and higher weight to the ones in the center is called *Winsorizing*.

Instead of computing the mean on the original distribution we compute the mean on the Winsorized distribution. Similar to the trimmed mean, the amount of Winsorizing (i.e., the *Winsorizing level*) has to be choosen a priori. The `WRS2` function to compute Windsorized means is called `winmean`.

There is also a function supplied in the `DescTools` Package, the `DescTools::Winsorize()` function. The following code and output shows some examples of these estimators:


``` r
  #=== Generate Data
  timevec <- c(92, 19, 101, 58, 1053, 91, 26, 78, 10, 13, -40, 101, 
               86, 85, 15, 89, 89, 28, -5, 41)
  # timevec <- c( 77,  87,  88, 114, 151, 210, 219,  246, 253, 262, 
  #              296, 299, 306, 376, 428, 515, 666, 1310, 2611)
  timevec <- sort(timevec)
  #=== Trimmed Mean and SE
  TM      <- mean(timevec, trim = TF) # calculate the trimmed mean of the time vector
  TSE     <- trimse(timevec, tr = TF) #calculate the trimmed mean of the time vector
  #=== Winsorized Mean an SE, Median and SE of Median
  WinS_Mean <- winmean(timevec, tr = TF, na.rm = FALSE) # winsorized mean
  WinS_SE   <- winse(timevec, tr = TF) # winsorized mean
  Med       <- median(timevec) # winsorized mean
  Med_SE    <- msmedse(timevec) # winsorized mean
  tv_DT     <- DescTools::Winsorize(timevec, 
                                    val = quantile(timevec, 
                                                   probs = c(TF, 1-TF),
                                    na.rm = FALSE))
  M_DT      <- mean(tv_DT)
  #=== manually winsorized
  QTV              <- quantile(timevec, probs = c(TF, 1-TF))
  QTIndUG          <- timevec <= QTV[1]
  QTIndOG          <- timevec >= QTV[2]
  timevec[QTIndUG] <- QTV[1]
  timevec[QTIndOG] <- QTV[2]
  ManMeanWS        <- mean(timevec)
```

Note that winsorizing is not equivalent to simply excluding (trimming) data, but is a method of censoring data. Thus a winsorized mean is not the same as a truncated mean (cf. [Winsorizing, Wikipedia](https://en.wikipedia.org/wiki/Winsorizing){target="_blank"}).

  - the 10% trimmed mean is the average of the 5th to 95th percentile of the data
  - the 90% winsorized mean sets the bottom 5% to the 5th percentile, the top 5% to the 95th percentile, and then averages the data. 
  
### Winsorization Round-Up {-}

Following round-up is taken from this [SAS-blog](https://blogs.sas.com/content/iml/2017/02/08/winsorization-good-bad-and-ugly.html){target="_blank"}:

**The good:**

  * The purpose of Winsorization is to "robustify" classical statistics by reducing the impact of extreme observations.
  * If you compare a Winsorized statistic with classical statistic, you can identify variables that might contain contaminated data or are long-tailed and require special handling in models.

**The ugly:**

Modifying the data is a draconian measure. In his book [@Tukey.1960], he says:

When statisticians encounter a few extreme values in data, we are likely to think of them as *strays*, *wild shots* and to focus our attention on how normally distributed the rest of the distribution appears to be. One who does this commits two oversights:

  * forgetting Winsor's principle that *all distributions are normal in the middle*, and 
  * forgetting that the distribution relevant to statistical practice is that of the values actually provided and not of the values which ought to have been provided.

Concluding a bit further on:

> Sets of observations which have been *de-tailed* by *over-vigorous* use of a rule for rejecting outliers are inappropriate, since they are not samples.

## M-estimators {-}

A general family of robust location measures are so called $M$-estimators (the $M$ stands for **M**aximum Likelihood-type"). They are based on a loss function to be minimized. Huber (1981) proposed a function [cf. @Wilcox.2012] in which a *bending constant **K** *  increases sensitivity to the tails of the distribution. The estimation of M-estimators is performed iteratively and implemented in the `mest()` function.


``` r
  MWHub <- mest(timevec, bend = 1.28, na.rm = FALSE) # Huber-estimator
  SEHub <- mestse(timevec)                           # Huber-estimator for SE
```

# Measures of Scale {-}

There are several robust measure of scales, of which we will only name a few, without getting into the mathematical details of their properties. Currently there are two general approaches to measuring scale that are of importance:

  1. [L-measures](https://en.wikipedia.org/wiki/L-estimator "L-estimator"){target="_blank"}:  is an estimator which is a linear combination of order statistics[^7] of the measurements (which is also called an L-statistic). This can be as little as a single point, as in the median (of an odd number of values), or as many as all points, as in the mean. The main benefits of L-estimators are that they are often extremely simple, and *robust*.
  2. [M-measures](https://en.wikipedia.org/wiki/M-estimator "M-estimator"){target="_blank"}: proposedby Huber (1964) these measures are a generalization of the maximum likelihood estimations. The $M$ stands for *Maximum likelihood type*. M-measures can be constructed for location parameters and scale parameters in univariate and multivariate settings, as well as being used in robust regression. 

[^7]: together with rank statistics, order statistics are among the most fundamental tools in non-parametric statistics and inference. Examples of order statistics are maximum, minimum, range, quantiles, median, etc.

Different measures of scale used frequently are:

  1. Mean Deviation from the Mean
  2. Mean Deviation from the Median
  3. Median Absolute Deviation
  4. q-Quantile Range
  5. Winsorized Variance

Some of these measures will be used and discussed in the next chapter. For further details to their definition and properties refer to [page36 @Wilcox.2012].
