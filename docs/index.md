---
title: "Robuste Methoden"
author: "Walter Gruber"
date: "2025-03-01"
description: "Maximize Accuracy and Power of Research"
cover-image: "Images/Cover.png"
github-repo: wgruber/RobustMethods
site: bookdown::bookdown_site
output:
  bookdown::gitbook: default
bibliography: "bibliography.bib"
biblio-style: "apalike"
link-citations: true
---

# {-}

<center>

![](Images/Assumptions.JPG){ width=100% }

</center>



# Foreword {-}

This summary of robust methods is (mainly) based on an articles of Andy Field and Rand Wilcox [@FieldWilcox.2017], David Erceg-Hurn et.al. [@Hurn.2008], Mair [@Mair.2020] and Wilcox [@Wilcox.2012]. Parts of the texts from the papers and books mentioned above have been included unchanged to this summary.

# Introduction {-}

Statistical tests, such as 

  - Correlation
  - Linear Regression 
  - t-Tests and ANOVA's
  - ANCOVA, Mediator- and Moderator-Analysis 

are widely used in psychology. These methods are part of a family of methods often referred to as parametric methods\footnote{\tiny The basic idea of these methods is that there is a set of fixed parameters that determine a probability model - hence the name parametrix methods}. In order to produce accurate results, assumptions underlying them must be satisfied, i.e.:

  - independence of errors between groups
  - normality of errors/residuals
  - homoscedasticity/sphericity

But these assumptions are rarely met when analyzing real data! The algorithms used to analyze the data nevertheless produce results. But when classic parametric methods are used with violated assumptions, following  result can/should/must be expected:

  - inaccurate computation of $p$ values.
  - biase effect sizes, and confidence intervals. 
  - substantive errors in the interpretation of data. 
  - substaintial loss of power.
  
For instance, the general linear model (GLM), has been considered to be robust to violations of its assumptions. 
However, based on many journal articles over the last decades it is well established that this view is incorrect. Many modern robust statistical methods alleviate the problems inherent in using parametric methods with violated assumptions. 

<center>

![**Figure 1**: use StatParaPlast to solve all your nasty statistical problems. Look inside the package to disover the fantastic world on NNS (No Nonsense Statistic). Available in different sizes and colors.](Images/RobustHansaPlast.JPG){ width=30% }

</center>

## Roadmap {-}

We will have a look at some (selected) robust statistical procedures which can solve some, if not most of the afore mentioned problems. Robust methods are nowadays easily conducted with software such as SPSS, R, SAS, Matlab, but despite the advantages they are rarely used.

To start off with, we will have a closer look a the core properties of linear models, i.e. we want to make sure to understand the assumptions and moreover, what is meant by not finding them in our data!

Then we will define what is meant by *robustness* and *robust methods*. They are still rarely used in psychological research [cf. @Hurn.2008], mainly because many researchers are unaware of the limitations of classic methods and do not realize that modern alternatives exist. To at least get an insight in some of the techniques we will provide a practical, nontechnical introduction to some of these methods.
