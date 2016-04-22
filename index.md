---
title       : Reproducible Pitch Presentation
subtitle    : Developing Data Products
author      : Nuno Melo
job         : Data Scientist (want to be)
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---





## Executive Summary
This presentation  analyses mtcars data from the 1974 Motor Trend US magazine 
and aims to create a linear regression model, predicting car fuel consumption:  

**Agenda**
* Is manual transmission better than automatic for fuel consumption?
* Selected predictive regression model: mpg ~ wt + qsec + am
* Model validaty with residuals plot

--- .class #id 
## Is manual better than automatic for mpg?

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

---

## Is manual better than automatic for mpg? (cont.)
* Regression model selected: mpg ~ qsec + wt + am
* The graphs shows that no firm conclusion can be taken
* For wt > 3.4 (1000 lbs) manual transmission seams to be worse
* However we only have one car above this level, i.e. the prediction model may 
not be accurate for manual cars with wt > 3.4

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

---

## Model validaty with residuals plot
* No relevant systematic patterns or large outlying observations  
* The data is approximately normally distributed
* No highly influential or high leverage and outlying points 

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)





