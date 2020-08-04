## Beets split plot example
### Søren Højsgaard

Also presented in  Halekoh, U., and Højsgaard, S. (2014) A Kenward-Roger Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed Models - the R Package pbkrtest. J. Stat. Soft. Vol. 59, Issue 9



```r
library(pbkrtest)
```

```
## Loading required package: lme4
```

```
## Loading required package: Matrix
```

```r
data(beets, package="pbkrtest")
```

Because the experiment is balanced, the correct tests can be made using
aov():


```r
beets$bh <- with(beets, interaction(block, harvest))
summary(aov(sugpct ~ block + sow + harvest + 
                Error(bh), data=beets))
```

```
## 
## Error: bh
##           Df  Sum Sq Mean Sq F value Pr(>F)  
## block      2 0.03267 0.01633   2.579 0.2794  
## harvest    1 0.09633 0.09633  15.211 0.0599 .
## Residuals  2 0.01267 0.00633                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Error: Within
##           Df Sum Sq Mean Sq F value   Pr(>F)    
## sow        4   1.01  0.2525     101 5.74e-13 ***
## Residuals 20   0.05  0.0025                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Alternative; use lmer


```r
beetLarge <- lmer(sugpct ~ block + sow + harvest + 
                      (1 | block:harvest), data=beets, REML=FALSE)
beet_no.harv <- update(beetLarge, .~. - harvest)
beet_no.sow  <- update(beetLarge, .~. - sow)
```

The effect of harvest is "too significant"; no problem with the effect of
sowing time compared to the aov() results above.


```r
anova(beetLarge, beet_no.harv)
```

```
## Data: beets
## Models:
## beet_no.harv: sugpct ~ block + sow + (1 | block:harvest)
## beetLarge: sugpct ~ block + sow + harvest + (1 | block:harvest)
##              Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
## beet_no.harv  9 -69.084 -56.473 43.542  -87.084                         
## beetLarge    10 -79.998 -65.986 49.999  -99.998 12.914      1  0.0003261
##                 
## beet_no.harv    
## beetLarge    ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(beetLarge, beet_no.sow)
```

```
## Data: beets
## Models:
## beet_no.sow: sugpct ~ block + harvest + (1 | block:harvest)
## beetLarge: sugpct ~ block + sow + harvest + (1 | block:harvest)
##             Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
## beet_no.sow  6  -2.795   5.612  7.398  -14.795                         
## beetLarge   10 -79.998 -65.986 49.999  -99.998 85.203      4  < 2.2e-16
##                
## beet_no.sow    
## beetLarge   ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

KRmodcomp remedies this


```r
KRmodcomp(beetLarge, beet_no.harv)
```

```
## F-test with Kenward-Roger approximation; computing time: 0.15 sec.
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##        stat   ndf   ddf F.scaling p.value  
## Ftest 15.21  1.00  2.00         1  0.0599 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
KRmodcomp(beetLarge, beet_no.sow)
```

```
## F-test with Kenward-Roger approximation; computing time: 0.09 sec.
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##       stat ndf ddf F.scaling   p.value    
## Ftest  101   4  20         1 5.741e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

PBmodcomp remedies this


```r
PBmodcomp(beetLarge, beet_no.harv)
```

```
## Parametric bootstrap test; time: 20.72 sec; samples: 1000 extremes: 25;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##          stat df   p.value    
## LRT    12.914  1 0.0003261 ***
## PBtest 12.914    0.0259740 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
PBmodcomp(beetLarge, beet_no.sow)
```

```
## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q

## Warning in optwrap(object@optinfo$optimizer, ff, x0, lower = lower, control
## = control$optCtrl, : convergence code 3 from bobyqa: bobyqa -- a trust
## region step failed to reduce q
```

```
## Parametric bootstrap test; time: 21.26 sec; samples: 1000 extremes: 0;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##          stat df   p.value    
## LRT    85.203  4 < 2.2e-16 ***
## PBtest 85.203     0.000999 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

