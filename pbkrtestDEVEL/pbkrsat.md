## Satterthwaite in pbkrtest
### Søren Højsgaard
date: `date()`


```r
library(ggplot2)
library(broom)
load_all("pbkrtest")
```

```
## Loading pbkrtest
```

## Sleep study data


```r
qplot(Days, Reaction, color=Subject, data=sleepstudy) + geom_line()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Quadratic curve:


```r
(fit1 <- lmer(Reaction ~ Days + I(Days^2) + (Days|Subject), sleepstudy))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Reaction ~ Days + I(Days^2) + (Days | Subject)
##    Data: sleepstudy
## REML criterion at convergence: 1742.816
## Random effects:
##  Groups   Name        Std.Dev. Corr
##  Subject  (Intercept) 24.761       
##           Days         5.925   0.06
##  Residual             25.534       
## Number of obs: 180, groups:  Subject, 18
## Fixed Effects:
## (Intercept)         Days    I(Days^2)  
##     255.449        7.434        0.337
```

```r
summary(fit1) %>% coef
```

```
##                Estimate Std. Error   t value
## (Intercept) 255.4493728  7.5135467 33.998507
## Days          7.4340850  2.8188719  2.637255
## I(Days^2)     0.3370223  0.2619154  1.286760
```

Remove quadratic term; corresponds to restriction matrix (0, 0, 1)


```r
(fit2 <- update(fit1, ~.-I(Days^2)))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Reaction ~ Days + (Days | Subject)
##    Data: sleepstudy
## REML criterion at convergence: 1743.628
## Random effects:
##  Groups   Name        Std.Dev. Corr
##  Subject  (Intercept) 24.741       
##           Days         5.922   0.07
##  Residual             25.592       
## Number of obs: 180, groups:  Subject, 18
## Fixed Effects:
## (Intercept)         Days  
##      251.41        10.47
```

```r
summary(fit2) %>% coef
```

```
##              Estimate Std. Error   t value
## (Intercept) 251.40510   6.824597 36.838090
## Days         10.46729   1.545790  6.771481
```

## Three methods available

Each method takes argument in different forms


```r
L <- c(0, 0, 1)
```

### Kenward-Roger



```r
k1 <- KRmodcomp(fit1, L); k1
```

```
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## L = 
##      [,1] [,2] [,3]
## [1,]    0    0    1
##           stat      ndf      ddf F.scaling p.value
## Ftest   1.6558   1.0000 143.0000         1  0.2003
```

```r
k2 <- KRmodcomp(fit1, fit2); k2
```

```
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## small : Reaction ~ Days + (Days | Subject)
##           stat      ndf      ddf F.scaling p.value
## Ftest   1.6558   1.0000 143.0000         1  0.2003
```

```r
k3 <- KRmodcomp(fit1, ~.-I(Days^2)); k3
```

```
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## small : Reaction ~ Days + (Days | Subject)
##           stat      ndf      ddf F.scaling p.value
## Ftest   1.6558   1.0000 143.0000         1  0.2003
```

### Satterthwaite


```r
s1 <- SATmodcomp(fit1, L); s1
```

```
## Reaction ~ Days + I(Days^2) + (Days | Subject)
##      Fvalue    ndf ddf p.value
## [1,] 1.6558 1.0000 143  0.2003
```

```r
s2 <- SATmodcomp(fit1, fit2); s2
```

```
## Reaction ~ Days + I(Days^2) + (Days | Subject)
##      Fvalue    ndf ddf p.value
## [1,] 1.6558 1.0000 143  0.2003
```

```r
s3 <- SATmodcomp(fit1, ~.-I(Days^2)); s3
```

```
## Reaction ~ Days + I(Days^2) + (Days | Subject)
##      Fvalue    ndf ddf p.value
## [1,] 1.6558 1.0000 143  0.2003
```

### Parametric bootstrap


```r
p1 <- PBmodcomp(fit1, L); p1
```

```
## Bootstrap test; time: 7.74 sec;samples: 1000; extremes: 196;
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## small :      [,1] [,2] [,3]
## [1,]    0    0    1
##          stat df p.value
## LRT    1.6218  1  0.2028
## PBtest 1.6218     0.1968
```

```r
p2 <- PBmodcomp(fit1, fit2); p2
```

```
## Bootstrap test; time: 8.68 sec;samples: 1000; extremes: 216;
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## small : Reaction ~ Days + (Days | Subject)
##          stat df p.value
## LRT    1.6684  1  0.1965
## PBtest 1.6684     0.2168
```

```r
p3 <- PBmodcomp(fit1, ~.-I(Days^2)); p3
```

```
## Bootstrap test; time: 8.84 sec;samples: 1000; extremes: 193;
## large : Reaction ~ Days + I(Days^2) + (Days | Subject)
## small : Reaction ~ Days + (Days | Subject)
##          stat df p.value
## LRT    1.6684  1  0.1965
## PBtest 1.6684     0.1938
```

## Beets data


```r
data(beets)

fit1 <- lmer(sugpct ~ block + sow + harvest + 
                 (1 | block:harvest), data=beets, REML=FALSE)
```

Remove harvest effect


```r
fit2 <- update(fit1, ~.-harvest)

L <- c(rep(0, 7), 1)
```

### Kenward-Roger



```r
k1 <- KRmodcomp(fit1, L); k1
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## L = 
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    0    0    0    0    0    0    0    1
##        stat   ndf   ddf F.scaling p.value  
## Ftest 15.21  1.00  2.00         1  0.0599 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
k2 <- KRmodcomp(fit1, fit2); k2
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##        stat   ndf   ddf F.scaling p.value  
## Ftest 15.21  1.00  2.00         1  0.0599 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
k3 <- KRmodcomp(fit1, ~.-harvest); k3
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##        stat   ndf   ddf F.scaling p.value  
## Ftest 15.21  1.00  2.00         1  0.0599 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Satterthwaite


```r
s1 <- SATmodcomp(fit1, L); s1
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue    ndf    ddf   p.value    
## [1,] 45.632  1.000 6.0002 0.0005133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
s2 <- SATmodcomp(fit1, fit2); s2
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue    ndf    ddf   p.value    
## [1,] 45.632  1.000 6.0002 0.0005133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
s3 <- SATmodcomp(fit1, ~.-harvest); s3
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue    ndf    ddf   p.value    
## [1,] 45.632  1.000 6.0002 0.0005133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Parametric bootstrap


```r
p1 <- PBmodcomp(fit1, L); p1
```

```
## Bootstrap test; time: 6.11 sec;samples: 1000; extremes: 38;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small :      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    0    0    0    0    0    0    0    1
##          stat df   p.value    
## LRT    12.914  1 0.0003261 ***
## PBtest 12.914    0.0389610 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
p2 <- PBmodcomp(fit1, fit2); p2
```

```
## Bootstrap test; time: 6.05 sec;samples: 1000; extremes: 31;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##          stat df   p.value    
## LRT    12.914  1 0.0003261 ***
## PBtest 12.914    0.0319680 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
p3 <- PBmodcomp(fit1, ~.-harvest); p3
```

```
## Bootstrap test; time: 6.01 sec;samples: 1000; extremes: 35;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + sow + (1 | block:harvest)
##          stat df   p.value    
## LRT    12.914  1 0.0003261 ***
## PBtest 12.914    0.0359640 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Remove sow effect



```r
fit2 <- update(fit1, ~.-sow)
```

```
## boundary (singular) fit: see ?isSingular
```

```r
model2remat(fit1, fit2) %>% zapsmall
```

```
##      [,1] [,2] [,3]       [,4]       [,5]       [,6]       [,7] [,8]
## [1,]    0    0    0  0.9176629 -0.2294157 -0.2294157 -0.2294157    0
## [2,]    0    0    0  0.0636285  0.8907987 -0.3181424 -0.3181424    0
## [3,]    0    0    0  0.1048285  0.1048285  0.8386279 -0.5241424    0
## [4,]    0    0    0 -0.3779645 -0.3779645 -0.3779645 -0.7559289    0
```

```r
L <- matrix(0, nr=4, nc=8)
L[, 4:7] <- diag(1, 4)
```

### Kenward-Roger



```r
k1 <- KRmodcomp(fit1, L); k1
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## L = 
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    0    0    0    1    0    0    0    0
## [2,]    0    0    0    0    1    0    0    0
## [3,]    0    0    0    0    0    1    0    0
## [4,]    0    0    0    0    0    0    1    0
##       stat ndf ddf F.scaling   p.value    
## Ftest  101   4  20         1 5.741e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
k2 <- KRmodcomp(fit1, fit2); k2
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##       stat ndf ddf F.scaling   p.value    
## Ftest  101   4  20         1 5.741e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
k3 <- KRmodcomp(fit1, ~.-sow); k3
```

```
## boundary (singular) fit: see ?isSingular
```

```
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##       stat ndf ddf F.scaling   p.value    
## Ftest  101   4  20         1 5.741e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Satterthwaite


```r
s1 <- SATmodcomp(fit1, L); s1
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue   ndf ddf   p.value    
## [1,]  121.2   4.0  24 1.554e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
s2 <- SATmodcomp(fit1, fit2); s2
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue   ndf ddf   p.value    
## [1,]  121.2   4.0  24 1.554e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
s3 <- SATmodcomp(fit1, ~.-sow); s3
```

```
## boundary (singular) fit: see ?isSingular
```

```
## sugpct ~ block + sow + harvest + (1 | block:harvest)
##      Fvalue   ndf ddf   p.value    
## [1,]  121.2   4.0  24 1.554e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Parametric bootstrap


```r
p1 <- PBmodcomp(fit1, L); p1
```

```
## boundary (singular) fit: see ?isSingular
## boundary (singular) fit: see ?isSingular
```

```
## Bootstrap test; time: 6.02 sec;samples: 1000; extremes: 0;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small :      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    0    0    0    1    0    0    0    0
## [2,]    0    0    0    0    1    0    0    0
## [3,]    0    0    0    0    0    1    0    0
## [4,]    0    0    0    0    0    0    1    0
##          stat df   p.value    
## LRT    85.203  4 < 2.2e-16 ***
## PBtest 85.203     0.000999 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
p2 <- PBmodcomp(fit1, fit2); p2
```

```
## Bootstrap test; time: 5.86 sec;samples: 1000; extremes: 0;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##          stat df   p.value    
## LRT    85.203  4 < 2.2e-16 ***
## PBtest 85.203     0.000999 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
p3 <- PBmodcomp(fit1, ~.-sow); p3
```

```
## boundary (singular) fit: see ?isSingular
```

```
## Bootstrap test; time: 6.65 sec;samples: 1000; extremes: 0;
## large : sugpct ~ block + sow + harvest + (1 | block:harvest)
## small : sugpct ~ block + harvest + (1 | block:harvest)
##          stat df   p.value    
## LRT    85.203  4 < 2.2e-16 ***
## PBtest 85.203     0.000999 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

