
# From sweave to an R script
#### Søren Højsgaard



## The shoes data

Consider the shoes data from the MASS package:



```r
data(shoes, package="MASS")
shoes
```

```
## $A
##  [1] 13.2  8.2 10.9 14.3 10.7  6.6  9.5 10.8  8.8 13.3
## 
## $B
##  [1] 14.0  8.8 11.2 14.2 11.8  6.4  9.8 11.3  9.3 13.6
```



We shall do
*   an unpaired $t$-test
  and
*   a paired $t$-test

Compare two shoe types with a $t$-test:



```r
with(shoes, t.test(A, B))
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  A and B
## t = -0.36891, df = 17.987, p-value = 0.7165
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.745046  1.925046
## sample estimates:
## mean of x mean of y 
##     10.63     11.04
```


The test is misleading because observations are paired. A better
alternative is to make a paired $t$-test:



```r
with(shoes, t.test(A, B, paired=T))
```

```
## 
## 	Paired t-test
## 
## data:  A and B
## t = -3.3489, df = 9, p-value = 0.008539
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.6869539 -0.1330461
## sample estimates:
## mean of the differences 
##                   -0.41
```




