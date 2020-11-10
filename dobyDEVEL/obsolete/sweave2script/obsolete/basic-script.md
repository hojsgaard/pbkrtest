


---

# Summarizing data
#### Søren Højsgaard

The `shoes` data is a list of two vectors, giving the wear of shoes of
materials `X` and `Y` for one foot each of ten boys.


```r
data(shoes, package="MASS")
names(shoes) <- c("x","y")
shoes
```

```
## $x
##  [1] 13.2  8.2 10.9 14.3 10.7  6.6  9.5 10.8  8.8 13.3
## 
## $y
##  [1] 14.0  8.8 11.2 14.2 11.8  6.4  9.8 11.3  9.3 13.6
```


First focus on data for material `A`;


```r
x <- shoes$x; x
```

```
##  [1] 13.2  8.2 10.9 14.3 10.7  6.6  9.5 10.8  8.8 13.3
```


We shall look at measures of where is the "location" or "center" of the data and what is the
"spread" of data.

---

For the sum $x_1 + x_2 + x_3 + \dots + x_7+ x_8$ we write
$$
  x_. = \Sigma_{i=1}^8 x_i = x_1 + x_2 + x_3 + \dots + x_7+ x_8
$$

