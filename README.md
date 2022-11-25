
<!-- README.md is generated from README.Rmd. Please edit only README.Rmd! -->

<!-- # dlmextra -->

<!-- Extra functionality for the dlm package (for dynamic linear models) -->

# `pbkrtest`: Parametric Bootstrap, Kenward-Roger and Satterthwaite Based Methods for Mixed Model Comparison

Attention is on mixed effects models (as implemented in the `lme4`
package). For linear mixed models, `pbkrtest` implements (1) a
parametric bootstrap test, (2) a Kenward-Roger-type F-test and (3) a
Satterthwaite-type F-test. The parametric bootstrap test is also
implemented for generalized linear mixed models (as implemented in
`lme4`) and for generalized linear models. The facilities of the
package are documented in the paper by Halekoh and Højsgaard, (2012, ).

Please see `citation("pbkrtest")` for information about citing the paper
and the package. If you use the package in your work, please do cite the
2012-paper. There are other packages that use `pbkrtest` under the hood.
If you use one of those packages, please do also cite our 2012 paper.

Documents:

1.  [Halekoh and Højsgaard (2012) A Kenward-Roger Approximation and
    Parametric Bootstrap Methods for Tests in Linear Mixed Models The R
    Package
    pbkrtest](https://www.jstatsoft.org/index.php/jss/article/view/v059i09/v59i09.pdf)
2.  [Vignette: introduction to
    `pbkrtest`](https://cran.r-project.org/package=pbkrtest/vignettes/pbkrtest.pdf)
3.  [Webpage for the
    package](https://people.math.aau.dk/~sorenh/software/pbkrtest/index.html)

<!-- badges: start 
[![R build status](https://github.com/hojsgaard/pbkrtest/workflows/R-CMD-check/badge.svg)](https://github.com/hojsgaard/pbkrtest/actions) 
[![codecov.io](https://codecov.io/gh/hojsgaard/dlmextra/branch/master/graphs/badge.svg)](https://codecov.io/gh/hojsgaard/dlmextra?branch=master)
badges: end -->

## Installation

`pbkrtest` is available on CRAN and can be installed as usual:

    install.packages('pbkrtest')

To build and install from Github with vignettes run this command from
within `R` (please install `remotes` first if not already installed):

    # install.packages('remotes')
    remotes::install_github("hojsgaard/pbkrtest", build_vignettes = TRUE)

You can also install the package without vignettes if needed as follows:

    remotes::install_github("hojsgaard/pbkrtest", build_vignettes = FALSE)

## Development site

See <https://github.com/hojsgaard/pbkrtest>.

## Online documentation

See <https://hojsgaard.github.io/pbkrtest/>.

## Brief introduction

``` r
library(pbkrtest)
library(ggplot2)

ggplot(sleepstudy) + geom_line(aes(Days, Reaction, group=Subject, color=Subject))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r

fm0 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
fm1 <- update(fm0, .~. - Days)

p0 <- anova(fm0, fm1)
p1 <- PBmodcomp(fm0, fm1)
p2 <- KRmodcomp(fm0, fm1)
p3 <- SATmodcomp(fm0, fm1)

p0
#> Data: sleepstudy
#> Models:
#> fm1: Reaction ~ (Days | Subject)
#> fm0: Reaction ~ Days + (Days | Subject)
#>     npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#> fm1    5 1785.5 1801.4 -887.74   1775.5                         
#> fm0    6 1763.9 1783.1 -875.97   1751.9 23.537  1  1.226e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
p1
#> Bootstrap test; time: 8.58 sec; samples: 1000; extremes: 0;
#> Requested samples: 1000 Used samples: 994 Extremes: 0
#> large : Reaction ~ Days + (Days | Subject)
#> Reaction ~ (Days | Subject)
#>          stat df   p.value    
#> LRT    23.516  1 1.239e-06 ***
#> PBtest 23.516     0.001005 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
p2
#> large : Reaction ~ Days + (Days | Subject)
#> small : Reaction ~ (Days | Subject)
#>         stat    ndf    ddf F.scaling   p.value    
#> Ftest 45.853  1.000 17.000         1 3.264e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
p3
#> large : Reaction ~ Days + (Days | Subject)
#> small (restriction matrix) : 
#>     
#>  0 1
#>      statistic    ndf ddf   p.value    
#> [1,]    45.853  1.000  17 3.264e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

tidy(p0)
#> Warning in tidy.anova(p0): The following column names in ANOVA output were not
#> recognized or transformed: npar
#> # A tibble: 2 x 9
#>   term   npar   AIC   BIC logLik deviance statistic    df     p.value
#>   <chr> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl> <dbl>       <dbl>
#> 1 fm1       5 1785. 1801.  -888.    1775.      NA      NA NA         
#> 2 fm0       6 1764. 1783.  -876.    1752.      23.5     1  0.00000123
tidy(p1)
#> # A tibble: 2 x 4
#>   type    stat    df    p.value
#>   <chr>  <dbl> <dbl>      <dbl>
#> 1 LRT     23.5     1 0.00000124
#> 2 PBtest  23.5    NA 0.00101
tidy(p2)
#> # A tibble: 2 x 6
#>   type    stat   ndf   ddf F.scaling    p.value
#>   <chr>  <dbl> <int> <dbl>     <dbl>      <dbl>
#> 1 Ftest   45.9     1   17.         1 0.00000326
#> 2 FtestU  45.9     1   17.        NA 0.00000326
tidy(p3)
#> # A tibble: 1 x 5
#>   type  statistic   ndf   ddf    p.value
#>   <chr>     <dbl> <int> <dbl>      <dbl>
#> 1 Ftest      45.9     1  17.0 0.00000326
```

Please find more examples in the other vignettes available at
<https://hojsgaard.github.io/pbkrtest/>.
