
<!-- README.md is generated from README.Rmd. Please edit only README.Rmd! -->

# `pbkrtest`: Parametric Bootstrap, Kenward-Roger and Satterthwaite Based Methods for Mixed Model Comparison

For linear mixed models (as implemented in the `lme4` package),
`pbkrtest` implements the following tests for fixed effects:

1.  a parametric bootstrap test,
2.  a Kenward-Roger-type F-test and
3.  a Satterthwaite-type F-test.

For generalized linear mixed models (as implemented in `lme4`) and for
generalized linear models, `pbkrtest` also implements a parametric
bootstrap test

The facilities of the package are documented in the paper by Halekoh and
Højsgaard, (2012, ).

Please see `citation("pbkrtest")` for information about citing the paper
and the package. If you use the package in your work, please do cite the
2012-paper.

There are other packages that use `pbkrtest` under the hood. If you use
one of those packages, please do also cite our 2012 paper.

Documents:

1.  [Halekoh and Højsgaard (2012) A Kenward-Roger Approximation and
    Parametric Bootstrap Methods for Tests in Linear Mixed Models The R
    Package
    pbkrtest](https://www.jstatsoft.org/index.php/jss/article/view/v059i09/v59i09.pdf)
2.  [Vignette: introduction to
    `pbkrtest`](https://cran.r-project.org/package=pbkrtest/vignettes/pbkrtest.pdf)
3.  [Webpage for the
    package](https://people.math.aau.dk/~sorenh/software/pbkrtest/index.html)

<!-- badges: 
[![R build status](https://github.com/hojsgaard/pbkrtest/workflows/R-CMD-check/badge.svg)](https://github.com/hojsgaard/pbkrtest/actions) 
[![codecov.io](https://codecov.io/gh/hojsgaard/dlmextra/branch/master/graphs/badge.svg)](https://codecov.io/gh/hojsgaard/dlmextra?branch=master)
badges: end -->

## Online documentation

See <https://hojsgaard.github.io/pbkrtest/>.

## Installation

`pbkrtest` is available on CRAN and development versions can also be
found on Github:

    ## Install from CRAN:
    install.packages('pbkrtest')
    ## Install from Github:
    # install.packages('remotes')
    remotes::install_github("hojsgaard/pbkrtest", build_vignettes = TRUE)

## Development site

See <https://github.com/hojsgaard/pbkrtest>.

## Brief introduction

``` r
library(pbkrtest)
library(ggplot2)

## Sugar beets: Does suger content depend on harvest time?

beets |> ggplot(aes(x=sow, y=sugpct, group=harvest)) +
    geom_jitter(aes(color=harvest), width=0)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r

fm0 <- lmer(sugpct ~ block + sow + harvest + (1|block:harvest), data=beets)
fm1 <- update(fm0, .~. -harvest)

## Is there an effect of harvest time?
an <- anova(fm0, fm1)
pb <- PBmodcomp(fm0, fm1)
kr <- KRmodcomp(fm0, fm1)
sa <- SATmodcomp(fm0, fm1)

tidy(an)
#> # A tibble: 2 × 9
#>   term   npar   AIC   BIC logLik deviance statistic    df   p.value
#>   <chr> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl> <dbl>     <dbl>
#> 1 fm1       9 -69.1 -56.5   43.5    -87.1      NA      NA NA       
#> 2 fm0      10 -80.0 -66.0   50.0   -100.       12.9     1  0.000326
tidy(pb)
#> # A tibble: 2 × 4
#>   type    stat    df  p.value
#>   <chr>  <dbl> <dbl>    <dbl>
#> 1 LRT     12.9     1 0.000326
#> 2 PBtest  12.9    NA 0.0360
tidy(kr)
#> # A tibble: 1 × 6
#>   type   stat   ndf   ddf F.scaling p.value
#>   <chr> <dbl> <int> <dbl>     <dbl>   <dbl>
#> 1 Ftest  15.2     1  2.00         1  0.0599
tidy(sa)
#> # A tibble: 1 × 5
#>   type  statistic   ndf   ddf p.value
#>   <chr>     <dbl> <int> <dbl>   <dbl>
#> 1 Ftest      15.2     1  2.00  0.0599
```

Please find more examples in the other vignettes available at
<https://hojsgaard.github.io/pbkrtest/>.
