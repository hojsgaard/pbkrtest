library(pbkrtest)
library(nlme)
library(lme4)

data(Orthodont)
Orth1 <- Orthodont
Orth1$visno <- rep(1:4, nrow(Orth1)/4)
set.seed(20181011)
Orth1 <- Orth1[sample(0:1, size=nrow(Orth1), replace=TRUE, prob=c(0.75, 0.25)) == 0, ]
Orth2 <- Orth1[sample(1:nrow(Orth1)), ]
# so getData.gls can find it:
assign('Orth1', Orth1, envir=.GlobalEnv)
assign('Orth2', Orth2, envir=.GlobalEnv)

context("Tests for gls, lme, lmer")

test_that("test corCompSymm vs random intercept", {
  fm0.lmer <- lmer(distance ~ 1 + (1|Subject), data = Orthodont)
  fm1.lmer <- lmer(distance ~ age + Sex + (1|Subject), data = Orthodont)
  fm0.lme <- lme(distance ~ 1, data = Orthodont, random = ~ 1|Subject)
  fm1.lme <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1|Subject)
  fm0.gls <- gls(distance ~ 1, data = Orthodont, correlation = corCompSymm(form = ~ 1 | Subject))
  fm1.gls <- gls(distance ~ age + Sex, data = Orthodont, correlation = corCompSymm(form = ~ 1 | Subject))
  
  kr.lmer <- KRmodcomp(fm0.lmer, fm1.lmer)
  kr.lme <- KRmodcomp(fm0.lme, fm1.lme)
  kr.gls <- KRmodcomp(fm0.gls, fm1.gls)
  
  expect_equal(kr.lmer$test, kr.lme$test)
  expect_equal(kr.lmer$test, kr.gls$test)
})


test_that("test robustness to missing and permuted data", {
  fm01.lmer <- lmer(distance ~ 1 + (age|Subject), data = Orth1)
  fm11.lmer <- lmer(distance ~ age + Sex + (age|Subject), data = Orth1)
  fm02.lmer <- lmer(distance ~ 1 + (age|Subject), data = Orth2)
  fm12.lmer <- lmer(distance ~ age + Sex + (age|Subject), data = Orth2)
  kr1.lmer <- KRmodcomp(fm01.lmer, fm11.lmer)
  kr2.lmer <- KRmodcomp(fm02.lmer, fm12.lmer)
  expect_equal(kr1.lmer$test, kr2.lmer$test, tolerance=1e-05)
  
  fm01.lme <- lme(distance ~ 1, data = Orth1, random = ~ age|Subject)
  fm11.lme <- lme(distance ~ age + Sex, data = Orth1, random = ~ age|Subject)
  fm02.lme <- lme(distance ~ 1, data = Orth2, random = ~ age|Subject)
  fm12.lme <- lme(distance ~ age + Sex, data = Orth2, random = ~ age|Subject)
  kr1.lme <- KRmodcomp(fm01.lme, fm11.lme)
  kr2.lme <- KRmodcomp(fm02.lme, fm12.lme)
  expect_equal(kr1.lme$test, kr2.lme$test, tolerance=1e-05)

  fm01.gls <- gls(distance ~ 1, data = Orth1, correlation = corCompSymm(form = ~ 1 | Subject))
  fm11.gls <- gls(distance ~ age + Sex, data = Orth1, correlation = corCompSymm(form = ~ 1 | Subject))
  fm02.gls <- gls(distance ~ 1, data = Orth2, correlation = corCompSymm(form = ~ 1 | Subject))
  fm12.gls <- gls(distance ~ age + Sex, data = Orth2, correlation = corCompSymm(form = ~ 1 | Subject))
  kr1.gls <- KRmodcomp(fm01.gls, fm11.gls)
  kr2.gls <- KRmodcomp(fm02.gls, fm12.gls)
  expect_equal(kr1.gls$test, kr2.gls$test)

  fm01.gls <- gls(distance ~ 1, data = Orth1, correlation = corAR1(form = ~ visno | Subject))
  fm11.gls <- gls(distance ~ age + Sex, data = Orth1, correlation = corAR1(form = ~ visno | Subject))
  fm02.gls <- gls(distance ~ 1, data = Orth2, correlation = corAR1(form = ~ visno | Subject))
  fm12.gls <- gls(distance ~ age + Sex, data = Orth2, correlation = corAR1(form = ~ visno | Subject))
  kr1.gls <- KRmodcomp(fm01.gls, fm11.gls)
  kr2.gls <- KRmodcomp(fm02.gls, fm12.gls)
  expect_equal(kr1.gls$test, kr2.gls$test)

  fm01.gls <- gls(distance ~ 1, data = Orth1, correlation = corCompSymm(form = ~ visno | Subject), varIdent(form = ~ 1 | visno))
  fm11.gls <- gls(distance ~ age + Sex, data = Orth1, correlation = corCompSymm(form = ~ visno | Subject), varIdent(form = ~ 1 | visno))
  fm02.gls <- gls(distance ~ 1, data = Orth2, correlation = corCompSymm(form = ~ visno | Subject), varIdent(form = ~ 1 | visno))
  fm12.gls <- gls(distance ~ age + Sex, data = Orth2, correlation = corCompSymm(form = ~ visno | Subject), varIdent(form = ~ 1 | visno))
  kr1.gls <- KRmodcomp(fm01.gls, fm11.gls)
  kr2.gls <- KRmodcomp(fm02.gls, fm12.gls)
  expect_equal(kr1.gls$test$ddf, kr2.gls$test$ddf, tolerance=1)
  expect_equal(kr1.gls$test$stat, kr2.gls$test$stat, tolerance=0.5)
  expect_equal(kr1.gls$test$pvalue, kr2.gls$test$pvalue)  
})
