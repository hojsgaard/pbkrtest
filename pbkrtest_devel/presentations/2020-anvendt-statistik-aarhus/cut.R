

---
## The Kenward--Roger approach

### The Kenward--Roger modification of the F--statistic

For multivariate normal data
\[
  Y \sim  N( X \beta, \Sigma)
\]
we consider the test of the hypothesis
\[
  \Lb_{d \times p} (\betab -\bm \beta_0) = 0
\]
where $\Lb$ is a regular matrix of estimable functions of $\bm \beta$. 

With $\hat \betab \sim N_d(\betab, \bm\Phi)$, a Wald statistic is

$$
  W = [\Lb(\hat\betab - \betab_0)]\transp [L\bm\Phi L\transp]\inv [\Lb(\hat\betab - \betab_0)]
$$
which is asymptotically $W \sim \chi^2_d$ under the null hypothesis.

---

A scaled version of $W$ is
\begin{displaymath}
  F = \frac{1}{d} W
\end{displaymath}


* Asymptotically $F \sim \frac{1}{d} \chi^2_d$
under the null hypothesis

* Think of as the limiting distribution
of an $F_{d,m}$--distribution as $m\rightarrow \infty$

* To account for the fact that $\bm\Phi=\var(\hat\beta)$
is estimated from data, we must come up with a better estimate of the
denominator degrees of freedom $m$ (better than $m=\infty$).

* That was what Kenward and Roger worked on...

---

The linear hypothesis $\Lb \betab = \betab_0$
can be tested via the  Wald-type  statistic
\begin{gather*}
F= \frac{1}{r}(\hat \betab - \betab_0)^\top \Lb^\top   (\Lb^\top \bm \Phi(\ssb) \Lb)^{-1}
 \Lb (\hat \betab - \betab_0)
\end{gather*}

\begin{itemize}
\item $\bm \Phi (\sigmab) = (\bm X^\top \bm \Sigma(\sigmab) \bm X)^{-1} \approx
\cov(\hat \betab)$, $\hat  \betab$ REML estimate of $\betab$
\item $\ssb$: vector of REML estimates of the elements of $\Sigmab=\var(Y)$
\end{itemize}

Kenward and Roger (1997)

* replaced $\bm \Phi$ by an improved small sample approximation $\bm \Phi_A$

* scaled $F$ by a factor $\lambda$

* determined denominator degrees of freedom $m$ by matching moments of $F/\lambda$ with an $F_{d,m}$ distribution.


## Kenward and Roger's modification

Kenward and Roger (1997) modify the test statistic
\begin{itemize}
\item $\bm \Phi$ is replaced by an improved small sample approximation $\bm \Phi_A$
\end{itemize}

Furthermore
\begin{itemize}
\item the statistic $F$ is scaled by a factor $\lambda$,
\item denominator degrees of freedom $m$ are determined
\end{itemize}
such that the approximate expectation and variance are those of a $F_{d,m}$ distribution.

## Restriction on covariance, some details

\begin{itemize}

\item Consider only situations where
\begin{gather*}
\Sigmab= \sum_i \sigma_i \bm G_i, \quad \bm G_i \, \text{known matrices}
\end{gather*}

\item Variance component and random coefficient models satisfy this
restriction.

\item $\bm \Phi_A(\ssb)$  depends now only on the first  partial derivatives of $\bm \Sigma^{-1}$:
\begin{displaymath}
\frac{\partial \bm \Sigmab^{-1}}{\partial \sigma_i} = - \bm \Sigma^{-1}
\frac{\partial \bm \Sigmab}{\partial \sigma_i}
\bm \Sigma^{-1}.
\end{displaymath}

\item $\bm \Phi_A(\ssb)$  depends also on $\var(\ssb)$.

\item Kenward and Roger propose to estimate
  $\var(\ssb)$ via the  inverse expected information matrix.
\end{itemize}



  
## Parametric bootstrap



<!-- We have two competing models; a large model $f_1(y; \theta)$
<!-- and a null model $f_0(y; \theta_0)$; the null model is a submodel of the large model. -->

* The $p$ value for a composite hypothesis is
$$
 p = \sup_{\theta \in \Theta_0} Pr_{\theta}(T \ge t_{obs})
$$
where the $\sup$ is taken under the hypothesis.

* We can (usually) not evaluate the $\sup$ in practice, so instead we do:
$$
 p^{PB} = Pr_{\hat\theta}(T \ge t_{obs})
$$

* In practice we approximate $p^{PB}$ as

	+ Draw $B$ parametric bootstrap samples
$t^1, \dots, t^B$ under the fitted null model $\hat \theta_0$.

	+ Fit the large and the null model to each of these datasets;

	+ Calculate the LR-test statistic for each simulated data; this
      gives reference distribution.

	+ Calculate how extreme the observed statistic is.

---

<!-- ```{r, cache=TRUE} -->
<!-- lg2 <- update(lg2, REML=FALSE) -->
<!-- sm2 <- update(sm2, REML=FALSE) -->
<!-- # Observed test statistic: -->
<!-- t.obs <- 2 * (logLik(lg2) - logLik(sm2)) -->
<!-- t.obs -->
<!-- # Reference distribution -->
<!-- set.seed(121315) -->
<!-- t.sim <- PBrefdist(lg2, sm2, nsim=2000) -->
<!-- # p-value -->
<!-- head(t.sim) -->
<!-- sum(t.sim >= t.obs) / length(t.sim) -->
<!-- # compare with X^2 dist -->
<!-- 1 - pchisq(t.obs, df=1) -->
<!-- ``` -->

<!-- --- -->

<!-- Interesting to overlay limiting $\chi^2_1$ -->
<!-- distribution and simulated reference distribution. -->

<!-- Bootstrap reference distribution has heavier tail giving larger $p$-value. -->

<!-- ```{r echo=FALSE, fig.width=4, fig.height=3} -->
<!-- t.sim2 <- t.sim[t.sim<10] -->
<!-- hist(t.sim2, breaks=40, prob=T, col="green") -->
<!-- abline(v=t.obs, col="red", lwd=3) -->
<!-- f <- function(x){dchisq(x, df=1)} -->
<!-- curve(f, 0, 20, add=TRUE, col="blue", lwd=2) -->
<!-- ``` -->



<!-- ## Speedup I: Sequential $p$-value -->
<!-- \label{sec:seqp} -->

<!--   Instead of simulating a fixed number of values $t^1, \dots, t^B$ for -->
<!--   determining the reference distribution used for finding $p^{PB}$ -->
<!--   we may instead introduce a stopping rule saying \emph{simulate until we -->
<!--   have found, say $h=20$ values $t^j$ larger than $t_{obs}$.} If $J$ -->
<!--   simulations are made then the reported $p$--value is $h/J$. -->

<!-- <\!--   --- -\-> -->

<!-- <\!-- The simulation of the reference distribution can be parallelized -\-> -->
<!-- <\!-- onto different processors (happens by default): -\-> -->


<!-- ```{r cache=TRUE} -->
<!-- spb <- seqPBmodcomp(lg2, sm2)  -->
<!-- spb -->
<!-- ``` -->

<!-- --- -->

<!-- ## Speedup II: Parallel computations -->

<!-- Parametric bootstrap is computationally demanding, but multiple cores can be exploited. Done by default on linux / mac platforms. -->

<!-- ```{r cache=TRUE} -->
<!-- PBmodcomp(lg2, sm2) # Default: Use all cores (4 on my computer) -->
<!-- PBmodcomp(lg2, sm2, cl=1) # Use one core -->
<!-- ``` -->

<!-- --- -->

<!-- On windows (in fact, work on all platforms): -->



<!-- ```{r, eval=FALSE} -->
<!-- set.seed(121315) -->
<!-- library(parallel) -->
<!-- nc <- detectCores(); nc -->
<!-- clus <- makeCluster(rep("localhost", nc)) -->
<!-- PBmodcomp(lg2, sm2, cl=clus) -->
<!-- ``` -->




<!-- ## Speedup III: Parametric form of reference distribution: -->

<!--   Estimating tail--probabilities will require more samples than -->
<!--   estimating the mean (and variance) of the reference -->
<!--   distribution. -->

<!-- Suggests to approximate simulated reference distribution with a known distribution so that fewer samples will suffice: -->

<!-- ```{r cache=TRUE} -->
<!-- pb1 <- PBmodcomp(lg2, sm2, nsim=1000) -->
<!-- pb2 <- PBmodcomp(lg2, sm2, nsim=100) -->
<!-- summary(pb1) %>% as.data.frame -->
<!-- summary(pb2) %>% as.data.frame -->
<!-- ``` -->


<!-- ## Why use parametric bootstrap -->

<!-- * Applies generally; in `pbkrtest` implemented for e.g. generalized -->
<!--   linear mixed models (hwere random effects are on the linear -->
<!--   predictor scale). -->

<!-- * Kenward-Roger does not readily scale to larger problems because of the computation of -->
<!--   \begin{displaymath} -->
<!--     G_j {\bm \Sigma}^{-1} G_j -->
<!--   \end{displaymath} -->
<!--   where $\Sigmab= \sum_i \sigma_i \bm G_i$. -->
<!--   Can be space and time consuming! -->

<!-- * For example, in random regression models with few relatively long -->
<!-- time series. In this case simulation is faster. -->




<!-- ## Simulation study -->


<!-- ```{r} -->
<!-- dub -->
<!-- ``` -->

<!-- --- -->

<!-- * Task: Test the hypothesis that there is no effect of treatment. How -->
<!--   good are the various tests? -->

<!-- * Simulate data $1000$ times with divine insight: there is no effect of treatment. -->

<!-- * Test the hypothesis e.g. at level $5\%$. If test -->
<!--   has correct nominal level we shall reject about $50$ times. -->

<!-- * If hypothesis is rejected e.g. $100$ times then $p$ values are -->
<!--   anti-conservative: Effects appear more significant than the really -->
<!--   are. That is we draw "too strong" conclusions. -->


<!-- ```{r echo=FALSE} -->
<!-- sim<-list(c(0.11, 0.24, 0.322), c(0.178, 0.282, 0.342), c(0.044, 0.152,  -->
<!-- 0.24), c(0.012, 0.044, 0.114), c(0.008, 0.052, 0.108)) -->
<!-- sim <- do.call(rbind, sim) -->
<!-- ## 1: lnm + F -->
<!-- ## 2: lnm + X^2 -->
<!-- ## 3: mixed + X^2 -->
<!-- ## 4: mixed + KR -->
<!-- ## 5: mixed + LR -->

<!-- rownames(sim) <- -->
<!--     c("lm+F", "lm+X2", "mixed+X2", "mixed+F-KR", "mixed+PB") -->
<!-- colnames(sim) <- c("0.010", "0.050", "0.100") -->
<!-- sim <- sim[c(2,1,3,4,5),] -->

<!-- ``` -->

<!-- ```{r echo=FALSE} -->
<!-- kable(sim) -->
<!-- ``` -->




<!-- ## Motivation: Sugar beets - A split--plot experiment -->

<!-- * Model how sugar percentage in sugar beets depends on -->
<!--    harvest time and sowing time. -->
<!-- * Five sowing times ($s$) and two harvesting times ($h$). -->
<!-- * Experiment was laid out in three blocks ($b$). -->


<!-- Experimental plan for sugar beets experiment -->


<!-- <\!-- \begin{Verbatim}[fontsize=\tiny] -\-> -->

<!-- <\!-- Sowing times: -\-> -->
<!-- <\!--  1: 4/4, 2: 12/4, 3: 21/4, 4: 29/4, 5: 18/5 -\-> -->
<!-- <\!-- Harvest times: -\-> -->
<!-- <\!--  1: 2/10, 2: 21/10 -\-> -->

<!-- <\!-- Plot allocation: -\-> -->
<!-- <\!--       |  Block 1           |  Block 2           |  Block 3           | -\-> -->
<!-- <\!--       +--------------------|--------------------|--------------------+ -\-> -->
<!-- <\!-- Plot  | h1  h1  h1  h1  h1 | h2  h2  h2  h2  h2 | h1  h1  h1  h1  h1 | Harvest time -\-> -->
<!-- <\!-- 1-15  | s3  s4  s5  s2  s1 | s3  s2  s4  s5  s1 | s5  s2  s3  s4  s1 | Sowing time -\-> -->
<!-- <\!--       |--------------------|--------------------|--------------------| -\-> -->
<!-- <\!-- Plot  | h2  h2  h2  h2  h2 | h1  h1  h1  h1  h1 | h2  h2  h2  h2  h2 | Harvest time -\-> -->
<!-- <\!-- 16-30 | s2  s1  s5  s4  s3 | s4  s1  s3  s2  s5 | s1  s4  s3  s2  s5 | Sowing time -\-> -->
<!-- <\!--       +--------------------|--------------------|--------------------+ -\-> -->
<!-- <\!-- \end{Verbatim} -\-> -->


<!-- <\!-- Plot allocation: -\-> -->
<!-- <\!--       |  Block 1           |  Block 2           |  Block 3           | -\-> -->
<!-- <\!--       +--------------------|--------------------|--------------------+ -\-> -->
<!-- <\!-- Plot  | h1  h1  h1  h1  h1 | h2  h2  h2  h2  h2 | h1  h1  h1  h1  h1 | Harvest time -\-> -->
<!-- <\!-- 1-15  | s3  s4  s5  s2  s1 | s3  s2  s4  s5  s1 | s5  s2  s3  s4  s1 | Sowing time -\-> -->
<!-- <\!--       |--------------------|--------------------|--------------------| -\-> -->
<!-- <\!-- Plot  | h2  h2  h2  h2  h2 | h1  h1  h1  h1  h1 | h2  h2  h2  h2  h2 | Harvest time -\-> -->
<!-- <\!-- 16-30 | s2  s1  s5  s4  s3 | s4  s1  s3  s2  s5 | s1  s4  s3  s2  s5 | Sowing time -\-> -->
<!-- <\!--       +--------------------|--------------------|--------------------+ -\-> -->
<!-- <\!-- \end{Verbatim} -\-> -->






<!-- ```{r size="scriptsize"} -->
<!-- # Plot allocation: -->
<!-- #       |  Block 1       |  Block 2       |  Block 3       | -->
<!-- #       +----------------|----------------|----------------+ -->
<!-- # Plot  | h1 h1 h1 h1 h1 | h2 h2 h2 h2 h2 | h1 h1 h1 h1 h1 | Harvest time -->
<!-- # 1-15  | s3 s4 s5 s2 s1 | s3 s2 s4 s5 s1 | s5 s2 s3 s4 s1 | Sowing time -->
<!-- #       |----------------|----------------|----------------| -->
<!-- # Plot  | h2 h2 h2 h2 h2 | h1 h1 h1 h1 h1 | h2 h2 h2 h2 h2 | Harvest time -->
<!-- # 16-30 | s2 s1 s5 s4 s3 | s4 s1 s3 s2 s5 | s1 s4 s3 s2 s5 | Sowing time -->
<!-- #       +----------------|----------------|----------------+ -->
<!-- ``` -->






<!-- ## beets data -->

<!-- ```{r} -->
<!-- data(beets, package='pbkrtest') -->
<!-- head(beets) -->
<!-- ``` -->



<!-- ```{r, fig.height=3} -->
<!-- par(mfrow=c(1,2)) -->
<!-- with(beets, interaction.plot(sow, harvest, sugpct)) -->
<!-- with(beets, interaction.plot(sow, harvest, yield)) -->
<!-- ``` -->

<!-- --- -->

<!-- * For simplicity assume  no interaction between sowing -->
<!--     and harvesting times. -->

<!-- * A typical model for such an experiment would be: -->
<!--     \begin{equation} -->
<!--       \label{eq:beetsmodel1} -->
<!--       y_{hbs} = \mu + \alpha_h + \beta_b + \gamma_s + U_{hb} + \epsilon_{hbs}, -->
<!--     \end{equation} -->
<!--     where $U_{hb} \sim N(0,\omega^2)$ and $\epsilon_{hbs}\sim -->
<!--     N(0,\sigma^2)$. -->
    

<!-- * Notice that $U_{hb}$ describes the random variation -->
<!-- between whole--plots (within blocks). -->



<!-- --- -->

<!-- Using `lmer()` from lme4 we can -->
<!--  test for no effect of sowing and harvest time as: -->


<!-- ```{r} -->
<!-- beet.lg <- lmer(sugpct ~ block + sow + harvest +  -->
<!--                       (1 | block:harvest), data=beets, REML=FALSE) -->
<!-- beet.noh <- update(beet.lg, .~. - harvest) -->
<!-- beet.nos  <- update(beet.lg, .~. - sow) -->
<!-- ``` -->

<!-- Both factors appear highly significant -->
<!-- ```{r} -->
<!-- anova(beet.lg, beet.noh)  %>% as.data.frame -->
<!-- anova(beet.lg, beet.nos)  %>% as.data.frame -->
<!-- ``` -->

<!-- However, the LRT based $p$--values are anti--conservative: the effect -->
<!-- of harvest appears stronger than it is. -->


<!-- --- -->

<!-- As the design is balanced we may make F--tests for each of the effects -->
<!-- as: -->

<!-- ```{r} -->
<!-- beets$bh <- with(beets, interaction(block, harvest)) -->
<!-- summary(aov(sugpct ~ block + sow + harvest +  -->
<!--                 Error(bh), data=beets)) -->
<!-- ``` -->

<!-- Notice: the F--statistics are $F_{1,2}$ for harvest time and $F_{4,20}$ for -->
<!-- sowing time. -->

<!-- --- -->

<!-- ```{r} -->
<!-- set.seed("260618") -->
<!-- KRmodcomp(beet.lg, beet.noh) -->
<!-- PBmodcomp(beet.lg, beet.noh) -->

<!-- ``` -->

<!-- ```{r} -->
<!-- seqPBmodcomp(beet.lg, beet.noh) -->
<!-- ``` -->


<!-- ## Final remarks -->

<!-- * Satterthwaite approximation of degrees of freedom on its way in -->
<!--   `pbkrtest`. Faster to compute than Kenward-Roger scales to larger -->
<!--   problems. -->

<!-- * `pbkrtest` available on CRAN [https://cran.r-project.org/package=pbkrtest](https://cran.r-project.org/package=pbkrtest) -->

<!-- * devel version on github: `devtools::install_github(hojsgaard/pbkrtest)` -->

<!-- * `pbkrtest` described in Ulrich Halekoh and SH (2014) [A Kenward-Roger Approximation and Parametric Bootstrap Methods for Tests in Linear Mixed Models The R Package pbkrtest](https://www.jstatsoft.org/article/view/v059i09); Journal of Statistical Software, Vol 59. -->



<!-- Thanks for your attention! -->

























<!-- ```{r include=FALSE} -->
<!-- PBmodcomp(lg2, sm2) # Default: Use all cores (4 on my computer) -->
<!-- PBmodcomp(lg2, sm2, cl=1) # Use one core -->
<!-- ``` -->

<!-- On windows (in fact, work on all platforms): -->

<!-- ```{r, eval=FALSE} -->
<!-- set.seed(121315) -->
<!-- library(parallel) -->
<!-- nc <- detectCores(); nc -->
<!-- clus <- makeCluster(rep("localhost", nc)) -->
<!-- PBmodcomp(lg2, sm2, cl=clus) -->
<!-- ``` -->


<!-- ```{r} -->
<!-- pb1 <- PBmodcomp(lg2, sm2, nsim=999) -->
<!-- pb2 <- PBmodcomp(lg2, sm2, nsim=99) -->
<!-- summary(pb1) %>% as.data.frame -->
<!-- summary(pb2) %>% as.data.frame -->
<!-- ``` -->
