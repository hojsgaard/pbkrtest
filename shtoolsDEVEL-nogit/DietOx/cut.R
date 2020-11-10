
\newslide
\subsubsection{Figures}
\shfig{dietox04}{XXX}{}


\newslide
\subsubsection{Adding a quadratic and cubic term}

There is still some curvature in the residuals. One solution to this is to add
quadratic (and possibly cubic) terms to the model:

$$
y_{cpt} = \alpha+ \beta t+ {\color{red}\gamma t^2 + \delta t^3} + \alpha_c + \beta_c t 
+{\color{red}\gamma_c t^2} +U_{cp}+ W_{cp} t + e_{cpt}; \ \ 
%e_{cpt}\sim N(0,\sigma_e^2), U_{cp}\sim N(0,\sigma_U^2), W_{cp}\sim N(0,\sigma_W^2), 
$$
Written shortly as:
\begin{eqnarray*}
[y] &=& 1 + \underline{time}+ {\color{red}\underline{time}^2+ \underline{time}^3} + Cu + 
Cu * \underline{time} + {\color{red}Cu * \underline{time}^2+Cu *
  \underline{time}^3} \\
&& + [Cu*Pig] + 
[Cu*Pig]*\underline{time}+[e]  
\end{eqnarray*}
$$
$$

\newslide
\subsubsection{In \SAS\ and \R}

\begin{verbatim}
proc mixed data=dietox noinfo noclprint;
  class cu pig timec;
  model weight = time time*time time*time*time cu 
       cu*time cu*time*time cu*time*time*time / 
       solution htype=1;
  random int time/ subject=pig;
run;
\end{verbatim}

@ 
<<>>=
fm3 <- lme(Weight ~ Cu *(Time + I(Time^2) + I(Time^3)),
                  data = dietox, random = ~ 1 + Time| Pig)
@ %def 

\newslide
\subsubsection{Figures}

%\shfig{dietox05}{XXX}{}
\shfig{dietox06}{XXX}{}
These residuals look fairly reasonable!

\newslide
\subsubsection{The results}

Stepwise addition of terms can be accomplished using the \texttt{anova}
function: 
@ 
<<>>=
anova(fm3)
@ %def 

There is an effect of Cu on the quadratic Time term, \texttt{$Cu:I(Time^2)$}.


\subsection{Updating models}

The term \texttt{$Cu:I(Time^3)$} is not statistically significant, and can safely
be eliminated from the model. 

A very easy way to accomplish this is to update the model object as follows:
@ 
<<>>=
fm32 <- update(fm3, . ~ . - Cu:I(Time^3))
anova(fm32)
@ %def 

\newslide

\subsection{Parameter estimates}
@ 
<<>>=
round(summary(fm32)$tTable,3)
@ %def 


\newslide
\subsection{Estimating contrasts}

The estimated difference (gain) in going from Cu=1 (no copper) to Cu=3 (high
level) at time $t$ is then 
$$
Cu3 + Cu3:Time * t + Cu3:I(Time^2)*t^2
$$

This is achieved by multiplying the regression coefficients by the vector
@ 
<<>>=
time<-2
L <- c(0,0,1,0,0,0,0,time,0,time^2)
L
@ %def 
and summing the results afterwards
@ 
<<>>=
sum(fm32$coef$fixed*L)
@ %def 

\newslide
However, such an estimate is not of much interest unless one can obtain the
standard error, which can be achieved using the \texttt{esticon} function:
@ 
<<>>=
esticon(fm32, L)
@ %def 

\newslide
It may be of interest to calculate this estimate for a variety of different $t$
values and to plot these together with a confidence interval,
Figure~\ref{fig:dietox01pred}: 
@ 
<<>>=
e <- NULL
for (time in 1:12){
    L <- c(0,0,1,0,0,0,0,time,0,time^2)
    e <- rbind(e, esticon(fm32, L))
    }

plot(1:12,e$Estimate,type="l",ylim=c(-5,7),col="red",lwd=2)
abline(h=0)
lines(1:12,e$Estimate+2*e$Std.Error,type="l",col="green",lwd=2)
lines(1:12,e$Estimate-2*e$Std.Error,type="l",col="green",lwd=2)
@ %def 


\shfig{dietox01pred}{XXX}{height=5cm}

Statistical significance and practical significance is two different things.

Whether this effect is of any practical relevance is a good question!
