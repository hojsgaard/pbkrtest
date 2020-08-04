source("ymc/R/ymc.R")
ymc()

## Copy into ymc
In> 0+x;
Out> x;
In> x+1*y;
Out> x+y;
In> Sin(ArcSin(alpha))+Tan(ArcTan(beta));
Out> alpha+beta;
In> (x+y)^3-(x-y)^3
Out> (x+y)^3-(x-y)^3;
In> Simplify(%)
Out> 6*x^2*y+2*y^3;


In> Taylor(x,0,3) Exp(x)
Out> 1+x+(1/2)*x^2+(1/6)*x^3;
In> PrettyForm(%);

