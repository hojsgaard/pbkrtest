## == __Rmarkup Example 2__ ==
## === Søren Højsgaard ===
## %%date

## In this example we embed parts of the examples from
## the &&kruskal.test&& help page into an HTML document:

##@@
data ( airquality )
kruskal.test ( Ozone ~ Month , data = airquality )
##@

## which shows that the location parameter of the Ozone
## distribution varies significantly from month to month.
##
## Notice that there are __\Sexpr{nrow(airquality)}__
## observations.
##
## Finally we include a boxplot of the data :

##@@@
boxplot ( Ozone ~ Month , data = airquality )
##@



