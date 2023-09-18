## == __Rmarkup Example 1__ ==
## === Søren Højsgaard ===
## %%date

## In this example we embed parts of the examples from the
## &&kruskal.test&& help page into a LaTeX document :

##<<>>=
data ( airquality )
kruskal.test ( Ozone ~ Month , data = airquality )
##@

##        @@
kruskal.test ( Ozone ~ Month , data = airquality )
##@

#@@ 
kruskal.test ( Ozone ~ Month , data = airquality )
##@

## 
# @@ 
kruskal.test ( Ozone ~ Month , data = airquality )
##@

##  <<>>=
kruskal.test ( Ozone ~ Month , data = airquality )
# @

##@@
kruskal.test ( Ozone ~ Month , data = airquality )
## @




## which shows that the location parameter of the Ozone
## distribution varies significantly from month to month. Finally we
## include a boxplot of the data :

#@@@
boxplot ( Ozone ~ Month , data = airquality )
##@

##@@@
boxplot ( Ozone ~ Month , data = airquality )
##@

#   @@@
boxplot ( Ozone ~ Month , data = airquality )
##@

##  @@@
boxplot ( Ozone ~ Month , data = airquality )
##@



