## Letter of rebeuttal for the manuscript "Computer Algebra in R Bridges a Gap Between Mathematics and Data in the Teaching of Statistics and Data Science"

## By Mikkel Meyer Andersen and Søren Højsgaard

First of all we wish to thank the reviewer for very useful comments
and constructive suggestions. Our replies to specific points are
marked by ">>>>" below.



## Referee report of "Computer Algebra in R Bridges a Gap Between Mathematics and Data in the Teaching of Statistics and Data Science"

- **Id**: {{id}}

## General

The article presents the caracas package for symbolic mathematics and
illustrates its usage via several examples.  The package enhances the
capability of R to deal with symbolic operations and will certainly be
of interest to the R community.  However, I am concerned about
implementation choices and the article's contribution. Specifically,
the package seems to be a wrapper for reticulate + sympy, and the
wrapper seems less general and less efficient than the direct usage of
sympy via reticulate.  In short, the added value of caracas is
unclear. I would recommend either:

1) Explain why the package redefines (a subset of) functions exposed
   by sympy and the advantages of using them instead of calling sympy
   via reticulate. Do the wrappers provide additional functionalities?
   What is the difference with sympy + reticulate?  From my
   understanding, the package's contribution seems to redefine the
   sympy syntax to make it more R-friendly. But this comes at the cost
   of less generality and confusion when comparing the documentation
   at https://r-cas.github.io/caracas/ with https://docs.sympy.org

OR

2) Directly expose the handle to sympy (get_sympy()) and illustrate
   its usage in R. Refer the users to https://docs.sympy.org for the
   complete documentation of all available functions. Drop the caracas
   functions that do not provide additional functionalities.

Unless I have missed important points in the added value of the
wrappers defined by caracas, I believe that using the sympy handle
directly would make caracas easier to use, easier to document, and
easier to maintain as it would automatically include new potential
functionalities that sympy may provide in the future.

>>>> We have included a short section which illustrates the handle to
>>>> sympy via get_sympy(), and we mention that there is also the
>>>> sympy_func() which provides access to sympy functions not already
>>>> interfaced in caracas. 

>>>> However, caracas is more than an interface to sympy; caracas also
>>>> interfaces the yacas program via the Ryacas package. This is
>>>> exploited e.g. in the inv() function for finding the inverse of a
>>>> matrix. The implementation in yacas is faster than the
>>>> implementation in sympy, and the yacas implementation is the
>>>> default method used in inv().


Ryacas, ease-of-use, extendible, ...



## Detailed comments

The main question I have is:

1) What is the advantage of using the wrappers provided by caracas instead of using the sympy handle via reticulate?

>>>> For technically minded users, it is probably no big deal to also
>>>> learn python, for others it might be. caracas bridges gap between
>>>> symbolic and numerical computations. caracas also draws on Ryacas
>>>> and can be made to draw on other packages as well.

Other points include:

2) Installing caracas also requires installing python, sympy, and other dependencies that R users may not be used to. 
   The authors did an excellent job of simplifying the installation. I would recommend including a section 
   (e.g., after the introduction) that describes how to set up caracas.

>>>> Thanks, we have done so.

3) There is a mismatch between sympy/caracas behavior for derivatives. For instance:

# load caracas
library(caracas)

# get the sympy engine via reticulate
sympy <- reticulate::import("sympy")

# sympy gives the derivative D_x D_y f 
sympy$diff("sin(x*y)", "x", "y")
> -x*y*sin(x*y) + cos(x*y)

# caracas gives the gradient [D_x f, D_y f]
def_sym(x, y)
der(sin(x*y), c(x, y))
> [y⋅cos(x⋅y)  x⋅cos(x⋅y)]

Although this may be intended, it makes it difficult to use caracas or sympy interchangeably.

>>>> Vi skal måske bruge gradient of hessian sproget i stedet for???

4) Higher-order derivatives do not seem to be provided by caracas. Iterating "caracas::der" several times is orders of magnitude slower 
   than using the sympy handle.

# load microbenchmark
library(microbenchmark)

# n-th derivative via caracas
derN <- function(expr, vars, n){
  for(i in 1:n) expr <- der(expr, vars)
  return(expr)
}

# benchmark
microbenchmark(
  sympy$diff("sin(x*y)", "x", 100L),
  derN(sin(x*y), x, 100L)
)
> Unit: microseconds
                              expr       min         lq       mean     median        uq       max neval
 sympy$diff("sin(x*y)", "x", 100L)   758.787   796.1995   898.6581   951.6305   982.934  1112.084   100
         derN(sin(x * y), x, 100L) 53200.288 54060.1195 56225.6620 54821.7150 58087.755 80522.483   100

5) I recommend illustrating the use of the sympy handle as the main object in the paper. E.g.:

# solve a parametrized integral and evaluate in a=1
i <- sympy$integrate("a*x", c("x", -1, 2))
i$evalf(subs = list(a=1))

# or use the function "latex" for printing in LaTeX
sympy$latex(...)

>>>> Indeed a good idea. Thanks

## Code

Usability:

- Does the package have a clear and user-friendly workflow?

The workflow may be improved by using the sympy handle directly and maintaining only caracas functions 
with additional functionalities. This is already possible with the function "caracas::get_sympy()" but it is not
discussed in the paper.

>>>> We have expanded the paper to meed this suggestion.

- Is the documentation well-written and user-focused?

The documentation is well written but significantly less comprehensive than https://docs.sympy.org

>>>> Thanks

Code:

- Are there obvious edge cases that are missed?

Several functionalities of sympy are not exported ("wrapped") by
caracas and using the sympy handle is not illustrated in the paper.

>>>> Some cunning reply...

- Could the functions presented be improved? 

The current version makes it difficult to understand which functions
do provide added value and which are simple wrappers to sympy.  I
recommend describing caracas functions that provide additional
functionalities in detail and using the sympy handle otherwise.

>>>> Good point 

- Is the code well-organized and understandable?

The code is well organized.
            
>>>> Thanks
