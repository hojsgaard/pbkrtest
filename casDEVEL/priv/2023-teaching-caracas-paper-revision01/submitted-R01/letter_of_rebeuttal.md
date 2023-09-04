## Letter of rebeuttal for the manuscript "Computer Algebra in R Bridges a Gap Between Mathematics and Data in the Teaching of Statistics and Data Science"

## By Mikkel Meyer Andersen and Søren Højsgaard

First of all we wish to thank the reviewer for very useful comments
and constructive suggestions. The suggestions and criticisms have
certainly helped us improve the manuscript.

Our replies to specific points are
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

>>>> Using reticulate and sympy directly is relatively easy for single operations, 
>>>> but it is fairly cumbersome to chain together operations from within R as
>>>> it would most likely require much text string manipulation. 
>>>> Our key contributions is to facilitate a session of operations on the 
>>>> objects at a higher abstraction level (without text string manipulation). 
>>>> This gives access to e.g. intermediate results (e.g. for printing 
>>>> or other usage) and then being able to continue computation on these 
>>>> results.

>>>> We have included some of the above in the paper's abstract and 
>>>> introduction.

>>>> We have included a short section which illustrates the handle to
>>>> sympy via get_sympy(), and we also illustrates  the
>>>> sympy_func() which provides access to sympy functions not already
>>>> interfaced in caracas. This allows  a community of users to
>>>> contribute to the continued development of caracas.

>>>> A minor aspect of the fact that caracas is more than an interface
>>>> to SymPy is that caracas also interfaces the yacas program via
>>>> the Ryacas package. This is exploited e.g. in the inv() function
>>>> for finding the inverse of a matrix. The implementation in yacas
>>>> is in some cases faster than the implementation in SymPy. 




## Detailed comments

The main question I have is:

1) What is the advantage of using the wrappers provided by caracas
instead of using the sympy handle via reticulate?

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

>>>> We appreciate the reviewer's input and we have elaborated on this
>>>> in one of the final sections of the manuscript. To be specific, 
>>>> SymPy has chosen to have to different entry points for
>>>> differentiation. (1) One for functions of one variable (diff) which
>>>> allows for efficient computation of higher order derivatives [1]. (2) One
>>>> for functions of several variables (derive_by_array) [2]. We have chosen
>>>> one entry point (which detects SymPy function to use), namely 
>>>> to specify the variables and the only take the derivative once. 
>>>> We believe this is easier for the typical R-user, but it is a 
>>>> design choice (like SymPy's) that comes at the price that it can not 
>>>> currently exploit the SymPy approach with diff. 
>>>> 
>>>> We will demonstrate though below, that caracas is extensible 
>>>> (e.g. with `sympy_func()`), and we have illustrated this explicitly 
>>>> in the article.
>>>>
>>>> [1]: https://docs.sympy.org/latest/tutorials/intro
>>>> [2]: https://docs.sympy.org/latest/modules/tensor/array.html-tutorial/calculus.html
>>>> 
>>>> library(caracas)
>>>> sympy <- get_sympy()
>>>> sympy$diff("sin(x * y)", "x", "x")
>>>> def_sym(x, y)
>>>> sin(x * y) |> der(x) |> der(x)
>>>> ## New variant
>>>> der_diff <- function(expr, ...){
>>>> 	 sympy_func(expr, "diff", ...)
>>>> }
>>>> der_diff(sin(x * y), x, x)

4) Higher-order derivatives do not seem to be provided by caracas. Iterating "caracas::der" several times is orders of magnitude slower 
   than using the sympy handle.


# load microbenchmark
library(microbenchmark)

# n-th derivative via caracas
derN <- function(expr, vars, n){
  for(i in 1:n) expr <- der(expr, vars)
  return(expr)
}

>>>> The previous benchmark:
> Unit: microseconds
                              expr       min         lq       mean     median        uq       max neval
 sympy$diff("sin(x*y)", "x", 100L)   758.787   796.1995   898.6581   951.6305   982.934  1112.084   100
         derN(sin(x * y), x, 100L) 53200.288 54060.1195 56225.6620 54821.7150 58087.755 80522.483   100

>>>> Again, like above, we can implement our own
>>>> der_diff(sin(x * y), x, "100")
>>>> # Another variant than above:
>>>> derN2 <- function(expr, vars, n){
>>>>   sympy_func(expr, "diff", vars, as.character(n))
>>>> }
>>>> derN2(sin(x * y), x, 100)
>>>> New benchmark
>>>> microbenchmark(
>>>>   sympy$diff("sin(x*y)", "x", 100L),
>>>>   derN(sin(x*y), x, 100L),
>>>>   derN2(sin(x * y), x, 100L)
>>>> )
>>>> 
>>>> Unit: milliseconds
>>>>                               expr   min    lq  mean median    uq    max neval cld
>>>>  sympy$diff("sin(x*y)", "x", 100L)  1.48  1.59  1.74   1.72  1.84   2.87   100  a 
>>>>          derN(sin(x * y), x, 100L) 85.62 89.14 94.46  90.28 95.26 266.87   100   b
>>>>         derN2(sin(x * y), x, 100L)  1.80  2.01  2.28   2.16  2.29  10.76   100  a 
>>>> 
>>>> As seen, the two are comparable.




5) I recommend illustrating the use of the sympy handle as the main object in the paper. E.g.:

# solve a parametrized integral and evaluate in a=1
i <- sympy$integrate("a*x", c("x", -1, 2))
i$evalf(subs = list(a=1))

# or use the function "latex" for printing in LaTeX
sympy$latex(...)

>>>> Indeed a good idea. We have done so in one of the last sections of the manuscript.

## Code

Usability:

- Does the package have a clear and user-friendly workflow?

The workflow may be improved by using the sympy handle directly and maintaining only caracas functions 
with additional functionalities. This is already possible with the function "caracas::get_sympy()" but it is not
discussed in the paper.

>>>> We have expanded the paper to meet this suggestion.

- Is the documentation well-written and user-focused?

The documentation is well written but significantly less comprehensive than https://docs.sympy.org

>>>> Thanks

Code:

- Are there obvious edge cases that are missed?

Several functionalities of sympy are not exported ("wrapped") by
caracas and using the sympy handle is not illustrated in the paper.

>>>> As mentioned above, we have provided the sympy_func function for
>>>> extending caracas and we hope to recieve contributions from caracas users.

- Could the functions presented be improved? 

The current version makes it difficult to understand which functions
do provide added value and which are simple wrappers to sympy.  I
recommend describing caracas functions that provide additional
functionalities in detail and using the sympy handle otherwise.

>>>> As argued above, we believe that usage of SymPy directly via text
>>>> strings etc. is a useful option (which we have illustrated) but
>>>> also that ease of use is greatly enhanced by our approach. 

- Is the code well-organized and understandable?

The code is well organized.
            
>>>> Thanks
