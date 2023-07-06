---
output: pdf_document
---

# Installation instructions

See <https://github.com/r-cas/caracas>. Brief version:
You need Python on your system. When you have that, ´caracas` is available on CRAN and can be installed as usual:

```r
install.packages('caracas')

# If SymPy not installed yet, first time it must be installed:
if (!caracas::has_sympy()) {
  caracas::install_sympy() 
}
```

Now, you can proceed, e.g.:

```r
library(caracas)
def_sym(x) ## Declares 'x' as a symbol
p <- 1 - x^2 + x^3 + x^4/4 - 3 * x^5 / 5 + x^6 / 6
p
```

This should work without errors.

