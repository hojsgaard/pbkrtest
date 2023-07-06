Computer Algebra Systems in R
Mikkel Meyer Andersen and Søren Højsgaard
Department of Mathematical Sciences, Aalborg University

---

R's ability to do symbolic mathematics is largely restricted to
finding derivatives.  There are many tasks involving symbolic math
that are of interest to R users, e.g. inversion of symbolic matrices,
limits and solving non-linear equations.  Users must resort to other
computer algebra systems (CAS) for such tasks and many R users
(especially outside of academia) do not readily have access to such
software.  There are also other indirect use-cases of symbolic
mathematics in R that can exploit other strengths of R, including
Shiny apps with auto-generated mathematics exercises.

We maintain two R-packages that enable symbolic mathematics in R:
Ryacas and caracas.  Ryacas is based on Yacas (Yet Another Computer
Algebra System) and caracas is based on SymPy (Python library).  Each
have their advantages: Yacas is extensible and has a close integration
to R which makes auto-generated mathematics exercises easy to make.
SymPy is feature-rich and thus gives many possibilities.

In this talk we will discuss the two packages and demonstrate various
use-cases including uses that help understanding statistical models
and Shiny apps with auto-generated mathematics exercises.
