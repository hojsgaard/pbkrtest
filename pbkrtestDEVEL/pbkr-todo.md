## TODOs for pbkrtest
1. Get vcov(beta) from Hessian. There is already vcovAdj() based on
   KR. Need to get it for SAT also.

1. Contact Rune; make him contributor

1. Include gls-code from github contributor Donohue. Make him contributor.

1. How hard is it to get SAT to work for gls/lme? Maybe make Donohue do it.

1. In all `XXmodcomp` functions: smallModel (silly name) can be model object or restriction matrix. Allow also for formula.
   Either `. ~ . - a-b` or `~ - a - b` or c("a", "b") Then it must be checked that the new model is submodel of the old.

1. restrictionMatrix2model is unbearable (and unreadable). Write Lmatrix2model, L_matrix2model or L2model instead.

1. How to check that a matrix is a restriction matrix? Check that it has full rank?

