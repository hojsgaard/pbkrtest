using namespace std;

#include "tntsupp.h"
#include "geese.h"

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

#include "famstr.h"
#include "param.h"
#include "inter.h"


DMatrix asDMatrix(SEXP a) {
  double *x;
  x = NUMERIC_POINTER(AS_NUMERIC(a));
  int *dims = INTEGER_POINTER(AS_INTEGER(GET_DIM(a)));
  DMatrix ans(dims[0], dims[1], x);
  return ans;
}

DVector asDVector(SEXP a) {
  double *x;
  x = NUMERIC_POINTER(AS_NUMERIC(a));
  int len = GET_LENGTH(a);
  DVector ans(len, x);
  return ans;
}

IVector asIVector(SEXP a) {
  int *x;
  x = INTEGER_POINTER(AS_INTEGER(a));
  int len = GET_LENGTH(a);
  IVector ans(len, x);
  return ans;
}

Vector<DVector> asVDVector(SEXP a) {//a is a matrix
  double *x;
  x = NUMERIC_POINTER(AS_NUMERIC(a));
  int *dims = INTEGER_POINTER(AS_INTEGER(GET_DIM(a)));
  Vector<DVector> ans(dims[1]);
  for (int i = 1; i <= ans.size(); i++) {
    DVector tmp(dims[0], x);
    ans(i) = tmp;
    x += dims[0];
  }
  return ans;
}

SEXP asSEXP(const DMatrix &a) {
  int size = a.num_cols() * a.num_rows();

  SEXP val;
  PROTECT(val = NEW_NUMERIC(size));
  double *p = NUMERIC_POINTER(val);
  const double *q = a.begin();
  for (int i = 0; i < size; i++) p[i] = q[i];
  //  SET_CLASS(val, ScalarString(mkChar("matrix")));

  SEXP dim;
  PROTECT(dim = NEW_INTEGER(2));
  INTEGER(dim)[0] = a.num_rows(); INTEGER(dim)[1] = a.num_cols();
  SET_DIM(val, dim);

  UNPROTECT(2);
  return val;
}

SEXP asSEXP(const DVector &a) {
  int size = a.size();
  SEXP val;
  PROTECT(val = NEW_NUMERIC(size));
  double *p = NUMERIC_POINTER(val);
  const double *q = a.begin();
  for (int i = 0; i < size; i++) p[i] = q[i];
  //  SET_CLASS(val, ScalarString(mkChar("vector")));

  SEXP len;
  PROTECT(len = NEW_INTEGER(1));
  INTEGER(len)[0] = size;
  SET_LENGTH(val, size);
  UNPROTECT(2);
  return val;
}

SEXP asSEXP(const IVector &a) {
  int size = a.size();
  SEXP val;
  PROTECT(val = NEW_INTEGER(size));
  int *p = INTEGER_POINTER(val);
  const int *q = a.begin();
  for (int i = 0; i < size; i++) p[i] = q[i];
  //  SET_CLASS(val, ScalarString(mkChar("vector")));

  SEXP len;
  PROTECT(len = NEW_INTEGER(1));
  INTEGER(len)[0] = size;
  SET_LENGTH(val, size);
  UNPROTECT(2);
  return val;
}


Control asControl(SEXP con) {
  //con is a list of trace, jack, j1s, fij, maxiter, epsilon
  int trace, jack, j1s, fij, maxiter;
  double tol;
  trace = INTEGER(VECTOR_ELT(con, 0))[0];
  jack = INTEGER(VECTOR_ELT(con, 1))[0];
  j1s = INTEGER(VECTOR_ELT(con, 2))[0];
  fij = INTEGER(VECTOR_ELT(con, 3))[0];
  maxiter = INTEGER(VECTOR_ELT(con, 4))[0];
  tol = REAL(VECTOR_ELT(con, 5))[0];
  Control Con(trace, jack, j1s, fij, maxiter, tol);
  return Con;
}

GeeParam asGeeParam(SEXP par) {
  //par is a list of beta, alpha, gamma;
  DVector Beta = asDVector(VECTOR_ELT(par, 0));
  DVector Alpha = asDVector(VECTOR_ELT(par, 1));
  DVector Gamma = asDVector(VECTOR_ELT(par, 2));
  GeeParam Par(Beta, Alpha, Gamma);
  return Par;
}

GeeStr asGeeStr(SEXP geestr) {
  //geestr is a list of maxwave, meanlink, v, scalelink, corrlink, scale.fix;
  int maxwave = INTEGER(AS_INTEGER(VECTOR_ELT(geestr, 0)))[0];
  IVector MeanLink = asIVector(AS_INTEGER(VECTOR_ELT(geestr, 1)));
  IVector V = asIVector(AS_INTEGER(VECTOR_ELT(geestr, 2)));
  IVector ScaleLink = asIVector(AS_INTEGER(VECTOR_ELT(geestr, 3)));
  int corrlink = INTEGER(AS_INTEGER(VECTOR_ELT(geestr, 4)))[0];
  int scalefix = INTEGER(AS_INTEGER(VECTOR_ELT(geestr, 5)))[0];
  GeeStr G(maxwave, MeanLink, V, ScaleLink, corrlink, scalefix);
  return G;
}

Corr asCorr(SEXP cor) {
  //cor is a list of corst, maxwave
  int corstr, maxwave;
  corstr = INTEGER(VECTOR_ELT(cor, 0))[0];
  maxwave = INTEGER(VECTOR_ELT(cor, 1))[0];
  Corr Cor(corstr, maxwave);
  return Cor;
}

SEXP asSEXP(GeeParam &Par) {
  SEXP ans;
  PROTECT(ans = NEW_LIST(19));
  SET_VECTOR_ELT(ans, 0, asSEXP(Par.beta()));
  SET_VECTOR_ELT(ans, 1, asSEXP(Par.alpha()));
  SET_VECTOR_ELT(ans, 2, asSEXP(Par.gamma()));
  SET_VECTOR_ELT(ans, 3, asSEXP(Par.vbeta()));
  SET_VECTOR_ELT(ans, 4, asSEXP(Par.valpha()));
  SET_VECTOR_ELT(ans, 5, asSEXP(Par.vgamma()));
  SET_VECTOR_ELT(ans, 6, asSEXP(Par.vbeta_naiv()));
  SET_VECTOR_ELT(ans, 7, asSEXP(Par.valpha_naiv()));
  SET_VECTOR_ELT(ans, 8, asSEXP(Par.valpha_stab()));
  SET_VECTOR_ELT(ans, 9, asSEXP(Par.vbeta_ajs()));
  SET_VECTOR_ELT(ans, 10, asSEXP(Par.valpha_ajs()));
  SET_VECTOR_ELT(ans, 11, asSEXP(Par.vgamma_ajs()));
  SET_VECTOR_ELT(ans, 12, asSEXP(Par.vbeta_j1s()));
  SET_VECTOR_ELT(ans, 13, asSEXP(Par.valpha_j1s()));
  SET_VECTOR_ELT(ans, 14, asSEXP(Par.vgamma_j1s()));
  SET_VECTOR_ELT(ans, 15, asSEXP(Par.vbeta_fij()));
  SET_VECTOR_ELT(ans, 16, asSEXP(Par.valpha_fij()));
  SET_VECTOR_ELT(ans, 17, asSEXP(Par.vgamma_fij()));

  IVector Err(1); Err(1) = Par.err();
  SET_VECTOR_ELT(ans, 18, asSEXP(Err));
  UNPROTECT(1);
  return ans;
}
