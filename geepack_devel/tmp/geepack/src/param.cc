//using namespace std;

#include "tntsupp.h"
#include "geese.h"
#include "param.h"

//class Control
Control::Control(int trace, int ajs, int j1s, int fij, 
		 int maxiter, double tol) :
  _trace(trace), _ajs(ajs), _j1s(j1s), _fij(fij),
  _maxiter(maxiter), _tol(tol){}
Control::Control(int *con, double tol) {
  _trace = con[0]; _ajs = con[1]; _j1s = con[2]; _fij = con[3];
  _maxiter = con[4]; _tol = tol;
}
Control::Control(const Control &C) :
  //{
  _trace(C.trace()), _ajs(C.ajs()), _j1s(C.j1s()),
  _fij(C.fij()), _maxiter(C.maxiter()), _tol(C.tol()) {}
  // _trace = C.trace(); _ajs = C.ajs(); _j1s = C.j1s();
  //_fij = C.fij(); _maxiter = C.maxiter(); _tol = C.tol();
  //}

//class GeeParam
GeeParam::GeeParam(DVector Beta, DVector Alpha, DVector Gamma):
    _beta(Beta), _alpha(Alpha), _gamma(Gamma), _err(0) {
    int p = Beta.size(), q = Alpha.size(), r = Gamma.size();
    DMatrix vb(p,p), va(q,q), vg(r,r);
    _vbeta = vb; _vbeta_naiv = vb; _vbeta_ajs = vb; _vbeta_j1s = vb; _vbeta_fij = vb;
    _valpha = va; _valpha_naiv = va; _valpha_ajs = va; _valpha_j1s = va; _valpha_fij = va; _valpha_stab = va;
    _vgamma = vg; _vgamma_ajs = vg; _vgamma_j1s = vg; _vgamma_fij = vg;
}

GeeParam::GeeParam(DVector Beta, DVector Alpha, DVector Gamma,
	DMatrix VBeta, DMatrix VBeta_naiv, 
	DMatrix VBeta_ajs, DMatrix VBeta_j1s, 
	DMatrix VBeta_fij,
	DMatrix VAlpha, DMatrix VAlpha_stab,
	DMatrix VAlpha_naiv, DMatrix VAlpha_ajs, 
	DMatrix VAlpha_j1s, DMatrix VAlpha_fij,
	DMatrix VGamma, DMatrix VGamma_ajs, 
	DMatrix VGamma_j1s, DMatrix VGamma_fij):
  _beta(Beta),
  _alpha(Alpha),
  _gamma(Gamma),
  _vbeta(VBeta), _vbeta_naiv(VBeta_naiv),
  _vbeta_ajs(VBeta_ajs), _vbeta_j1s(VBeta_j1s),
  _vbeta_fij(VBeta_fij),
  _valpha(VAlpha), _valpha_stab(VAlpha_stab), 
  _valpha_naiv(VAlpha_naiv), _valpha_ajs(VAlpha_ajs), 
  _valpha_j1s(VAlpha_j1s), _valpha_fij(VAlpha_fij),
  _vgamma(VGamma),
  _vgamma_ajs(VGamma_ajs), _vgamma_j1s(VGamma_j1s),
  _vgamma_fij(VGamma_fij) {}
/*
  GeeParam(int p, int q, double *beta, double *alpha, 
	double *vbeta, double *vbeta_naiv,
	double *valpha, double *valpha_stab, double *valpha_naiv) {
    _beta(beta, p);
    _alpha(alpha, q);
    _vbeta(vbeta, p, p);

  }
*/
