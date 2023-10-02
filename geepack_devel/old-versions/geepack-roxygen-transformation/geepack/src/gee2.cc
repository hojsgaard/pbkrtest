// using namespace std;

//  #include "tntsupp.h"
//  #include "geese.h"

//  extern "C"{
//  #include <R.h>
//  #include <Rmath.h>
//  #include <Rdefines.h>
//  }

//  #include "famstr.h"
//  #include "param.h"
//  #include "inter.h"
//  #include "utils.h"
//  #include "geesubs.h"
#include "gee2.h"

IVector comp_lev(GeeStr &geestr, Corr &cor) {
  IVector level(2);
  if (geestr.ScaleFix() != 1) level(1) = 1;
  if (cor.nparam() > 0) level(2) = 1;
  return level;
}

DMatrix gee_infls(DVector &Y, DMatrix &X, 
		  DVector &Offset, DVector &Doffset, DVector &W,
		  IVector &LinkWave, 
		  DMatrix &Zsca, DMatrix &Zcor, DVector &CorP, 
		  IVector &Clusz,  
		  GeeStr &geestr, Corr &cor, GeeParam &par, Control &con) {
  Hess Hi(par), H(par); Grad Gi(par);

  int n = Clusz.size();
  IVector ZcorSize(n);
  //if (cor.nparam() > 1) 
  if (cor.corst() > AR1) // == UNSTRUCTRUED || USERDEFINED || FIXED
    for (int i = 1; i <= n; i++) 
      ZcorSize(i) = Clusz(i) * (Clusz(i) - 1) / 2;
  else ZcorSize = 1;

  IVector level(2); level = 0;
  if (geestr.ScaleFix() != 1) level(1) = 1;
  if (cor.nparam() > 0) level(2) = 1;
  
  int p = par.p(), q = par.q(), r = par.r();
  DMatrix L11(p,p), L12(p,r), L13(p,q), L22(r,r), L23(r,q), L33(q,q); 
  int l = p + q + r;
  DMatrix infls(l, n), HH(l, l);

  Index1D I(0,0), J(0,0);
  Index1D I1(0, 0), JJ(0, 0), I2(0, 0), I3(0, 0);
  I1 = Index1D(1, p);
  I2 = Index1D(p + 1, p + r);
  I3 = Index1D(p + r + 1, p + r + q);
  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i), s2 = ZcorSize(i), crs = s1 * (s1 - 1) / 2;;
    I = Index1D(1, s1) + I.ubound();
    if (s2 > 0) J = Index1D(1, s2) + J.ubound();
    DVector PRi(s1), Vi(s1), V_Mui(s1); DMatrix Di(s1,p);
    gee_prep(Y, X, Offset, I, LinkWave, par, geestr, PRi, Di, Vi, V_Mui);
    DVector Phii(s1); DMatrix D2i(s1, r);
    PhiandD2(I, LinkWave, Doffset, Zsca, par, geestr, Phii, D2i);
    DMatrix R(s1, s1), E(crs, q);
    RandE(Zcor, I, J, CorP, par, geestr, cor, R, E);
    //cout << "i = " << i;
    DVector Wi = asVec(VecSubs(W, I));
    HiandGi(PRi, Phii, Di, R, Vi, V_Mui, D2i, E, Wi, level, Hi, Gi);
    //cout << "Hi = " << Hi; cout << "H = " << H;
    //cout << "Gi = " << Gi;
    H.inc(Hi); 
    JJ = Index1D(i, i);
    infls(I1, JJ) = asColMat(Gi.U1());
    if (level(1) == 1)  infls(I2, JJ) = asColMat(Gi.U2());
    if (level(2) == 1)  infls(I3, JJ) = asColMat(Gi.U3());
  }
  Hess Hinv = inv(H, level);
  I1 = Index1D(1, p);
  HH(I1, I1) = Hinv.A();
  if (level(1) == 1) {
    HH(I2, I1) = Hinv.B();
    HH(I2, I2) = Hinv.C();
  }
  if (level(2) == 1) {
    HH(I3, I1) = Hinv.D();
    HH(I3, I3) = Hinv.F();
    if (level(1) == 1) HH(I3, I2) = Hinv.E();
  }
  infls = HH * infls;
  return infls;
}


void gee_var(DVector &Y, DMatrix &X, 
	     DVector &Offset, DVector &Doffset, DVector &W,
	     IVector &LinkWave, 
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP, 
	     IVector &Clusz, IVector &ZcorSize, 
	     GeeStr &geestr, Corr &cor, GeeParam &par, Control &con) {
  Hess Hi(par), H(par); Grad Gi(par);

  IVector level(2); level = 0;
  if (geestr.ScaleFix() != 1) level(1) = 1;
  if (cor.nparam() > 0) level(2) = 1;
  
  int p = par.p(), q = par.q(), r = par.r();
  DMatrix L11(p,p), L12(p,r), L13(p,q), L22(r,r), L23(r,q), L33(q,q); 

  Index1D I(0,0), J(0,0);
  for (int i = 1; i <= Clusz.size(); i++) {
    int s1 = Clusz(i), s2 = ZcorSize(i), crs = s1 * (s1 - 1) / 2;;
    I = Index1D(1, s1) + I.ubound();
    if (s2 > 0) J = Index1D(1, s2) + J.ubound();
    DVector PRi(s1), Vi(s1), V_Mui(s1); DMatrix Di(s1,p);
    gee_prep(Y, X, Offset, I, LinkWave, par, geestr, PRi, Di, Vi, V_Mui);
    DVector Phii(s1); DMatrix D2i(s1, r);
    PhiandD2(I, LinkWave, Doffset, Zsca, par, geestr, Phii, D2i);
    DMatrix R(s1, s1), E(crs, q);
    RandE(Zcor, I, J, CorP, par, geestr, cor, R, E);
    //cout << "i = " << i;
    DVector Wi = asVec(VecSubs(W, I));
    HiandGi(PRi, Phii, Di, R, Vi, V_Mui, D2i, E, Wi, level, Hi, Gi);
    //cout << "Hi = " << Hi; cout << "H = " << H;
    //cout << "Gi = " << Gi;
    H.inc(Hi); 
    L11 = L11 + outerprod(Gi.U1());
    if (level(1) == 1) {
      L12 = L12 + outerprod(Gi.U1(), Gi.U2());
      L22 = L22 + outerprod(Gi.U2());
    }
    if (level(2) == 1) {
      L13 = L13 + outerprod(Gi.U1(), Gi.U3());
      L33 = L33 + outerprod(Gi.U3());
      if (level(1) == 1) L23 = L23 + outerprod(Gi.U2(), Gi.U3());
    }
  }
  //Vbeta:
  Hess Hinv = inv(H, level);
  par.set_vbeta_naiv(Hinv.A());
  par.set_vbeta(Hinv.A() * L11 * Hinv.A());

  //Vgamma:
  if (level(1) == 1) {
    par.set_vgamma((Hinv.B() * L11 + Hinv.C() * transpose(L12))  
		   * transpose(Hinv.B()) + 
		   (Hinv.B() * L12 + Hinv.C() * L22) * Hinv.C());
  }

  //Valpha:
  if (level(2) == 1) {
    par.set_valpha_naiv(Hinv.F());
    par.set_valpha_stab(Hinv.F() * L33 * Hinv.F());
    par.set_valpha((Hinv.D() * L11 + Hinv.E() * transpose(L12) + 
		    Hinv.F() * transpose(L13)) * transpose(Hinv.D()) + 
		   (Hinv.D() * L12 + Hinv.E() * L22 + 
		    Hinv.F() * transpose(L23)) * transpose(Hinv.E()) +
		   (Hinv.D() * L13 + Hinv.E() * L23 + 
		    Hinv.F() * L33) * Hinv.F());
  }
}


double update_beta(DVector &Y, DMatrix &X, DVector &Offset, 
		   DVector &W, DVector &Phi,
		   IVector &LinkWave, DVector &CorP,
		   DMatrix &Zcor,  IVector &Clusz,
		   IVector &ZcorSize, IVector &Jack, 
		   GeeParam &par, GeeStr &geestr, Corr &cor) {
  double del = 0;
  //  DVector alp = par.alpha();
  int p = par.p(); 
  DMatrix H(p,p); DVector G(p);
  int n = Clusz.size();
  Index1D I(0,0), J(0,0);
  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i), s2 = ZcorSize(i);
    I = Index1D(1, s1) + I.ubound(); 
    if (s2 > 0) J = Index1D(1, s2) + J.ubound(); //?? what is s2 == 0 ??
    if (Jack(i) == 1) continue;
    DVector PRi(s1); DMatrix Di(s1,p);
    PRandD(Y, X, Offset, I, LinkWave, par, geestr, PRi, Di);
    DVector rootInvPhii = sqrt(recip(asVec(VecSubs(Phi, I))));
    DVector rootWi = sqrt(asVec(VecSubs(W, I)));
    Di = SMult(rootWi, Di); PRi = SMult(rootWi, PRi);
    Di = SMult(rootInvPhii, Di); PRi = SMult(rootInvPhii, PRi);
    DMatrix R = getR(Zcor, I, J, CorP, par, geestr, cor);
    H = H + AtBiC(Di, R, Di);
    G = G + AtBiC(Di, R, PRi);
  }
  DVector Del = solve(H, G);
  DVector Bnew = par.beta() + Del;
  while (1) {
    //    cerr << "in updating beta: " << "Del = " << Del << endl;
    DVector Eta = X * Bnew + Offset;
    DVector Mu = geestr.MeanLinkinv(Eta, LinkWave);
    if (geestr.validMu(Mu, LinkWave)) break;
    Del = 0.5 * Del;
    Bnew = par.beta() + Del;
  }
  par.set_beta(Bnew);
  del = fmax(fabs(Del));
  return del;
}

double update_gamma(DVector &PR, DVector &W, IVector &LinkWave,
		    IVector &Clusz, IVector &Jack,
		    DVector &Doffset, DMatrix &Zsca, 
		    GeeParam &par, GeeStr &geestr) {
  double del = 0;
  int r = par.r(), n = Clusz.size();
  //  double adj = (double) (PR.size()) / (double)(PR.size() - par.p());
  if (geestr.ScaleFix() == 1) return del; 
  DMatrix H(r,r); DVector G(r);
  Index1D I(0,0);
  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i);
    I = Index1D(1, s1) + I.ubound();
    if (Jack(i) == 1) continue;
    DVector Phii(s1), Si(s1); DMatrix D2i(s1, r);
    gm_prep(PR, I, LinkWave, Doffset, Zsca, par, geestr, Phii, Si, D2i);

    //DMatrix V2 = diag(2.0 * Phii); 
    //independence working structure only now, so no inverting below
    DVector WiV2inv = SMult(asVec(VecSubs(W, I)), recip(2.0 * Phii));
    H = H + Transpose_view(D2i) * SMult(WiV2inv, D2i);
    G = G + Transpose_view(D2i) * SMult(WiV2inv, Si - Phii); //adj * Si
    //H = H + AtBiC(D2i, WiV2, D2i);
    //G = G + AtBiC(D2i, WiV2, Si - Phii);
  }
  DVector Del = solve(H, G);
  //cout << "H = " << H << "G = " << G;
  //par.set_gamma((double) N / (double)(N - p) * (par.gamma() + Del));
  par.set_gamma(par.gamma() + Del);
  del = fmax(fabs(Del));
  return del;
}

double update_alpha(DVector &PR, DVector &Phi, DVector &CorP, DVector &W,
		    IVector &Clusz, IVector &ZcorSize, IVector &Jack,
		    DMatrix &Zcor, 
		    GeeParam &par, GeeStr &geestr, Corr &cor) {
  double del = 0;
  int q = par.q(), n = Clusz.size();
  if (cor.nparam() == 0) return del;
  DMatrix H(q,q); DVector G(q);
  Index1D I(0,0), J(0,0);
  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i), s2 = ZcorSize(i), crs = s1 * (s1 - 1) / 2;
    I = Index1D(1, s1) + I.ubound(); 
    if (s2 > 0) J = Index1D(1, s2) + J.ubound();
    if (Jack(i) == 1) continue;
    if (s1 == 1) continue;

    DVector PRi = asVec(VecSubs(PR, I));
    DVector Phii = asVec(VecSubs(Phi, I));
    DVector sPRi = SMult(reciproot(Phii), PRi);
    DVector zi = genzi(sPRi);
    
    DMatrix R(s1, s1), E(crs, q);
    RandE(Zcor, I, J, CorP, par, geestr, cor, R, E);
    DVector rhoi = utri(R);
    DVector Wi = asVec(VecSubs(W, I));
    //DMatrix V3 = diag(genzi(rootWi)); 
    //independence working correlation only now, no need of inverting below
    DVector WiV3inv = genzi(Wi);
    H = H + Transpose_view(E) * SMult(WiV3inv, E);
    G = G + Transpose_view(E) * SMult(WiV3inv, zi - rhoi);
    //H = H + AtBiC(E, V3, E);
    //G = G + AtBiC(E, V3, zi - rhoi);
  }
  DVector Del = solve(H, G);
  par.set_alpha(par.alpha() + Del);
  del = fmax(fabs(Del));
  return del;
}

/*********************************************************
Input:
  Y: response vector;
  X: covariate matrix for mean structure;
  LinkWave: determines which link to apply on each response component;
  Weight: weight, to be implemented ... ... ???;
  Offset: offset, to be implemented ... ... ???;
  Zsca: covariate matrix for scale structure;
  Zcor: covariate matrix for correlation structure;
  Corp: correlation parameters to feed cor.mat(rho, .), can be distances for spatial correlation; it is now a vector, which can not really handle >=2 spatial correlations; it really should be a matrix which contains the data to feed cor.mat(rho, .); it actually is the same as LinkWave now, but should be more general to contain high dimensional data, such as coordinates in R x R.
  Clusz: cluster sizes;
  ZcorSize: number of rows in Zcor for each cluster;
  geestr: GEE structure, contains links, variances for each wave;
  cor: correlation structure;
  par: parameter values;
  Jack: Jackknife indicator;
  con: control parameters: ScaleFix, ajs, j1s, fij, tol, maxiter;
*********************************************************/
void gee_est(DVector &Y, DMatrix &X, 
	     DVector &Offset, DVector &Doffset, DVector &W, 
	     IVector &LinkWave,
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	     IVector &Clusz, IVector &ZcorSize,
	     GeeStr &geestr, Corr &cor, GeeParam &par,
	     IVector &Jack, Control &con) {
  DVector Del(3); 

  int N = Y.size();
  DVector PR(N), Phi(N);
  
  int iter; double del;
  for (iter = 0; iter < con.maxiter(); iter++) {
    if (con.trace() == 1) {
      //cerr << "iter " << iter << endl;
      //cerr << "beta = " << par.beta() << "gamma = " << par.gamma() << "alpha = " << par.alpha();
      Rprintf("iter = %d\n", iter);
      Rprintf("beta = "); VecPrint(par.beta()); 
      Rprintf("gamma = "); VecPrint(par.gamma());
      Rprintf("alpha = "); VecPrint(par.alpha());
    }
    //updating beta;
    Phi = getPhi(Doffset, Zsca, LinkWave, par, geestr);
    Del(1) = update_beta(Y, X, Offset, W, Phi, LinkWave, CorP, Zcor, Clusz, ZcorSize, Jack, par, geestr, cor);

    //updating gamma;
    PR = getPR(Y, X, Offset, LinkWave, par, geestr);
    //cout << "PR = " << PR;
    //PR = (double) (N / (N - p)) * PR; //df adjusting
    Del(2) = update_gamma(PR, W, LinkWave, Clusz, Jack, Doffset, Zsca, par, geestr);

    //updating alpha;
    Phi = getPhi(Doffset, Zsca, LinkWave, par, geestr);
    Del(3) = update_alpha(PR, Phi, CorP, W, Clusz, ZcorSize, Jack, Zcor, par, geestr, cor);

    del = fmax(Del);
    if (del <= con.tol()) break;
  }
  if (iter == con.maxiter()) par.set_err(1);
}

void getJackVar(Vector<DVector> &beta_i, Vector<DVector> &alpha_i,
		Vector<DVector> &gamma_i, GeeParam &par, 
		int jack) { //jack = 1, 2, 3 for ajs, j1s, fij
  int I = beta_i.size(), p = par.p(), q = par.q(), r = par.r();
  DMatrix vb(p,p), va(q,q), vc(r,r);
  //cout << par.beta();
  for (int i = 1; i <= I; i++) {
    //cout << "i = " << i << " " << beta_i(i);
    vb = vb + outerprod(beta_i(i) - par.beta());
    //can use level as in gee2_var
    va = va + outerprod(alpha_i(i) - par.alpha());
    vc = vc + outerprod(gamma_i(i) - par.gamma());
  }
    
  double f = (double) (I - p - q - r) / I;
  if (jack == 3) {//fij
    par.set_vbeta_fij(f * vb);
    par.set_valpha_fij(f * va);
    par.set_vgamma_fij(f * vc);
  }
  else if (jack == 2) { //j1s
    par.set_vbeta_j1s(f * vb);
    par.set_valpha_j1s(f * va);
    par.set_vgamma_j1s(f * vc);
  }
  else {//ajs
    par.set_vbeta_ajs(f * vb);
    par.set_valpha_ajs(f * va);
    par.set_vgamma_ajs(f * vc);
  }
}

void gee_jack(DVector &Y, DMatrix &Xmat, DVector &Offset, DVector &Doffset,
	      DVector &W, IVector &LinkWave, DMatrix &Zsca, DMatrix &Zcor,
	      DVector &CorP, IVector &Clusz, IVector &ZcorSize,
	      GeeStr &geestr, Corr &cor,
	      GeeParam &par, Control &con) {
  int I = Clusz.size();
  //  int p = par.p(), q = par.q(), r = par.r();
  IVector Jack(I);
  Vector<DVector> beta_i(I), alpha_i(I), gamma_i(I);
  Vector<DVector> beta_fi(I), alpha_fi(I), gamma_fi(I);
  //DVector b0(p), a0(q), c0(r);
  //beta_i = b0; alpha_i(I) = a0; gamma_i(I) = c0;
  //beta_fi = b0; alpha_fi(I) = a0; gamma_fi(I) = c0;
  Control con1(con); con1.set_maxiter(1); //for j1s

  for (int i = 1; i <= I; i++) {
    Jack(i) = 1;
    GeeParam par_i(par.beta(), par.alpha(), par.gamma());
    if (con.j1s() == 1) {
      gee_est(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par_i, Jack, con1); //1-step
      beta_i(i) = par_i.beta();
      alpha_i(i) = par_i.alpha();
      gamma_i(i) = par_i.gamma();
    }

    if (con.fij() == 1) {
      gee_est(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par_i, Jack, con); //full iterated
      beta_fi(i) = par_i.beta();
      alpha_fi(i) = par_i.alpha();
      gamma_fi(i) = par_i.gamma();
    }
    Jack(i) = 0;
  }
  if (con.j1s() == 1) getJackVar(beta_i, alpha_i, gamma_i, par, 2);
  if (con.fij() == 1) getJackVar(beta_fi, alpha_fi, gamma_fi, par, 3);
}

void jack_ajs(DVector &Y, DMatrix &X, DVector &Offset, DVector &Doffset,
	      DVector &W, IVector &LinkWave, DMatrix &Zsca, DMatrix &Zcor,
	      DVector &CorP, IVector &Clusz, IVector &ZcorSize,
	      GeeStr &geestr, Corr &cor,
	      GeeParam &par, Control &con) {//con is not used
  int I = Clusz.size(), p = par.p(), q = par.q(), r = par.r();
  Vector<Hess> His(I); Vector<Grad> Gis(I);
  IVector level = comp_lev(geestr, cor), Scur(Y.size()); Scur = 1;
  HisandGis(Y, X, Offset, Doffset, W, LinkWave, Clusz, ZcorSize,
	    Zsca, Zcor, CorP, par, geestr, cor, Scur, level,
	    His, Gis);
  Hess Hn(par); 
  for (int i = 1; i <= I; i++) Hn.inc(His(i));

  Vector<DVector> beta_i(I), alpha_i(I), gamma_i(I);
  DVector b0(p), a0(q), c0(r);
  beta_i = b0; alpha_i(I) = a0; gamma_i(I) = c0;

  DMatrix vb(p,p), va(q,q), vc(r,r);
  for (int i = 1; i <= I; i++) {
    Hess H_i = Hn - His(i);
    H_i = inv(H_i, level);
    beta_i(i) = H_i.A() * Gis(i).U1();
    gamma_i(i) = H_i.B() * Gis(i).U1() + H_i.C() * Gis(i).U2();
    alpha_i(i) = H_i.D() * Gis(i).U1() + H_i.E() * Gis(i).U2() + H_i.F() * Gis(i).U3();
    vb = vb + outerprod(beta_i(i));
    //can use level as in gee2_var
    va = va + outerprod(alpha_i(i));
    vc = vc + outerprod(gamma_i(i));
  }
  double f = (double) (I - p - q - r) / I;
  par.set_vbeta_ajs(f * vb);
  par.set_valpha_ajs(f * va);
  par.set_vgamma_ajs(f * vc);
}

void gee_top(DVector &Y, DMatrix &Xmat, 
	     DVector &Offset, DVector &Doffset, DVector &W, 
	     IVector &LinkWave,
	     DMatrix &Zsca, DMatrix &Zcor, DVector &CorP,
	     IVector &Clusz, 
	     GeeStr &geestr, Corr &cor, GeeParam &par,
	     Control &con) {
  int I = Clusz.size();
  IVector Jack(I), ZcorSize(I);
  //initializing ZcorSize
  //if (cor.nparam() > 1) 
  if (cor.corst() > AR1) // == UNSTRUCTRUED || USERDEFINED || FIXED
    for (int i = 1; i <= I; i++) 
      ZcorSize(i) = Clusz(i) * (Clusz(i) - 1) / 2;
  else ZcorSize = 1;

  gee_est(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par, Jack, con);

  gee_var(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par, con);
 
  if (con.ajs() == 1) 
    jack_ajs(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par, con);

  if (con.j1s() + con.fij() > 0) 
    gee_jack(Y, Xmat, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, ZcorSize, geestr, cor, par, con);
}

extern "C" {
  SEXP gee_rap(SEXP y, SEXP x, SEXP offset, SEXP doffset, SEXP w,
	       SEXP linkwave, SEXP zsca, SEXP zcor, SEXP corp,
	       SEXP clusz, SEXP geestr, SEXP cor, SEXP par, SEXP con) {
    DVector Y = asDVector(y), Offset = asDVector(offset), Doffset = asDVector(doffset), W = asDVector(w);
    IVector LinkWave = asIVector(linkwave); 
    DVector CorP = asDVector(corp); 
    DMatrix X = asDMatrix(x), Zsca = asDMatrix(zsca), Zcor = asDMatrix(zcor);
    IVector Clusz = asIVector(clusz); // ZcorSize = asIVector(zcorsize);
    Control Con = asControl(con);   
    GeeParam Par = asGeeParam(par);   
    GeeStr Geestr = asGeeStr(geestr);   
    Corr Cor = asCorr(cor);   

    gee_top(Y, X, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, Geestr, Cor, Par, Con);
    SEXP ans = asSEXP(Par);
    return ans;
  }

  /* return the influence functions for parameters */
  SEXP infls_rap(SEXP y, SEXP x, SEXP offset, SEXP doffset, SEXP w,
		 SEXP linkwave, SEXP zsca, SEXP zcor, SEXP corp,
		 SEXP clusz, SEXP geestr, SEXP cor, SEXP par, SEXP con) {
    DVector Y = asDVector(y), Offset = asDVector(offset), Doffset = asDVector(doffset), W = asDVector(w);
    IVector LinkWave = asIVector(linkwave); 
    DVector CorP = asDVector(corp); 
    DMatrix X = asDMatrix(x), Zsca = asDMatrix(zsca), Zcor = asDMatrix(zcor);
    IVector Clusz = asIVector(clusz); // ZcorSize = asIVector(zcorsize);
    Control Con = asControl(con);   
    GeeParam Par = asGeeParam(par);   
    GeeStr Geestr = asGeeStr(geestr);   
    Corr Cor = asCorr(cor);   

    DMatrix infls = gee_infls(Y, X, Offset, Doffset, W, LinkWave, Zsca, Zcor, CorP, Clusz, Geestr, Cor, Par, Con);
    SEXP ans = asSEXP(infls);
    return ans;
  }
}
