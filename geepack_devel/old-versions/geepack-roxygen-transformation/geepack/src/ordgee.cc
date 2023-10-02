// using namespace std;

//  #include "tnt/region1d.h"
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
#include "ordgee.h"

double odds2p11(double psi, double mu1, double mu2) {
  if (fabs(psi - 1.0) < .001) return mu1 * mu2;
  else {
    double exp1 = 1 + (mu1 + mu2) * ( psi - 1);
    double s = exp1 * exp1 + 4 * psi * (1 - psi) * mu1 * mu2;
    s = sqrt(s);
    return .5 / (psi - 1) * (exp1 - s);
  }
}

DMatrix odds2p11(DVector &Psi, DVector &Mu1, DVector &Mu2) {
  //Psi is c^2 by 1, contains odds ratio in the sequence: 1-1, 1-2, 2-1, 2-2
  //which can be viewed as the result of hvec of PSI matrix
  int c = Mu1.size(), k = 1;
  DMatrix ans(c, c);
  for (int i = 1; i <= c; i++)
    for (int j = 1; j <= c; j++)
      ans(i, j) = odds2p11(Psi(k++), Mu1(i), Mu2(j)); 
  return ans;
}

/*  get derivative from R function
f <- deriv( ~ .5 / (psi - 1) * (1 + (mu1 + mu2) * ( psi - 1) - (((1 + (mu1 + mu2) * ( psi - 1))^ 2 + 4 * psi * (1 - psi) * mu1 * mu2))^.5), c("psi", "mu1", "mu2"), function(psi, mu1, mu2){})
*/

double p11_odds(double psi, double mu1, double mu2) {
  if (fabs(psi - 1.0) < .001) return mu1*mu2*( - (mu1 + mu2) + mu1*mu2 + 1);
  else {
    double expr1 = psi - 1.0;
    double expr2 = .5 / expr1;
    double expr3 = mu1 + mu2;
    double expr5 = 1 + expr3 * expr1;
    double expr7 = 4 * psi;
    double expr8 = 1 - psi;
    double expr9 = expr7 * expr8;
    double expr10 = expr9 * mu1;
    double expr12 = pow(expr5, 2.0) + expr10 * mu2;
    double expr14 = expr5 - pow(expr12, 0.5);
    double expr23 = pow(expr12, -0.5);
    //double expr33 = 2 * (expr1 * expr5);
    //.value <- .expr2 * .expr14
    double ans = expr2 * (expr3 - 0.5 * ((2 * (expr3 * expr5) + (4 * expr8 - expr7) * mu1 * mu2) * expr23)) - 0.5/pow(expr1, 2.0) * expr14;
    return ans;
  }
}

DVector p11_mu(double psi, double mu1, double mu2) {
  DVector ans(2);
  if (fabs(psi - 1.0) < .001) {
    ans(1) = mu2; ans(2) = mu1;
    return ans;
  }
  double expr1 = psi - 1.0;
  double expr2 = .5 / expr1;
  double expr3 = mu1 + mu2;
  double expr5 = 1 + expr3 * expr1;
  double expr7 = 4 * psi;
  double expr8 = 1 - psi;
  double expr9 = expr7 * expr8;
  double expr10 = expr9 * mu1;
  double expr12 = pow(expr5, 2.0) + expr10 * mu2;
  //  double expr14 = expr5 - pow(expr12, 0.5);
  double expr23 = pow(expr12, -0.5);
  double expr33 = 2 * (expr1 * expr5);
  //  .grad[, "mu1"] <- .expr2 * (.expr1 - 0.5 * ((.expr33 + .expr9 *  mu2) * .expr23))
  ans(1) = expr2 * (expr1 - 0.5 * ((expr33 + expr9 *  mu2) * expr23));
  //  .grad[, "mu2"] <- .expr2 * (.expr1 - 0.5 * ((.expr33 + .expr10) * .expr23))
  ans(2) = expr2 * (expr1 - 0.5 * ((expr33 + expr10) * expr23));
  return ans;
}

DVector p11_odds(DVector &Psi, DVector &Mu1, DVector &Mu2) {
  //Mu1 and Mu2 are c x 1, Psi is c^2 x 1
  int c = Mu1.size(), k = 1;
  DVector ans(c * c);
  for (int i = 1; i <= c; i++) 
    for (int j = 1; j <= c; j++) {
      ans(k) = p11_odds(Psi(k), Mu1(i), Mu2(j)); 
      //need more attention to the ordering of Mu1 and Mu2, row-major or col-major; this is row major!!!
      k++;
    }
  return ans;
}

/*
DMatrix Vijj(DVector &Mu) {
  int c = Mu.size();
  DMatrix ans(c,c);
  for (int i = 1; i <= c; i++) 
    for (int j = 1; j <= c; j++) 
      ans(i,j) = Mu(fmax(i, j)) - Mu(i) * Mu(j);
  return ans;
}
*/

DMatrix Vijj(DVector &Mu, bool rev) {
  //rev = false: usual cumulated ordering; 
  //rev = true: Heagerty and Zeger (1996)
  int c = Mu.size(), ij;
  DMatrix ans(c,c);
  for (int i = 1; i <= c; i++) 
    for (int j = 1; j <= c; j++) {
      if (rev) ij = fmax(i, j);
      else ij = fmin(i, j);
      ans(i,j) = Mu(ij) - Mu(i) * Mu(j);
    }
  return ans;
}

DMatrix Vijk(DVector &Mu1, DVector &Mu2, DVector &Psi) {
  //Psi is a c^2 by 1 vector;
  int c = Mu1.size();
  DMatrix ans(c,c);
  int k = 1;
  for (int i = 1; i <= c; i++) {
    for (int j = 1; j <= c; j++) {
      double psi = Psi(k++);
      double p11 = odds2p11(psi, Mu1(i), Mu2(j));
      ans(i, j) = p11 - Mu1(i) * Mu2(j);
    }
  }
  return ans;
}

DMatrix getU3_Beta(DVector &Mu1, DVector &Mu2, DVector &Psi,
		   DMatrix &D1, DMatrix &D2, 
		   DVector &PR1, DVector &PR2) {
  int c = Mu1.size(), p = D1.num_cols();
  DMatrix ans(c * c, p);
  int k = 1;
  for (int i = 1; i <= c; i++) {
    DMatrix D1i = asMat(MatRow(D1,i));
    for (int j = 1; j <= c; j++) {
      DMatrix D2j = asMat(MatRow(D2,j));
      double psi = Psi(k);
      DVector P11_Mu = p11_mu(psi, Mu1(i), Mu2(j));
      P11_Mu(1) = P11_Mu(1) - Mu2(j); P11_Mu(2) = P11_Mu(2) - Mu1(i);
      //MatRow(ans, k) = P11_Mu(1) * D1i + P11_Mu(2) * D2j;
      MatRow(ans, k) = (- PR2(j) - P11_Mu(1)) * D1i + 
	(-PR1(i) - P11_Mu(2)) * D2j;
      k++;
    }
  }
  return ans;
}

DMatrix ord2V1(DVector &Mu, DVector &Psi, int clusz, bool rev) {
  //Mu is (ni*c) x 1, Psi is (choose(ni,2)*c^2) * 1  
  //This function should be named as ord2V1 instead of ord_V1, since it is forming V1 rather than taking derivatives
  int c = Mu.size() / clusz;
  DMatrix ans(Mu.size(), Mu.size());
  Index1D I(0,0), K(0,0);
  for (int i = 1; i <= clusz; i++) {
    I = Index1D(1, c) + I.ubound();
    Index1D J = I;
    DVector Mui = asVec(VecSubs(Mu, I));
    ans(I, I) = Vijj(Mui, rev);
    for (int j = i + 1; j <= clusz; j++) {
      J = Index1D(1, c) + J.ubound();
      DVector Muj = asVec(VecSubs(Mu, J));
      K = Index1D(1, c*c) + K.ubound();
      DVector Psik = asVec(VecSubs(Psi, K));
      ans(I, J) = Vijk(Mui, Muj, Psik);
      ans(J, I) = ans(I, J);
    }
  }
  return ans;
}

DMatrix Mu2V1(DVector &Mu, int clusz, bool rev) {
  int c = Mu.dim() / clusz;
  DMatrix ans(Mu.dim(), Mu.dim()); ans = .0;
  Index1D I(0,0);
  for (int i = 1; i <= clusz; i++) {
    I = Index1D(1, c) + I.ubound();
    DVector Mui = asVec(VecSubs(Mu, I));
    ans(I, I) = Vijj(Mui, rev);
  }
  return ans;
}

void ord_prep_beta(DVector &Yi, DMatrix &Xi, DVector &Offseti,
		   DMatrix &Zi, DVector &Ooffseti,
		   int clusz, int c, bool rev,
		   IVector &LinkWavei, 
		   GeeParam &par, GeeStr &geestr, Corr &cor,
		   //output
		   DMatrix &Di, DVector &PRi, DMatrix &Vi) {
  DVector Etai = Xi * par.beta() + Offseti;
  DVector Mui = geestr.MeanLinkinv(Etai, LinkWavei);
  DVector Mu_Etai = geestr.MeanMu_eta(Etai, LinkWavei);
  PRi = Yi - Mui;
  Di = SMult(Mu_Etai, Xi);

  if (clusz == 1) Vi = Vijj(Mui, rev);
  else if (cor.nparam() == 0) Vi = Mu2V1(Mui, clusz, rev);
  else { //cluster size greater than 1;
    DVector Psii = geestr.CorrLinkinv(Zi * par.alpha() + Ooffseti);
    Vi = ord2V1(Mui, Psii, clusz, rev);
  }
}

void ord_prep_beta(DVector &Y, DMatrix &X, DVector &Offset,
		   DMatrix &Z, DVector &Ooffset,
		   Index1D &I, Index1D &J,
		   int clusz, int c, bool rev,
		   IVector &LinkWave, 
		   GeeParam &par, GeeStr &geestr, Corr &cor,
		   //output
		   DMatrix &Di, DVector &PRi, DMatrix &Vi) {
  DVector Yi = asVec(VecSubs(Y, I));
  DMatrix Xi = asMat(MatRows(X, I));
  DVector Offseti = asVec(VecSubs(Offset, I));
  IVector LinkWavei = asVec(VecSubs(LinkWave, I));
  //cout << "Xi = " << Xi << "par.beta() = " << par.beta();
  DVector Etai = Xi * par.beta() + Offseti;
  DVector Mui = geestr.MeanLinkinv(Etai, LinkWavei);
  DVector Mu_Etai = geestr.MeanMu_eta(Etai, LinkWavei);
  PRi = Yi - Mui;
  Di = SMult(Mu_Etai, Xi);
  //if (I.lbound() == 1) cout << "Yi = " << Yi << "Xi = " << Xi << "Etai = " << Etai << "Mui = " << Mui;
  if (clusz == 1) Vi = Vijj(Mui, rev);
  else if (cor.nparam() == 0) Vi = Mu2V1(Mui, clusz, rev);
  else { //cluster size greater than 1;
    DMatrix Zi = asMat(MatRows(Z, J));
    DVector Ooffseti = asVec(VecSubs(Ooffset, J));
    DVector Psii = geestr.CorrLinkinv(Zi * par.alpha() + Ooffseti);
    Vi = ord2V1(Mui, Psii, clusz, rev);
  }
}

double update_beta(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
		   DVector &W, IVector &LinkWave, //DVector &CorP,
		   DMatrix &Z,  IVector &Clusz, int c, bool rev,
		   //IVector &ZcorSize, IVector &Jack, 
		   GeeParam &par, GeeStr &geestr, Corr &cor) {
  double del = 0;
  int p = par.p(), n = Clusz.size(); 
  DMatrix H(p,p); DVector G(p);
  Index1D I(0,0), J(0,0);

  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i);
    int s2 = s1 * (s1 - 1) / 2;
    I = Index1D(1, s1 * c) + I.ubound(); 
    if (s2 > 0) J = Index1D(1, s2 * c * c) + J.ubound();

    //    cout << "i = " << i << " J.lbound = " << J.lbound() << " J.ubound = " << J.ubound() << endl;
    DVector PRi(s1 * c); DMatrix Di(s1 * c, p), Vi(s1 * c, s1 * c);
    DVector Yi = asVec(VecSubs(Y, I));
    DMatrix Xi = asMat(MatRows(X, I));
    DVector Offseti = asVec(VecSubs(Offset, I));
    IVector LinkWavei = asVec(VecSubs(LinkWave, I));
    DMatrix Zi; DVector Ooffseti;
    if (cor.nparam() == 0 || s2 == 0) {
      Zi = DMatrix(1,1); Ooffseti = DVector(1);
    } else {
      Zi = asMat(MatRows(Z, J)); 
      Ooffseti = asVec(VecSubs(Ooffset, J));
    }

    ord_prep_beta(Yi, Xi, Offseti, Zi, Ooffseti, s1, c, rev,
		  LinkWavei, par, geestr, cor, Di, PRi, Vi);
    //if (i == 1) cout << "PRi = " << PRi << "Di = " << Di << "Vi = " << Vi;
    DVector rootWi = sqrt(asVec(VecSubs(W, I)));
    Di = SMult(rootWi, Di); PRi = SMult(rootWi, PRi);
    H = H + AtBiC(Di, Vi, Di);
    G = G + AtBiC(Di, Vi, PRi);
  }
  DVector Del = solve(H, G);
  par.set_beta(par.beta() + Del);
  del = fmax(fabs(Del));
  return del;
}

DVector kronecker(const DVector &v1, const DVector &v2) {
  int n1 = v1.size(), n2 = v2.size();
  DVector ans(n1 * n2);
  Index1D I(0,0);
  for (int i = 1; i <= n1; i++) {
    I = Index1D(1, n2) + I.ubound();
    VecSubs(ans, I) = v1(i) * v2;
  }
  return ans;
}

DVector vec(const DMatrix &m) {
  int r = m.num_rows(), c = m.num_cols();
  DVector ans(r * c, m.begin());
  return ans;
}

DVector hvec(const DMatrix &m) {
  int r = m.num_rows(), c = m.num_cols(), k = 1;
  DVector ans(r * c);
  for (int i = 1; i <= r; i++) 
    for (int j = 1; j <= c; j++)
      ans(k++) = m(i, j);
  return ans;
}

DMatrix ESSTijk(DVector &Mu1, DVector &Mu2, DMatrix &P11,
	      int c1, int c3, bool rev) {
  //P11 is c x c matrix
  int c = Mu1.size(), c13, c24;
  if (rev) c13 = fmax(c1, c3);
  else c13 = fmin(c1, c3); 
  DMatrix ans(c, c);
  for (int c2 = 1; c2 <= c; c2++) {
    for (int c4 = c2; c4 <= c; c4++) {
      if (rev) c24 = fmax(c2, c4);
      else c24 = fmin(c2, c4);
      ans(c2, c4) = 
	P11(c13, c24) - P11(c13, c2) * Mu2(c4) 
	- P11(c13, c4) * Mu2(c2) + Mu1(c13) * Mu2(c2) * Mu2(c4) 
	- P11(c1, c24) * Mu1(c3) + P11(c1, c2) * Mu1(c3) * Mu2(c4)
	+ P11(c1, c4) * Mu1(c3) * Mu2(c2) - 3 * Mu1(c1) * Mu1(c3) * Mu2(c2) * Mu2(c4)
	- P11(c3, c24) * Mu1(c1) + P11(c3, c2) * Mu1(c1) * Mu2(c4)
	+ P11(c3, c4) * Mu1(c1) * Mu2(c2)
	+ Mu1(c1) * Mu1(c3) * Mu2(c24);
      if (c4 > c2) ans(c4, c2) = ans(c2, c4);
    }
  }
  return ans;
}

DMatrix ESST(DVector &Mu1, DVector &Mu2, DMatrix &P11, bool rev) {
  int c = Mu1.size();
  DMatrix ans(c*c, c*c);
  Index1D I(0,0), J(0,0);
  for (int c1 = 1; c1 <= c; c1++) {
    J = I;
    I = Index1D(1, c) + I.ubound();
    for (int c3 = c1; c3 <= c; c3++) {
      J = Index1D(1, c) + J.ubound();
      ans(I, J) = ESSTijk(Mu1, Mu2, P11, c1, c3, rev);
      if (c3 > c1) ans(J, I) = ans(I, J);
    }
  }
  return ans;
}

void ord_prep_alpha(DVector &PR1, DVector &PR2, //DMatrix &V,
		    DVector &Mu1, DVector &Mu2, 
		    //c^2 x 1       c x 1           c x 1
		    DMatrix &Z, DVector &Ooffset,
		    bool rev, GeeParam &par, GeeStr &geestr,
		    //output
		    DVector &U2, DMatrix &V2, DMatrix &D2) {
  DVector Zeta = Z * par.alpha() + Ooffset; //Z is C^2 x q;
  DVector Psi = geestr.CorrLinkinv(Zeta);

  //cout << "PR1 = " << PR1 << "PR2 = " << PR2;
  DVector S = kronecker(PR1, PR2);
  //cout << "S = " << S;
  DMatrix V = Vijk(Mu1, Mu2, Psi);
  //  cout << "V = " << V;
  DVector Sigma = hvec(V);
  U2 = S - Sigma;

  DVector P11_Odds = p11_odds(Psi, Mu1, Mu2);
  DVector Odds_Zeta = geestr.CorrMu_eta(Zeta);
  D2 = SMult(SMult(P11_Odds, Odds_Zeta), Z);
  //D2 = d V / d alpha = d(P11 - mu1 * mu2) / d alpha = d P11 / d alpha 
  
  DMatrix P11 = odds2p11(Psi, Mu1, Mu2);
  V2 = ESST(Mu1, Mu2, P11, rev) - outerprod(Sigma);
}

double update_alpha(DVector &PR, DVector &Mu, DVector &W,
		    DMatrix &Z, DVector &Ooffset, 
		    IVector &Clusz, int c, bool rev,
		    GeeParam &par, GeeStr &geestr, Corr &cor) {
  double del = 0;
  int q = par.q(), n = Clusz.size();
  if (cor.nparam() == 0) return del;
  DMatrix H(q,q); DVector G(q);
  Index1D I(0,0), J(0,0);
  for (int i = 1; i <= n; i++) {
    int s1 = Clusz(i);
    int s2 = s1 * (s1 - 1) / 2;
    I = Index1D(1, s1 * c) + I.ubound(); 
    if (s2 > 0) J = Index1D(1, s2 * c * c) + J.ubound();
    if (s1 == 1) continue;

    DVector PRi = asVec(VecSubs(PR, I));
    DVector Mui = asVec(VecSubs(Mu, I));
    DMatrix Zi = asMat(MatRows(Z, J));
    DVector Ooffseti = asVec(VecSubs(Ooffset, J));
    Index1D K(0,0);
    for (int j = 1; j <= s1 - 1; j++) {
      Index1D I1((j - 1) * c + 1, j * c);
      DVector PR1 = asVec(VecSubs(PRi, I1));
      DVector Mu1 = asVec(VecSubs(Mui, I1));
      for (int k = j + 1; k <= s1; k++) {
	Index1D I2((k - 1) * c + 1, k * c);
	DVector PR2 = asVec(VecSubs(PRi, I2));
	DVector Mu2 = asVec(VecSubs(Mui, I2));

	K = Index1D(1,c*c) + K.ubound();
	DVector Ooffsetijk = asVec(VecSubs(Ooffseti, K));
	DMatrix Zijk = asMat(MatRows(Zi, K));
	DVector U2(c*c, 1); DMatrix V2(c*c, c*c), D2(c*c, q);
	//cout << "i = " << i << " j = " << j << " k = " << k;
	ord_prep_alpha(PR1, PR2, Mu1, Mu2, Zijk, Ooffsetijk, rev,
		       par, geestr, U2, V2, D2);

	//if (i == 1) cout << "U2 = " << U2 << "D2 = " << D2 << "V2 = "<< V2;
	H = H + AtBiC(D2, V2, D2);
	//cout << " AtBiC(D2, V2, D2)  = " << AtBiC(D2, V2, D2);
	//if (i == 37) cout << "V2 = "<< V2;
	G = G + AtBiC(D2, V2, U2);
      }
    }
  }
  //cout << "H = " << H;
  DVector Del = solve(H, G);
  par.set_alpha(par.alpha() + Del);
  del = fmax(fabs(Del));
  return del;
}

/* the following estimation procedure assumed that each cluster has size 
at least 1.    Nov. 6, 2002.
*/

void ordgee_est(DVector &Y, DMatrix &X, 
		DVector &Offset, DVector &Ooffset, DVector &W, 
		IVector &LinkWave,
		DMatrix &Z, IVector &Clusz, int c, bool rev,
		GeeStr &geestr, Corr &cor, GeeParam &par,
		Control &con) {
  DVector Del(3); Del = 0.0;

  int N = Y.size(); // N = sum(n_i) * c;  
  DVector PR(N), Mu(N);
  
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
    Del(1) = update_beta(Y, X, Offset, Ooffset, W, LinkWave, 
			 Z, Clusz, c, rev, par, geestr, cor);

    //no updating gamma;
    
    //updating alpha;
    Mu = geestr.MeanLinkinv(X * par.beta() + Offset, LinkWave);
    PR = Y - Mu;

    Del(3) = update_alpha(PR, Mu, W, Z, Ooffset, Clusz, c, rev, par, geestr, cor);

    del = fmax(Del);
    if (del <= con.tol()) break;
  }
  if (iter == con.maxiter()) par.set_err(1);
}

void HiandGi(DVector &Yi, DMatrix &Xi, DVector &Offseti, DVector &Ooffseti,
	     IVector &LinkWavei, 
	     DMatrix &Zi,  int s1, int c, bool rev,
	     //Index1D &I, Index1D &J,
	     GeeParam &par, GeeStr &geestr, Corr &cor,
	     //output
	     Hess &Hi, Grad &Gi) {
  //need D1, V1, U1, D2, V2, U2, Sig_Beta for H and G
  int p = par.p(), q = par.q();
  DVector PRi(s1 * c); 
  DMatrix D1i(s1 * c, p), V1i(s1 * c, s1 * c);
  ord_prep_beta(Yi, Xi, Offseti, Zi, Ooffseti, s1, c, rev,
		LinkWavei, par, geestr, cor, D1i, PRi, V1i);
  Hi.set_A(AtBiC(D1i, V1i, D1i));
  Gi.set_U1(AtBiC(D1i, V1i, PRi));
  
  if (s1 == 1) return;
  if (cor.nparam() == 0) return;
  
  DVector Mui = Yi - PRi;
  Index1D K(0,0);
  for (int j = 1; j <= s1 - 1; j++) {
    Index1D I1((j - 1) * c + 1, j * c);
    DVector PR1 = asVec(VecSubs(PRi, I1));
    DVector Mu1 = asVec(VecSubs(Mui, I1));
    DMatrix D1j = asMat(MatRows(D1i, I1));
    for (int k = j + 1; k <= s1; k++) {
      Index1D I2((k - 1) * c + 1, k * c);
      DVector PR2 = asVec(VecSubs(PRi, I2));
      DVector Mu2 = asVec(VecSubs(Mui, I2));
      DMatrix D1k = asMat(MatRows(D1i, I2));

      K = Index1D(1,c*c) + K.ubound();
      DVector Ooffsetijk = asVec(VecSubs(Ooffseti, K));
      DMatrix Zijk = asMat(MatRows(Zi, K));
      DVector U3i(c*c, 1); DMatrix V3i(c*c, c*c), D3i(c*c, q);
      ord_prep_alpha(PR1, PR2, Mu1, Mu2, Zijk, Ooffsetijk, rev,
		     par, geestr, U3i, V3i, D3i);
      
      Hi.inc_F(AtBiC(D3i, V3i, D3i));
      Gi.set_U3(Gi.U3() + AtBiC(D3i, V3i, U3i));

      DVector Zeta = Zi * par.alpha() + Ooffseti; //Z is C^2 x q;
      DVector Psi = geestr.CorrLinkinv(Zeta);
      DMatrix U3_Beta = getU3_Beta(Mu1, Mu2, Psi, D1j, D1k, PR1, PR2);
      Hi.inc_D(AtBiC(D3i, V3i, -1.0 * U3_Beta));
    }
  }
}

void HnandGis(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
	      IVector &LinkWave, DMatrix &Z,  
	      IVector &Clusz, int c, bool rev,
	      GeeParam &par, GeeStr &geestr, Corr &cor,
	      Hess &Hn, Vector<Grad> &Gis) {
//    Hess H(par), Hi(par); Grad Gi(par);
//    Index1D I(0,0), J(0,0);
//    int N = Clusz.size();
//    for (int i = 1; i <= N; i++) {
//      int s1 = Clusz(i);
//      int s2 = s1 * (s1 - 1) / 2;
//      I = Index1D(1, s1 * c) + I.ubound(); 
//      if (s2 > 0) J = Index1D(1, s2 * c * c) + J.ubound();
//      Hess Hi(par); Grad Gi(par);
//      HiandGi(Y, X, Offset, Ooffset, LinkWave, Z,  s1, c, I, J,
//  	    par, geestr, cor, Hi, Gi);
//      H.inc(Hi); Gis(i) = Gi;
//    }
//    Hn = (1.0/(double) N) * H;
  IVector Scur(Y.size() / c); Scur = 1;
  HnandGis(Y, X, Offset, Ooffset, LinkWave, Z, Clusz, c, rev,
	   par, geestr, cor, Scur, Hn, Gis);
}

void HnandGis(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
	      IVector &LinkWave, DMatrix &Z,  
	      IVector &Clusz, int c, bool rev,
	      GeeParam &par, GeeStr &geestr, Corr &cor, IVector &Scur,
	      Hess &Hn, Vector<Grad> &Gis) {
  Hess H(par), Hi(par); Grad Gi(par);
  Index1D I(0,0), J(0,0), K(0,0);
  int N = Clusz.size();
  for (int i = 1; i <= N; i++) {
    int s1 = Clusz(i);
    int s2 = s1 * (s1 - 1) / 2;
    K = Index1D(1, s1) + K.ubound();
    I = Index1D(1, s1 * c) + I.ubound(); 
    if (s2 > 0) J = Index1D(1, s2 * c * c) + J.ubound();

    IVector Scuri = asVec(VecSubs(Scur, K));
    int clsz = sum(Scuri); //this is the new cluster size that should be passed down!!!!!!!
    if (clsz == 0) continue; 

    //get dat i
    DVector Yi = asVec(VecSubs(Y, I));
    DMatrix Xi = asMat(MatRows(X, I));
    DVector Offseti = asVec(VecSubs(Offset, I));
    IVector LinkWavei = asVec(VecSubs(LinkWave, I));
    DMatrix Zi; DVector Ooffseti;

    if (cor.nparam() == 0 || s2 == 0) {
      Zi = DMatrix(1,1); Ooffseti = DVector(1);
    } 
    else {
      Zi = asMat(MatRows(Z, J)); 
      Ooffseti = asVec(VecSubs(Ooffset, J));
    }

    //valid data i
    IVector VI = genVI(Scuri, c), VJ = genCrossVI(Scuri, c);
    DVector VYi = Valid(Yi, VI), VOffseti = Valid(Offseti, VI); 
    IVector VLinkWavei = Valid(LinkWavei, VI);
    DMatrix VXi = Valid(Xi, VI);
    DMatrix VZi;  DVector VOoffseti;
    if (cor.nparam() == 0 || clsz == 1) {//clsz == 1: no need to go association
      VZi = DMatrix(1,1); VOoffseti = DVector(1);
    }
    else {
      VZi = Valid(Zi, VJ);
      VOoffseti = Valid(Ooffseti, VJ);
    }
    Hess Hi(par); Grad Gi(par);
    HiandGi(VYi, VXi, VOffseti, VOoffseti, VLinkWavei, VZi, clsz, c, rev,
	    par, geestr, cor, Hi, Gi);
    H.inc(Hi); Gis(i) = Gi;
  }
  Hn = (1.0/(double) N) * H;
}

void ordgee_var(DVector &Y, DMatrix &X, DVector &Offset, DVector &Ooffset,
		DVector &W, IVector &LinkWave, DMatrix &Z,  
		IVector &Clusz, int c, bool rev,
		GeeStr &geestr, Corr &cor, GeeParam &par) {
  int N = Clusz.size(), p = par.p(), q = par.q();
  Hess Hn(par); Vector<Grad> Gis(N); Grad G0(par); Gis = G0;
  HnandGis(Y, X, Offset, Ooffset, LinkWave, Z, Clusz, c, rev,
	   par, geestr, cor, Hn, Gis);
  IVector level(2); level(2) = 1;
  Hess Hinv = inv(Hn, level);
  Vector<DVector> Beta_infs(N), Alpha_infs(N);
  DMatrix VB(p,p), VA(q,q);
  for (int i = 1; i <= N; i++) {
    Beta_infs(i) = Hinv.A() * Gis(i).U1();
    VB = VB + outerprod(Beta_infs(i));
    if (cor.nparam() == 0) continue;
    Alpha_infs(i) = Hinv.D() * Gis(i).U1() + Hinv.F() * Gis(i).U3();
    VA = VA + outerprod(Alpha_infs(i));
  }
  par.set_vbeta_naiv(1.0/N * Hinv.A());
  par.set_vbeta(1.0/N/N * VB);
  if (cor.nparam() == 0) return;

  //only those cluster with size 2 or more contributes to the variance of alpha
  //int Nalp = 0;
  //for (int i = 1; i <= N; i++) if (Clusz(i) > 1) Nalp++;
  par.set_valpha_naiv(1.0/N * Hinv.F());
  //par.set_valpha(1.0/Nalp/Nalp * VA);
  par.set_valpha(1.0/N/N * VA);
}

void ordgee_top(DVector &Y, DMatrix &X, 
		DVector &Offset, DVector &Ooffset, DVector &W, 
		IVector &LinkWave,
		DMatrix &Z, IVector &Clusz, int c, bool rev,
		GeeStr &geestr, Corr &cor, GeeParam &par,
		Control &con) {
  ordgee_est(Y, X, Offset, Ooffset, W, LinkWave, Z, Clusz, c, rev,
	     geestr, cor, par, con);
  ordgee_var(Y, X, Offset, Ooffset, W, LinkWave, Z, Clusz, c, rev,
	     geestr, cor, par);
}

extern "C" {
  SEXP ordgee_rap(SEXP y, SEXP x, SEXP offset, SEXP doffset, SEXP w,
		  SEXP linkwave, SEXP z, SEXP clusz, SEXP ncat, SEXP rev, 
		  SEXP geestr, SEXP cor, SEXP par, SEXP con) {
    DVector Y = asDVector(y), Offset = asDVector(offset), Doffset = asDVector(doffset), W = asDVector(w);
    IVector LinkWave = asIVector(linkwave); 

    DMatrix X = asDMatrix(x), Z = asDMatrix(z);
    IVector Clusz = asIVector(clusz); 
    int C = INTEGER(AS_INTEGER(ncat))[0];
    bool Rev = LOGICAL(AS_LOGICAL(rev))[0];
    Control Con = asControl(con);   
    GeeParam Par = asGeeParam(par);   
    GeeStr Geestr = asGeeStr(geestr);   
    Corr Cor = asCorr(cor);   
    ordgee_top(Y, X, Offset, Doffset, W, LinkWave, Z, Clusz, C, Rev, Geestr, Cor, Par, Con);
    SEXP ans = asSEXP(Par);
    return ans;
  }
}
