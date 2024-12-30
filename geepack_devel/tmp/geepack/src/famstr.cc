//using namespace std;

#include "tntsupp.h"
#include "geese.h"
#include "utils.h"
#include "famstr.h"
#include <cfloat>

#include <Rmath.h>

/*
typedef double fun1(double);
typedef double fun2(double, int);

DVector Apply_elt(const DVector &V, fun1* f) {
  DVector ans(V.size());
  for (int i = 1; i <= V.size(); i++) ans(i) = f(V(i));
  return ans;
}

DVector Apply_elt(const DVector &V, const DVector &Wave, fun2* f) {
  DVector ans(V.size());
  for (int i = 1; i <= V.size(); i++) ans(i) = f(V(i), (int)Wave(i));
  return ans;
}
*/
/*
double dnorm(double x) {return dnorm4(x, 0, 1, 0);}

double pnorm(double x) {return pnorm5(x, 0, 1, 1, 0);}

double qnorm(double x) {return qnorm5(x, 0, 1, 1, 0);}
*/

//link functions

// logit
double linkfun_logit(double mu) {return log(mu/(1 - mu));}

double linkinv_logit(double eta) {
  double thres = - log(DBL_EPSILON);
  eta = (eta > thres) ? thres : eta;
  eta = (eta < - thres) ? -thres : eta;
  return exp(eta)/(1 + exp(eta));
}

double mu_eta_logit(double eta) {
  double thres = - log(DBL_EPSILON);
  if (fabs(eta) >= thres) return DBL_EPSILON;
  else return exp(eta)/pow(1 + exp(eta), 2);
} 

bool valideta_logit(double eta) {return true;}

//probit
double linkfun_probit(double mu) {return qnorm(mu,0,1,1,0);}
double linkinv_probit(double eta) {
  double thres = -qnorm(DBL_EPSILON,0,1,1,0);
  eta = min(thres, max(eta, -thres));
  return pnorm(eta,0,1,1,0);
}
double mu_eta_probit(double eta) {
  return max(dnorm(eta,0,1,0), DBL_EPSILON);
}
bool valideta_probit(double eta) {return true;}

//cloglog
double linkfun_cloglog(double mu) {return log(-log(1 - mu));}
double linkinv_cloglog(double eta) {
  double ans = 1 - exp(- exp(eta));
  ans = min(1 - DBL_EPSILON, ans);
  return max(DBL_EPSILON, ans);
}
double mu_eta_cloglog(double eta) {
  eta = min(eta, 700.0);
  return max(DBL_EPSILON, exp(eta) * exp(-exp(eta)));
}
bool valideta_cloglog(double eta) {return true;}

//ident
double linkfun_ident(double mu) {return mu;}
double linkinv_ident(double eta) {return eta;}
double mu_eta_ident(double eta) {return 1.0;}
bool valideta_ident(double eta) {return true;}

//log
double linkfun_log(double mu) {return log(mu);}
double linkinv_log(double eta) {return max(DBL_EPSILON, exp(eta));}
double mu_eta_log(double eta) {return max(DBL_EPSILON, exp(eta));}
bool valideta_log(double eta) {return true;}

//sqrt
double linkfun_sqrt(double mu) {return sqrt(mu);}
double inkinv_sqrt(double eta) {return eta * eta;}
double mu_eta_sqrt(double eta) {return 2 * eta;}
bool valideta_sqrt(double eta) {return eta > 0;}

//recipsquare
double linkfun_recipsquare(double mu) {return 1 / mu / mu;}
double linkinv_recipsquare(double eta) {return 1 / sqrt(eta);}
double mu_eta_recipsquare(double eta) {return -1 / (2 * pow(eta, 1.5));}
bool valideta_recipsquare(double eta) {return eta > 0;}

//inverse
double linkfun_inverse(double mu) {return 1 / mu;}
double linkinv_inverse(double eta) {return 1 / eta;}
double mu_eta_inverse(double eta) {return -1 / eta / eta;}
bool valideta_inverse(double eta) {return eta != 0;}

//fisherz
double linkfun_fisherz(double mu) {return log(2/(1 - mu) - 1);}
double linkinv_fisherz(double eta) {
  double thres = - log(DBL_EPSILON);
  eta = (eta > thres) ? thres : eta;
  eta = (eta < - thres) ? -thres : eta;
  return 1 - 2 / (exp(eta) + 1);
}
double mu_eta_fisherz(double eta) {
  double thres = - log(DBL_EPSILON);
  if (fabs(eta) >= thres) return DBL_EPSILON;
  return 2 * exp(eta) / pow(1 + exp(eta), 2);
}
bool valideta_fisherz(double eta) {return true;}


//Lin, Wei, Ying
double linkfun_lwyBC2(double mu) {
  return log(sqrt(mu + 1) - 1);
}
double linkinv_lwyBC2(double eta) {
  double foo = max(DBL_EPSILON, exp(eta));
  return pow(1 + foo, 2.0) - 1;
}
double mu_eta_lwyBC2(double eta) {
  double foo = exp(eta);
  return max(DBL_EPSILON, 2 * (1 + foo) * foo);
}

double linkfun_lwylog(double mu) {
  return log(exp(mu) - 1);
}
double linkinv_lwylog(double eta) {
  return log(exp(eta) + 1);
}
double mu_eta_lwylog(double eta) {
  double foo = exp(eta);
  return foo/(foo + 1);
}

//variance functions
double variance_binomial(double mu) {return mu * (1 - mu);}
double v_mu_binomial(double mu) {return 1 - 2 * mu;}
bool validmu_binomial(double mu) {return mu > 0 && mu < 1;}

double variance_gaussian(double mu) {return 1.0;}
double v_mu_gaussian(double mu) {return .0;}
bool validmu_gaussian(double mu) {return true;}

double variance_poisson(double mu) {return mu;}
double v_mu_poisson(double mu) {return 1.0;}
bool validmu_poisson(double mu) {return mu > 0;}

double variance_inverse_gaussian(double mu) {return pow(mu, 3);}
double v_mu_inverse_gaussian(double mu) {return 3 * mu * mu;}
bool validmu_inverse_gaussian(double mu) {return true;}

double variance_Gamma(double mu) {return mu * mu;}
double v_mu_Gamma(double mu) {return 2 * mu;}
bool validmu_Gamma(double mu) {return mu > 0;}

DMatrix cor_exch(const DVector &rho, const DVector &wave) {
  int n = wave.size();
  DMatrix ans(n,n);
  for (int i = 1; i <= n; i++)
    for (int j = 1; j <= n; j++) 
      ans(i,j) = (i == j) ? 1.0 : rho(1);
  return ans;
}

DMatrix cor_rho_exch(const DVector &rho, const DVector &wave) {
  int n = wave.size();
  DMatrix ans(n * (n - 1) / 2, 1);
  ans = 1.0;
  return ans;
}

DMatrix cor_indep(const DVector &, const DVector &wave) {
  return ident(wave.size());
}

DMatrix cor_rho_indep(const DVector &, const DVector &) {
  return ident(0);
}

DMatrix cor_fixed(const DVector &rho, const DVector &wave) {
  return cor_unstr(rho, wave);
}

DMatrix cor_rho_fixed(const DVector &, const DVector &) {
  return ident(0);
}


DMatrix cor_ar1(const DVector &rho, const DVector &wave) {
  int n = wave.size();
  DMatrix ans(n,n);
  for (int i = 1; i <= n; i++)
    for (int j = 1; j <= n; j++)
      ans(i,j) = (i == j) ? 1.0 : pow(rho(1), fabs(wave(j) - wave(i)));
  return ans;
}

DMatrix cor_rho_ar1(const DVector &rho, const DVector &wave) {
  int n = wave.size();
  DMatrix ans(n * (n - 1) / 2, 1);
  int k = 1;
  for (int i = 1; i <= n - 1; i++) {
    for (int j = i + 1; j <= n; j ++) {
      double tmp = fabs(wave(j) - wave(i));
      ans(k, 1) = (tmp == 1.0) ? 1.0 : (tmp * pow(rho(1), tmp - 1.0));
      k++;
    }
  }
  return ans;
}

DMatrix cor_unstr(const DVector &rho, const DVector &wave) {
  DMatrix fullmat = rho2mat(rho);
  return MatRowCol(fullmat, wave, wave);
}

DMatrix cor_rho_unstr(const DVector &rho, const DVector &wave) {
  int n = wave.size();
  return ident(n * (n - 1) / 2);
}

//class Corr
Corr:: Corr(int corst, int maxwave): _corst(corst), _maxwave(maxwave) {
    switch(corst) {
    case INDEPENDENCE:
      _nparam = 0; init(cor_indep, cor_rho_indep); break;
    case EXCHANGEABLE:
      _nparam = 1; init(cor_exch, cor_rho_exch); break;
    case AR1:
      _nparam = 1; init(cor_ar1, cor_rho_ar1); break;
    case UNSTRUCTURED:
    case USERDEFINED:
      _nparam = maxwave; init(cor_unstr, cor_rho_unstr); break;
    case FIXED:
      _nparam = 0; init(cor_fixed, cor_rho_fixed); break;
    }
}

//class Link
//Link::Link() { Link(IDENT); }
//Link::Link(int link) {
Link::Link(int link) {
    switch(link) {
    case LOGIT:
      init(linkfun_logit, linkinv_logit, mu_eta_logit);
      break;
    case IDENT:
      init(linkfun_ident, linkinv_ident, mu_eta_ident);
      break;
    case PROBIT:
      init(linkfun_probit, linkinv_probit, mu_eta_probit);
      break;
    case CLOGLOG:
      init(linkfun_cloglog, linkinv_cloglog, mu_eta_cloglog);
      break;
    case LOG:
      init(linkfun_log, linkinv_log, mu_eta_log);
      break;
    case INVERSE:
      init(linkfun_inverse, linkinv_inverse, mu_eta_inverse);
      break;
    case FISHERZ:
      init(linkfun_fisherz, linkinv_fisherz, mu_eta_fisherz);
      break;
    case LWYBC2:
      init(linkfun_lwyBC2, linkinv_lwyBC2, mu_eta_lwyBC2);
      break;
    case LWYLOG:
      init(linkfun_lwylog, linkinv_lwylog, mu_eta_lwylog);
      break;
    }
}

Link::Link(fun1* linkfun, fun1* linkinv, fun1* mu_eta) {
  init(linkfun, linkinv, mu_eta);
}

//class Variance
//Variance::Variance() {Variance(GAUSSIAN); }
Variance::Variance(int var) {
//Variance::Variance(int var) {
    switch(var) {
    case GAUSSIAN:
      init(variance_gaussian, v_mu_gaussian, validmu_gaussian); break;
    case BINOMIAL:
      init(variance_binomial, v_mu_binomial, validmu_binomial); break;
    case POISSON:
      init(variance_poisson, v_mu_poisson, validmu_poisson); break;
    case GAMMA:
      init(variance_Gamma, v_mu_Gamma, validmu_Gamma); break;
    }
}

//class GeeStr
GeeStr::GeeStr(int n, Vector<int> meanlink, Vector<int> v,
	       Vector<int> scalelink, int corrlink, int scalefix) :    
  CorrLink(corrlink), ScaleFix_(scalefix) {
  //int n = meanlink.size();
  //MeanLink.newsize(n); V.newsize(n); ScaleLink.newsize(n);
  Vector<Link> ML(n), SL(n); Vector<Variance> VS(n);
  for (int i = 1; i <= n; i++) {
    Link ml(meanlink(i)), sl(scalelink(i)); Variance vi(v(i));
    ML(i) = ml; //MeanLink(i) = LINK[meanlink(i) - 1];
    VS(i) = vi; //V(i) = VARIANCE[v(i) - 1]; 
    SL(i) = sl; //ScaleLink(i) = LINK[scalelink(i) - 1];
  }
  MeanLink = ML; V = VS; ScaleLink = SL;
}
DVector GeeStr::MeanLinkfun(const DVector &Mu, const IVector &Wave) {
  int size = Mu.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = MeanLink(Wave(i)).linkfun(Mu(i));
  return ans;
}
DVector GeeStr::MeanLinkinv(const DVector &Eta, const IVector &Wave) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = MeanLink(Wave(i)).linkinv(Eta(i));
  return ans;
}
DVector GeeStr::MeanMu_eta(const DVector &Eta, const IVector &Wave) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = MeanLink(Wave(i)).mu_eta(Eta(i));
  return ans;
}
DVector GeeStr::ScaleLinkfun(const DVector &Mu, const IVector &Wave) {
  int size = Mu.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = ScaleLink(Wave(i)).linkfun(Mu(i));
  return ans;
}
DVector GeeStr::ScaleLinkinv(const DVector &Eta, const IVector &Wave) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = ScaleLink(Wave(i)).linkinv(Eta(i));
  return ans;
}
DVector GeeStr::ScaleMu_eta(const DVector &Eta, const IVector &Wave) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = ScaleLink(Wave(i)).mu_eta(Eta(i));
  return ans;
}
DVector GeeStr::CorrLinkfun(const DVector &Mu) {
  int size = Mu.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = CorrLink.linkfun(Mu(i));
  return ans;
}
DVector GeeStr::CorrLinkinv(const DVector &Eta) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = CorrLink.linkinv(Eta(i));
  return ans;
}
DVector GeeStr::CorrMu_eta(const DVector &Eta) {
  int size = Eta.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = CorrLink.mu_eta(Eta(i));
  return ans;
}
DVector GeeStr::v(const DVector &Mu, const IVector &Wave) {
  int size = Mu.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = V(Wave(i)).v(Mu(i));
  return ans;
}
DVector GeeStr::v_mu(const DVector &Mu, const IVector &Wave) {
  int size = Mu.size(); DVector ans(size);
  for (int i = 1; i <= size; i++) ans(i) = V(Wave(i)).v_mu(Mu(i));
  return ans;
}
bool GeeStr::validMu(const DVector &Mu, const IVector &Wave) {
  int size = Mu.size(); 
  bool ans = true;
  for (int i = 1; i <= size; i++) {
    if ( !( V(Wave(i)).validmu(Mu(i)) ) ) {
      ans = false;
      break;
    }
  }
  return ans;
}
