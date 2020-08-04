// using namespace std;

#include <cmath>
#include <R.h>

#include "tntsupp.h"
#include "geese.h"

void VecPrint(const DVector &v) {
  for (int i = 0; i < v.dim(); i++) Rprintf("%f ", v[i]);
  Rprintf("\n");
}

Fortran_Matrix<double> ident (int n) {
  Fortran_Matrix<double> ans(n,n);
  for (int i = 1; i <= n; i++) ans(i,i) = 1.0;
  return ans;
}

Fortran_Matrix<double> MatRowCol(const Fortran_Matrix<double> &mat, const Vector<double> &r, const Vector<double> &c) {
  int m = r.size(), n = c.size();
  Fortran_Matrix<double> ans(m,n);
  for (int i = 1; i <= m; i++)
    for (int j = 1; j <= n; j++) 
      ans(i,j) = mat((int) r(i), (int) c(j));
  return ans;
}
Fortran_Matrix<double> rho2mat(const Vector<double> &rho) {
  int s = rho.size(); // s = n(n-1)/2
  int n = (int) (0.5 * ( 1 + sqrt(1.0 + 8 * s)));
  Fortran_Matrix<double> fullmat = ident(n); 
  int k = 1;
  for (int i = 1; i <= n - 1; i++)
    for (int j = i + 1; j <= n; j++) {
	fullmat(i, j) = rho(k++);
	fullmat(j, i) = fullmat(i, j);
    }
  return fullmat;
}

//solve(a, b = ident(n))
DMatrix solve(const DMatrix &a, const DMatrix &b) {
  Subscript m = a.dim(1); // assert(m == a.dim(2));
  Subscript n = b.dim(1); // assert(m == n);
  Subscript l = b.dim(2);
  Vector<Subscript> index(m);
  DMatrix T(a), B(b);
  DMatrix ans(n,l);
  if (LU_factor(T, index) != 0) {
    // cerr << "LU_factor() failed." << endl; 
    return ans;
  }
  DVector v(m);
  for (int i  = 1; i <= l; i++) {
    v = asVec(MatCol(B,i));
    LU_solve(T, index, v);
    MatCol(ans, i) = asColMat(v);
  }
  return ans;  
}

DVector solve(const DMatrix &A, const DVector &b) {
  DMatrix T(A); Vector<Subscript> index(b.size());
  DVector ans(b);
  if (LU_factor(T, index) !=0) {
    //cerr << "LU_factor() failed." << endl;
    return ans;
  }

  if (LU_solve(T, index, ans) != 0)  {
    //cerr << "LU_Solve() failed." << endl;
    return ans;
  }
  return ans;
} 

DMatrix solve(const DMatrix &a) {
  DMatrix b = ident(a.dim(1));
  return solve(a, b);
}

DMatrix AtBiC(const DMatrix &A, const DMatrix &B, const DMatrix &C) {
  DMatrix BiC = solve(B, C);
  return Transpose_view(A) * BiC;
}

DVector AtBiC(const DMatrix &A, const DMatrix &B, const DVector &C) {
  DVector BiC = solve(B, C);
  return Transpose_view(A) * BiC;
}


DMatrix apply_elwise(const DMatrix &x, double f(double)) {
  DMatrix ans = x;
  for (int i = 1; i <= x.dim(1); i++)
    for (int j = 1; j <= x.dim(2); j++)
      ans(i, j) = f(x(i, j));
  return ans;
}
/*
DMatrix apply_elwise(DMatrix &x, double f(double)) {
  return apply_elwise(x, f);
}
*/

DVector apply_elwise(const DVector &x, double f(double)) {
  DVector ans = x;
  for (int i = 1; i <= x.dim(); i++)
    ans(i) = f(x(i));
  return ans;
}

/*
DVector apply_elwise(DVector &x, double f(double)) {
  return apply_elwise(x, f);
}
*/

DVector sqrt(const DVector &x) {
  return apply_elwise(x, sqrt);
}

double square(double x) {
  return x * x;
}

DVector square(const DVector &x) {
  return apply_elwise(x, square);
}

double reciproot(double x) {
  return 1./sqrt(x);
}


DVector reciproot(const DVector &x) {
  return apply_elwise(x, reciproot);
}

double recip(double x) {return 1./x;}

DVector recip(const DVector &x) {
  return apply_elwise(x, recip);
}

int cluscount(DVector &ID) {
  int ans = 1;
  for (int i = 1; i < ID.dim(); i++)
    if (ID(i - 1) != ID(i)) ans++;
  return ans;
}

Vector<int> clussize(DVector &ID) {
  int K = ID.size();
  Vector<int> ans(K); 
  ans = 1;
  //double id = ID(0);
  int k = 1;
  for (int i = 1; i <= (ID.dim() - 1); i++) {
    if (ID(i + 1) == ID(i)) ans(k) += 1;
    else k++;
  }
  return ans;
}

DVector SMult(const DVector &v1, const DVector &v2) {
  // assert (v1.dim() == v2.dim());
  DVector ans = v1;
  for (int i = 1; i <= v1.dim(); i++)
    ans(i) = v1(i) * v2(i);
  return ans;
}

DMatrix SMult(const DVector &v, const DMatrix &m) {
  // assert (v.dim() == m.dim(1));
  DMatrix tmp = m;
  for (int i = 1; i <= m.dim(1); i++)
    for (int j = 1; j <= m.dim(2); j++)
      tmp(i, j) = v(i) * m(i, j);
  return tmp;
}

DMatrix operator*(const DVector &v, const DMatrix &m) {
  return SMult(v, m);
}


DMatrix diag(const DVector &v) {
  int n = v.dim();
  DMatrix ans(n, n); ans = .0;
  for (int i = 1; i <= n; i++) ans(i, i) = v(i);
  return ans;
}

DVector diag(const DMatrix &m) {
  int n = m.dim(1); //assert m.dim(0) == m.dim(1);
  DVector ans(n); ans = .0;
  for (int i = 1; i <= n; i++) ans(i) = m(i,i);
  return ans;
}

DMatrix inv(const DMatrix &x) {
  return solve(x);
}

DMatrix fabs(const DMatrix &m) {
  return apply_elwise(m, fabs);
}

DVector fabs(const DVector &v) {
  return apply_elwise(v, fabs);
}
