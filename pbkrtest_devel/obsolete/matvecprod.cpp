
#include <Rcpp.h>
using namespace Rcpp;


//[[Rcpp::export]]
NumericVector mvprod1_cpp (NumericMatrix A, NumericVector x){
  int nrA=A.nrow(), ncA=A.ncol(), i, j;
  NumericVector y(nrA);
  for (i=0; i<nrA; ++i){
    for (j=0; j<ncA; ++j){
      y[i] += A(i,j)*x[j];
    }
  }
  return y;
}

//[[Rcpp::export]]
NumericVector mvprod2_cpp (const NumericMatrix& A, const NumericVector& x){
  int nrA=A.nrow(), ncA=A.ncol(), i, j;
  NumericVector y(nrA);
  for (j=0; j<ncA; ++j){
    for (i=0; i<nrA; ++i){
      y[i] += A(i,j)*x[j];
    }
  }
  return y;
}

//[[Rcpp::export]]
NumericVector mmprod1_cpp (const NumericMatrix& A, const NumericMatrix& B){
  int nrA=A.nrow(), ncA=A.ncol(), i, j;
  NumericMatrix C(nrA, B.ncol());
  NumericVector y(nrA);
  for (j=0; j<B.ncol(); ++j){
    y = mvprod2_cpp(A, B(_, j));
    for (i=0; i<nrA; ++i)
      C(i, j) = y[i];
  }
  return C;
}





/*** R
n <- 2000
A <- matrix(rnorm(n^2), nr=n)
A <- A + t.default(A)
x <- rnorm(n)

y1 = as.numeric(A %*% x)
y2 = mvprod1_cpp(A, x)
y3 = mvprod2_cpp(A, x)

all.equal(y1, y2)
all.equal(y1, y3)

#microbenchmark::microbenchmark(A %*% x, mvprod1_cpp(A, x), mvprod2_cpp(A,x), times=10)


C1 = A %*% A
C2 = mmprod1_cpp(A, A)
all.equal(C1, C2)
microbenchmark::microbenchmark(A %*% A, crossprod(A), times=10)




 */

