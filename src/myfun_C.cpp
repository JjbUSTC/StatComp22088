// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
// using namespace arma;

//' @title SVD using RcppArmadillo
//' @description A faster SVD than R
//' @param X a numeric matrix
//' @return SVD Decomposition Right Matrix
//' @import Rcpp
//' @import RcppArmadillo
//' @useDynLib StatComp22088
//' @examples
//' \dontrun{
//' X<-matrix(1:10,ncol=2)
//' res <- SVD_V(X)
//' }
//' @export
// [[Rcpp::export]]
arma::mat SVD_V(arma::mat X) {
  arma::mat U;
  arma::vec s;
  arma::mat V;
  arma::svd_econ(U, s, V, X);  
  return V;
}

//' @title Scaling and Centering of Matrix-like Objects
//' @description center and scale the columns of a numeric matrix.
//' @param X a numeric matrix
//' @param center centralization or not
//' @param scale standardization or not
//' @return centered and scaled matrix
//' @import Rcpp
//' @import RcppArmadillo
//' @useDynLib StatComp22088
//' @examples
//' \dontrun{
//' X<-matrix(1:10,ncol=2)
//' res <- scaleC(X,center=T,scale=T)
//' }
//' @export
// [[Rcpp::export]]
arma::mat scaleC(arma::mat X,bool center,bool scale){
  arma::mat S,V;
  S=arma::mean(X,0);
  V=arma::stddev(X,0,0);
  if(center==TRUE){
    for(arma::uword i=0;i<X.n_rows;i++){
      X.row(i)=X.row(i)-S;
    } 
  }
  if(scale==TRUE){
    for(arma::uword i=0;i<X.n_rows;i++){
      X.row(i)=X.row(i)/V;
    } 
  }
  return(X);
}

//' @title SVD using RcppArmadillo also
//' @description First k singular values and vectors for sparse matrix
//' @param X a sparse matrix
//' @param k number of expected singular values
//' @return First k vectors of SVD Decomposition Right Matrix
//' @import Rcpp
//' @import RcppArmadillo
//' @useDynLib StatComp22088
//' @examples
//' \dontrun{
//' X<-matrix(sample(0:2,size=20,replace=T,prob = c(0.8,0.1,0.1)),ncol=4)
//' res <- SVDS_V(X)
//' }
//' @export
// [[Rcpp::export]]
arma::mat SVDS_V(arma::sp_mat X,int k=10) {
  arma::mat U;
  arma::vec s;
  arma::mat V;
  arma::svds(U, s, V, X, k);  
  return V;
}

//' @title Gibbs sampler in homework
//' @description Gibbs sampler in homework for two dimensional normal state
//' @param mu1 mean
//' @param mu2 mean
//' @param sigma1 standard deviation
//' @param sigma2 standard deviation
//' @param rho coefficient of correlation
//' @param N numbers
//' @return a random sample of size \code{N}
//' @import Rcpp
//' @useDynLib StatComp22088
//' @examples
//' \dontrun{
//' mu1=mu2=0
//' sigma1=sigma2=1
//' rho=0.9
//' N=100
//' gib<-gibbsC(mu1,sigma1,mu2,sigma2,rho,N)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(double mu1,double sigma1,double mu2,double sigma2,double rho,int N) {
  NumericMatrix Z(N, 2);
  double s1=0,s2=0;
  s1 = sqrt(1-rho*rho)*sigma1;
  s2 = sqrt(1-rho*rho)*sigma2;
  Z(0,0) = 0;
  Z(0,1) = 0;   
  double y = 0, m1 = 0, x=0, m2=0;
  for(int i = 1; i < N; i++) {
    y = Z(i-1, 1);
    m1 = mu1 + rho * (y - mu2) * sigma1/sigma2;
    Z(i, 0) = rnorm(1, m1, s1)[0];
    x = Z(i, 0);
    m2 = mu2 + rho * (x - mu1) * sigma2/sigma1;
    Z(i, 1) = rnorm(1, m2, s2)[0];
  }
  return(Z);
}