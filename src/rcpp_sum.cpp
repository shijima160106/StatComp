#include <Rcpp.h>
using namespace Rcpp;
//' @name rcpp_sum
//' @title A sum function using Rcpp
 //' @description A sum function using Rcpp
 //' @param v the vector to be calculated
 //' @return the sum of the vector
 //' @examples
 //' \dontrun{
 //' rcpp_sum(1:10)
 //' }
 //' @export
 // [[Rcpp::export]]
 double rcpp_sum(NumericVector v){
   double sum = 0;
   for(int i=0; i<v.length(); ++i){
     sum += v[i];
   }
   return(sum);
 }