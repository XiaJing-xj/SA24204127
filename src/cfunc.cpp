#include <Rcpp.h>
using namespace Rcpp;


//' @title 使用Rcpp的外罚函数法
//' @description 使用Rcpp的外罚函数法
//' @param x0 初始值
//' @param lossf 目标函数
//' @param func1 步骤一函数
//' @param func2 步骤二函数
//' @param epsilon 算法参数
//' @param sigma 算法参数
//' @param gamma 算法参数
//' @param MAX_ITER 最大迭代次数
//' @return 最优值
//' @export
// [[Rcpp::export]]
NumericVector my_CppFunction(NumericVector x0, 
                             Function lossf, 
                             Function func1, 
                             Function func2, 
                             float epsilon=1e-6, 
                             float sigma=0.5, 
                             float gamma=1.1, 
                             int MAX_ITER = 1000) {
  NumericVector xnew = x0;
  for(int i = 0; i < MAX_ITER; i++){
    xnew = func1(xnew,sigma,lossf);
    LogicalVector flag = func2(xnew,epsilon/sigma);
    if (flag[0]){
      break;
    }
    sigma = gamma*sigma;
  }
  return xnew;
}