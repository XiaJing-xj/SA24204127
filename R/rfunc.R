


step.1.function <- function(y, xmatrix, model.list=NULL){
  n <- nrow(xmatrix)
  p <- ncol(xmatrix)
  if (is.null(model.list)){
    model.list <- list()
    for (m in 1:p){model.list[[m]] <- 1:m}
  }
  M <- length(model.list)
  error.matrix <- matrix(nrow=n,ncol=M)
  s <- numeric(M)
  for (i in 1:M){
    xindex <- model.list[[i]]
    xm <- xmatrix[,xindex]
    pm <- xm %*% solve(t(xm)%*%xm) %*% t(xm)
    s[i] <- sum(diag(pm))
    error.matrix[,i] <- y - pm %*% y
  }
  w0 <- rep(1,M)/M
  return(list(error.matrix, s, w0))
}


loss.ln <- function(w,error.matrix,s){
  n <- nrow(error.matrix)
  # 1*1 矩阵
  part1.matrix <- t(w) %*% t(error.matrix) %*% error.matrix %*% w
  part2.matrix <- t(w) %*% s
  out <- part1.matrix[1,]*(1+2*part2.matrix[1,]/n)
  return(out)
}


p.bar.func <- function(x){
  part <- x[x<0]
  part2 <- (sum(x)-1)**2+sum(part**2)
  return(part2)
}
p.func <- function(x,need.f,need.sigma){
  return(need.f(x)+need.sigma*p.bar.func(x))
}

#' @importFrom stats optim
func1 <- function(x0,sigma,f.loss){
  out_li <- optim(x0,p.func, need.f=f.loss, need.sigma=sigma)
  return(out_li[[1]])
}
func2 <- function(xnew, epsilon){
  p.bar.func(xnew) <= epsilon
}

#' @title 使用R的外罚函数法
#' @description 使用R的外罚函数法
#' @param x0 初始值
#' @param lossf 目标函数
#' @param need.func1 步骤一函数
#' @param need.func2 步骤二函数
#' @param epsilon 算法参数
#' @param sigma 算法参数
#' @param gamma 算法参数
#' @param MAX_ITER 最大迭代次数
#' @return 最优值
#' @export
my_RFunction <- function(x0, lossf, need.func1=func1, need.func2=func2, epsilon=1e-6, sigma=0.5, gamma=1.1, MAX_ITER = 1000) {
  xnew = x0
  for(i in 1:MAX_ITER){
    xnew = need.func1(xnew,sigma,lossf)
    if (need.func2(xnew,epsilon/sigma)){break}
    sigma = gamma*sigma
  }
  return(xnew)
}


#' @title 基于GCV的最小二乘模型平均
#' @description 最小化损失函数求模型平均的权重
#' @param y n*1的向量，自变量
#' @param xmatrix n*p的矩阵，因变量
#' @param model.list 列表，即list(v1,v2,...,vM)，列表元素为向量，每一个向量代表一个模型，vi对应的模型使用的自变量为 xmatrix[,vi]；默认为 NULL，等价于 list(c(1),c(1,2),...,c(1,2,...,p))
#' @param use.func 函数，使用的最优化函数，my_RFunction 或 my_CppFunction
#' @return M*1的向量，即model.list中模型对应的权重
#' @export
LMA.Function <- function(y, xmatrix, model.list=NULL, use.func=my_RFunction){
  out1 <- step.1.function(y=y, xmatrix=xmatrix, model.list=model.list)
  error.matrix <- out1[[1]]
  s <- out1[[2]]
  w0 <- out1[[3]]
  loss.func <- function(w){loss.ln(w=w,error.matrix=error.matrix,s=s)}
  out.w <- use.func(w0, loss.func, func1, func2)
  return(out.w)
}

#' @useDynLib SA24204127
#' @import Rcpp
NULL