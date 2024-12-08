## ----eval=FALSE---------------------------------------------------------------
#  my_RFunction <- function(x0, lossf, need_func1, need_func2,
#                           epsilon=1e-6, sigma=0.5, gamma=1.1,
#                           MAX_ITER = 1000) {
#      xnew = x0
#      for(i in 1:MAX_ITER){
#          xnew = need_func1(xnew,sigma,lossf)
#          if (need_func2(xnew,epsilon/sigma)){break}
#          sigma = gamma*sigma
#      }
#      return(xnew)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  LMA.Function <- function(y, xmatrix, model.list=NULL, use.func=my_RFunction){
#      out1 <- step.1.function(y=y, xmatrix=xmatrix, model.list=model.list)
#      error.matrix <- out1[[1]]
#      s <- out1[[2]]
#      w0 <- out1[[3]]
#      loss.func <- function(w){loss.ln(w=w,error.matrix=error.matrix,s=s)}
#      out.w <- use.func(w0, loss.func, func1, func2)
#      return(out.w)
#  }

## ----eval=TRUE----------------------------------------------------------------
n <- 50
p <- 11
alpha <- 0.1
theta <- numeric(p)
for (i in 1:p){
    theta[i] <- sqrt(2*alpha)*i^(-alpha-0.5)
}
set.seed(24204127)
xmatrix <- matrix(nrow=n, ncol = p)
xmatrix[,1] <- 1
xmatrix[,-1] <- matrix(rnorm(n*(p-1)),nrow=n)
error <- rnorm(n)
y <- as.vector(xmatrix %*% theta + error)

## ----eval=TRUE----------------------------------------------------------------
library(SA24204127)
## R 函数
w <- LMA.Function(y,xmatrix)
## C++ 函数
wcpp <- LMA.Function(y,xmatrix,use.func = my_CppFunction)

## ----eval=TRUE----------------------------------------------------------------
# 比较
library(microbenchmark)
ts <- microbenchmark(
  meanR=LMA.Function(y,xmatrix),
  meanC=LMA.Function(y,xmatrix,use.func = my_CppFunction)
  )
summary(ts)[,c(1,3,5,6)]

