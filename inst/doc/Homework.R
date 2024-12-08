## -----------------------------------------------------------------------------
# 抽样
n <- 5000
flag <- rbinom(n,1,0.5)
sample_Z <- c()
for (i in flag){
    if (i==1){
        sample_Z <- c(sample_Z,rnorm(1,3,1))
    }else{
        sample_Z <- c(sample_Z,rnorm(1,6,1))
    }
}
# 绘图
pdf_Z <- function(x){
    return((dnorm(x,3,1)+dnorm(x,6,1))/2)
}
library(ggplot2)
step.x <- seq(0,10,0.1)
df <- data.frame(x=step.x,y=pdf_Z(step.x))
p <- ggplot(data.frame(x=sample_Z),aes(x=x,y=after_stat(density)))+
     geom_histogram(aes(color='样本直方图',),
                    binwidth = 0.25, 
                    alpha=0.5,
                    boundary=1  # 该参数能够调整 bin 矩形柱 的位置
                   )+
     geom_line(data=df,aes(x=x,y=y,col='实际密度函数'))+
     scale_color_manual(values = c('red', 'black'), name='')
p

## -----------------------------------------------------------------------------
library(ggplot2) # 导入绘图包
set.seed(2)  # 设置随机种子
x <- rnorm(10000)  # 从标准正态分布抽取 10000 个样本
y <- pnorm(x)  # y = F(x)
p <- ggplot(data.frame(x=y),aes(x=x,y=after_stat(density)))+
     geom_histogram(binwidth = 0.05, 
                    alpha=0.5,
                    color='black',
                    boundary=0  # 该参数能够调整 bin 矩形柱 的位置
                   )
p

## -----------------------------------------------------------------------------
num <- 0  # 计数 所求随机变量
all.num <- 0  # 计数 全部
alpha <- 1
M <- 5
n <- 1000
func.ni <- function(u,alpha){
    # Double-Exponential分布的逆变换公式
    if (u < 0.5){
        log(2*u)/alpha
    }else{
        -log(2-2*u)/alpha
    }
}

gx <- function(x,alpha){alpha/2*exp(-alpha*abs(x))}  # Double-Exponential分布的密度函数
x <- c()
while(num<n){
    all.num <- all.num+1
    
    y <- func.ni(runif(1),alpha)
    u <- M*runif(1)*gx(y,alpha)
    if (u<=dnorm(y)){
        x <- c(x,y)
        num <- num+1
    }
}
# library(ggplot2)
step.x <- seq(-4,4,0.1)
df <- data.frame(x=step.x,y=dnorm(step.x))
p <- ggplot(data.frame(x=x),aes(x=x,y=after_stat(density)))+
     geom_histogram(aes(color='样本直方图',),
                    binwidth = 0.25, 
                    alpha=0.5,
                    boundary=1  # 该参数能够调整 bin 矩形柱 的位置
                   )+
     geom_line(data=df,aes(x=x,y=y,col='实际密度函数'))+
     scale_color_manual(values = c('red', 'black'), name='')
p

## -----------------------------------------------------------------------------
func_inverse <- function(u,sigma){
    # 分布函数的逆函数
    sqrt(-2*sigma^2*log(1-u))
}
func_density <- function(x,sigma){
    # 密度函数
    x/sigma^2*exp(-x^2/2/sigma^2)
}
func_paint <- function(sample.x,sigma){
    # 绘图
    x.min <- min(sample.x)
    x.max <- max(sample.x)
    step.x <- seq(x.min,x.max,0.1)
    df <- data.frame(x=step.x,y=func_density(step.x,sigma))
    p <- ggplot(data.frame(x=sample.x),aes(x=x,y=after_stat(density)))+
         geom_histogram(aes(color='样本直方图',),
                        binwidth = 0.25, 
                        alpha=0.5,
                        boundary=1  # 该参数能够调整 bin 矩形柱 的位置
                       ) +
         geom_line(data=df,aes(x=x,y=y,col='实际密度函数'))+
         scale_color_manual(values = c('red', 'black'), name=paste0('sigma=',sigma))
    return(p)
}

## -----------------------------------------------------------------------------
library(ggplot2)
set.seed(1)
num <- 10000
u <- runif(num)
# example 1
sigma <- 1
sample.1 <- func_inverse(u,sigma)
func_paint(sample.1,sigma)

# example 2
sigma <- 4
sample.2 <- func_inverse(u,sigma)
func_paint(sample.2,sigma)

# example 3
sigma <- 7
sample.3 <- func_inverse(u,sigma)
func_paint(sample.3,sigma)

## -----------------------------------------------------------------------------
func_sample <- function(p1){
    # 根据概率 p1 抽样
    # 返回样本及相应密度函数
    num <- 1000
    flag <- rbinom(num,1,p1)
    sample.x <- c()
    for (i in flag){
        if (i==1){
            sample.x <- c(sample.x,rnorm(1,0,1))
        }else{
            sample.x <- c(sample.x,rnorm(1,3,1))
        }
    }
    pdf_x <- function(x){
        p1*dnorm(x,0,1)+(1-p1)*dnorm(x,3,1)
    }
    return(list(sample=sample.x,func=pdf_x))
}
func_paint2 <- function(sample.x,func){
    # 绘图
    x.min <- min(sample.x)
    x.max <- max(sample.x)
    step.x <- seq(x.min,x.max,0.1)
    df <- data.frame(x=step.x,y=func(step.x))
    p <- ggplot(data.frame(x=sample.x),aes(x=x,y=after_stat(density)))+
         geom_histogram(aes(color='样本直方图',),
                        binwidth = 0.25, 
                        alpha=0.5,
                        boundary=1  # 该参数能够调整 bin 矩形柱 的位置
                       ) +
         geom_line(data=df,aes(x=x,y=y,col='实际密度函数'))+
         scale_color_manual(values = c('red', 'black'), name='')
    return(p)
}

## -----------------------------------------------------------------------------
p1 <- 0.75
out <- func_sample(p1)
sample.x <- out$sample
func_paint2(sample.x,out$func)

p1 <- 0.5
out <- func_sample(p1)
sample.x <- out$sample
func_paint2(sample.x,out$func)

p1 <- 0.3
out <- func_sample(p1)
sample.x <- out$sample
func_paint2(sample.x,out$func)

p1 <- 0.8
out <- func_sample(p1)
sample.x <- out$sample
func_paint2(sample.x,out$func)

## -----------------------------------------------------------------------------
# parameters
T <- 1000
lambda <- 2
alpha <- 2
beta <- 1
# generate
N_part <- rpois(T,lambda)
N.T <- cumsum(N_part)
Y <- rgamma(N.T[T],alpha,beta)
X.t <- c()
for (i in 1:T){
    X.t <- c(X.t,sum(Y[1:N.T[i]]))
}
# 绘图
plot(X.t,xlab = 'T')

## -----------------------------------------------------------------------------
FuncForExercise3 <- function(theta){
    # theta <- c(lambda,alpha,beta)
    # 生成样本并估计 X_10 的均值和期望，同时输出理论结果
    lambda <- theta[1]
    alpha <- theta[2]
    beta <- theta[3]
    t <- 10
    num <- 5000
    N_10 <- rpois(num,t*lambda)
    X_10 <- c()
    for (i in 1:num){
        Y_list <- rgamma(N_10[i],alpha,beta)
        X_10 <- c(X_10,sum(Y_list))
    }
    # 估计值
    estimate.mean <- mean(X_10)
    estimate.var <- var(X_10)
    # 理论值
    X_10.mean <- lambda*t*alpha/beta
    X_10.var <- lambda*t*(alpha/beta^2+alpha^2/beta^2)
    out_li <- list(mean.est=estimate.mean,var.est=estimate.var,
                   mean.real=X_10.mean,var.real=X_10.var)
    return(out_li)
}

## -----------------------------------------------------------------------------
# 参数1
theta <- c(2,2,1)
FuncForExercise3(theta)
# 参数2
theta <- c(4,3,2)
FuncForExercise3(theta)
# 参数3
theta <- c(7,8,5)
FuncForExercise3(theta)

## -----------------------------------------------------------------------------
n <- 10000
x_li <- seq(0.1,0.9,0.1)
hat.Fx <- numeric(9)
for (index in 1:9){
    x <- x_li[index]
    t_li <- runif(n,0,x)
    hat.Fx[index] <- mean(t_li^2*(1-t_li)^2)*30*x
}
Fx <- pbeta(x_li,3,3)
data.frame(Fx=Fx,hat.Fx=hat.Fx)
plot(Fx,hat.Fx)
abline(0,1,col='red')

## -----------------------------------------------------------------------------
func_inverse <- function(u, sigma){
    sqrt(-2*sigma^2*log(1-u))
}

sigma <- 1
n <- 10000
u <- runif(n)
X <- func_inverse(u,sigma)
X.t <- func_inverse(1-u,sigma)

X1 <- func_inverse(u,sigma)
u2 <- runif(n)
X2 <- func_inverse(u2,sigma)
v1 <- var((X+X.t)/2)
v2 <- var((X1+X2)/2)
(v2-v1)/v2
c(v1,v2)

## -----------------------------------------------------------------------------
sigma <- 5
n <- 10000
u <- runif(n)
X <- func_inverse(u,sigma)
X.t <- func_inverse(1-u,sigma)

X1 <- func_inverse(u,sigma)
u2 <- runif(n)
X2 <- func_inverse(u2,sigma)
v1 <- var((X+X.t)/2)
v2 <- var((X1+X2)/2)
(v2-v1)/v2

## -----------------------------------------------------------------------------
m <- 1000
n <- 5000
A.hat.1 <- A.hat.2 <- numeric(m)
for (i in 1:m){
    u <- runif(n)
    x1 <- func_inverse(u,1)
    x2 <- rnorm(n)
    A.hat.1[i] <- mean(x1*(x1>1))/sqrt(2*pi)
    A.hat.2[i] <- mean(x2^2*(x2>1))
}
c(var(A.hat.1),var(A.hat.2))

## -----------------------------------------------------------------------------
c(mean(A.hat.1),mean(A.hat.2))

## -----------------------------------------------------------------------------
quicksort <- function(x){
  if(length(x)<=1)return(x)
    x0 <- x[1]
    loc <- 1
    low <- 1
    n <- length(x)
    high <- n
    while(low != high){
        if(loc == low){
            if(x[high] < x[loc]){
                tmp <- x[high]
                x[high] <- x[loc]
                x[loc] <- tmp
                low = low+1
                loc = high
            }else{
                high = high - 1
            }
        }else{
            if(x[low] > x[loc]){
                tmp <- x[low]
                x[low] <- x[loc]
                x[loc] <- tmp
                high = high -1
                loc = low
            }else{
                low = low+1
            }
        }
    }
    L = c()
    R = c()
    if(low>1) L = x[1:(low-1)]
    if(low<length(x)) R = x[(low+1):n]
    return(c(quicksort(L),x[low],quicksort(R)))
}
fast_sorted <- function(n){
    # 生成 1~n 个乱序不重复数字
    random_num <- sample(1:n,n)
    start.time <- as.numeric(Sys.time())
    
    quicksort(random_num)
    
    end.time <- as.numeric(Sys.time())
    return(end.time-start.time)
}
n_li <- c(1e4,2e4,4e4,6e4,8e4)
a_n <- numeric(5)  # 长度为 5 的零向量
m <- 100
for (index in 1:5){
    n <- n_li[index]
    t_li <- numeric(m)
    for (i in 1:m){
        t_li[i] <- fast_sorted(n)
    }
    a_n[index] <- mean(t_li)
}
md <- lm(a_n~n_li*log(n_li))
summary(md)

## -----------------------------------------------------------------------------
plot(n_li,a_n)
lines(n_li, predict(md,data.frame(x=n_li*log(n_li))), col='red')

## -----------------------------------------------------------------------------
get_data <- function(n,mu,sigma){rnorm(n,mu,sigma)}
calculate_data <- function(sample.x){
    mu <- mean(sample.x)
    sigma <- sd(sample.x)
    SK <- 1/n/sigma^3*sum((sample.x-mu)^3)
    return(SK)
}
show_data <- function(SK.li, q.li, m, n){
    hat.SK.q <- quantile(SK.li,q.li)
    sd.Sk.q <- sqrt(q.li*(1-q.li)/m/dnorm(hat.SK.q)^2)
    SK.q_large <- sqrt(6/n)*qnorm(q.li)
    return (data.frame('large.sample'=SK.q_large,'estimate'=hat.SK.q,'estimate.sd'=sd.Sk.q))
}
m <- 1000
n <- 50000
mu <- 0
sigma <- 1
hat.SK <- numeric(m)
q.li <- c(0.025, 0.05, 0.95, 0.975)
for (j in 1:m){
    sample.j <- get_data(n,mu,sigma)
    hat.SK[j] <- calculate_data(sample.j)
}
show_data(hat.SK,q.li,m,n)

## -----------------------------------------------------------------------------
get_data <- function(n){
    x <- rnorm(n)
    y <- rnorm(n)
    return(list(x=x,y=y))
}
calculate_data <- function(sample.x,sample.y){
    x <- sample.x
    y <- sample.y
    p1 <- cor.test(x,y,method='pearson')$p.value
    p2 <- cor.test(x,y,method='kendall')$p.value
    p3 <- cor.test(x,y,method='spearman')$p.value
    return(list(p1=p1,p2=p2,p3=p3))
}

show_data <- function(p1.li,p2.li,p3.li, alpha){
    c(mean(p1.li<=alpha),mean(p2.li<=alpha),mean(p3.li<=alpha))
}
m <- 5e4
n <- 10
alpha <- 0.05
p1 <- p2 <- p3 <- numeric(m)
for (j in 1:m){
    sample.j <- get_data(n)
    x <- sample.j$x
    y <- sample.j$y
    p.j <- calculate_data(x,y)
    p1[j] <- p.j$p1
    p2[j] <- p.j$p2
    p3[j] <- p.j$p3
}
show_data(p1,p2,p3,alpha)

## -----------------------------------------------------------------------------
get_data <- function(n){
    y <- rexp(n)
    x <- 1 + 1.5*y^{0.5} + rt(n,2)
    return(list(x=x,y=y))
}
m <- 5e3
n <- 100
alpha <- 0.05
p1 <- p2 <- p3 <- numeric(m)
for (j in 1:m){
    sample.j <- get_data(n)
    x <- sample.j$x
    y <- sample.j$y
    p.j <- calculate_data(x,y)
    p1[j] <- p.j$p1
    p2[j] <- p.j$p2
    p3[j] <- p.j$p3
}
show_data(p1,p2,p3,alpha)

## -----------------------------------------------------------------------------
n <- 10000
x.bar <- 0.651
y.bar <- 0.676
p.hat <- 0.6635
z <- (x.bar-y.bar)/sqrt(2*p.hat*(1-p.hat)/n)
p.value <- 2*min(pnorm(z),1-pnorm(z))
p.value

## -----------------------------------------------------------------------------
calculate.Bonferroni <- function(p0.value, pa.value, alpha){
    n0 <- length(p0.value)
    na <- length(pa.value)
    N <- n0+na
    V <- sum(p0.value<alpha/N)
    # U <- n0-V
    S <- sum(pa.value<alpha/N)
    # T <- na-S
    TPR <- S/na
    FDR <- V/(V+S)
    FWER <- 0
    if (V>=1) FWER <- 1
    return(c(FWER,FDR,TPR))
}
calculate.BH <- function(p0.value, pa.value, alpha){
    new.p <- p.adjust(p=c(p0.value,pa.value),method = 'BH')
    n0 <- length(p0.value)
    na <- length(pa.value)
    V <- sum(new.p[1:n0]<alpha)
    S <- sum(new.p[(n0+1):(n0+na)]<alpha)
    TPR <- S/na
    FDR <- V/(V+S)
    FWER <- 0
    if (V>=1) FWER <- 1
    return(c(FWER,FDR,TPR))
}
one.simulation <- function(){
    N <- 1000
    n0 <- 950
    na <- 50
    alpha <- 0.1
    p0.value <- runif(n0)
    pa.value <- rbeta(na,0.1,1)
    out1 <- calculate.Bonferroni(p0.value,pa.value,alpha)
    out2 <- calculate.BH(p0.value,pa.value,alpha)
    return(c(out1,out2))
}
show.table <- function(nm, row_names, column_names){
    nv <- colMeans(nm)
    out.m <- matrix(nv,nrow = 3, ncol=2)
    colnames(out.m) <- column_names
    rownames(out.m) <- row_names
    return(out.m)
}
row_names <- c('FWER', 'FDR', 'TPR')
column_names <- c('Bonferroni correction', 'B-H correction')
set.seed(10086)
m <- 10000
need.matrix <- matrix(0,ncol = 6,nrow=m)
for (i in 1:m){
    need.vector <- one.simulation()
    need.matrix[i,] <- need.vector
}
out.table <- show.table(need.matrix,row_names, column_names)
out.table

## -----------------------------------------------------------------------------
library(boot)
data <- as.vector(unlist(aircondit))
B <- 10000; set.seed(12345);thetastar <- numeric(B)
theta <- 1/mean(data)
for(b in 1:B){
    d1.star <- sample(data,replace=TRUE)
    thetastar[b] <- 1/mean(d1.star)
}
round(c(mean.boot=mean(thetastar),bias=mean(thetastar)-theta,se.boot=sd(thetastar)),4)

## -----------------------------------------------------------------------------
B <- 10000
boot.theta <- function(x,i) mean(x[i])
de <- boot(data=data,statistic=boot.theta, R = B)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
mean(de$t)
out.data <- function(boot_ci){
    ci <- boot_ci
    d <- c(ci$norm[2:3],ci$basic[4:5],ci$percent[4:5],ci$bca[4:5])
    m <- matrix(d,ncol=2,byrow = TRUE)
    colnames(m) <- c('下界','上界')
    rownames(m) <- c("norm","basic","perc","bca")
    return(m)
}
m <- out.data(ci)
m
rowMeans(m)
m[,2]-m[,1]

## ----eval=F-------------------------------------------------------------------
#  d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460,
#              -0.937, 0.779, -1.409, 0.027, -1.569);
#  d2  <- c(1.608, 1.009,  0.878,  1.600, -0.263,
#               0.680, 2.280,  2.390, 1.793,  8.091, 1.468)

## -----------------------------------------------------------------------------
d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, -0.937, 0.779, -1.409, 0.027, -1.569)
d2 <- c(1.608, 1.009, 0.878, 1.600, -0.263, 0.680, 2.280, 2.390, 1.793, 8.091, 1.468)

B <- 10000; set.seed(12345);thetastar <- numeric(B)
theta <- mean(d1)-mean(d2);
for(b in 1:B){
    d1.star <- sample(d1,replace=TRUE)
    d2.star <- sample(d2,replace=TRUE)
    thetastar[b] <- mean(d1.star)-mean(d2.star)
}
round(c(mean.boot=mean(thetastar),bias=mean(thetastar)-theta,se.boot=sd(thetastar),se.samp=sqrt(var(d1)/length(d1)+var(d2)/length(d2))),3)  

## -----------------------------------------------------------------------------
calculate.func <- function(input.data){
    m <- cov(input.data)
    lambda.vc <- eigen(m)$values
    theta.hat <- lambda.vc[1]/sum(lambda.vc)
    return(theta.hat)
}
library(bootstrap)
n <- 88
theta.hat <- calculate.func(scor)
theta.jack <- numeric(n)
for (i in 1:88){
    theta.jack[i] <- calculate.func(scor[-i,])
}
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- numeric(n)
for (k in 1:n) {
    y <- magnetic[-k]
    x <- chemical[-k]
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[k] <- magnetic[k] - yhat1
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
    e2[k] <- magnetic[k] - yhat2
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[k] <- magnetic[k] - yhat3
    J4 <- lm(y ~ x + I(x^2) + I(x^3))
    yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] +J4$coef[3] * chemical[k]^2+J4$coef[4] * chemical[k]^3
    e4[k] <- magnetic[k] - yhat4
}
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

# adjusted R^2
y <- magnetic
x <- chemical
J1 <- lm(y ~ x)
adj.r.squared1 <- summary(J1)$adj.r.squared
J2 <- lm(y ~ x + I(x^2))
adj.r.squared2 <- summary(J2)$adj.r.squared
J3 <- lm(log(y) ~ x)
adj.r.squared3 <- summary(J3)$adj.r.squared
J4 <- lm(y ~ x + I(x^2) + I(x^3))
adj.r.squared4 <- summary(J4)$adj.r.squared
c(adj.r.squared1,adj.r.squared2,adj.r.squared3,adj.r.squared4)

detach(ironslag)

## -----------------------------------------------------------------------------
attach(chickwts)
all.x <- sort(as.vector(weight[feed == "soybean"]))
all.y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

calculate.statistic <- function(datax,datay){
    n <- length(datax)
    m <- length(datay)
    Fn <- ecdf(datax)
    Gm <- ecdf(datay)
    part1 = sum((Fn(datax)-Gm(datax))^2)
    part2 = sum((Fn(datay)-Gm(datay))^2)

    out <- m*n/(m+n)^2*(part1+part2)
    return(out)
}
R <- 10000
z <- c(all.x, all.y)
n<-length(all.x)
set.seed(1212)
reps <- numeric(R)
t0 <- calculate.statistic(all.x,all.y)
for (i in 1:R) {
    xy <- sample(z);
    x1 <- xy[1:n]
    y1 <- xy[-(1:n)]
    reps[i] <- calculate.statistic(x1,y1)
}
p <- mean(abs(c(t0, reps)) >= abs(t0))
p

## -----------------------------------------------------------------------------
calculate.statistic <- function(datax,datay){
    cor(datax,datay,method = 'spearman')
}
all.x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
all.y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
R <- 9999
z <- c(all.x, all.y)
n<-length(all.x)
set.seed(12345)
reps <- numeric(R)
t0 <- calculate.statistic(all.x,all.y)
for (i in 1:R) {
    reps[i] <- calculate.statistic(all.x,sample(all.y))
}
p <- mean(abs(c(t0, reps)) >= abs(t0))
p
cor.test(all.x,all.y)

## -----------------------------------------------------------------------------
f <- function(x){
    dt(x,1)
}
g.X <- function(y,x){
    dt(y,abs(x)+0.1)
}
beta.func <- function(x,y){
    f(y)*g.X(x,y)/(f(x)*g.X(y,x))
}
N <- 5000
X <- numeric(N)
X[1] <- rt(1,1)
for (i in 2:N){
    x0 <- X[i-1]
    Y <- rt(1,abs(x0)+0.1)
    U <- runif(1)
    beta <- beta.func(x0,Y)
    if (U<=beta){
        X[i] <- Y
    }else{
        X[i] <- x0
    }
}
quantile(X[3000:5000],(1:10)/10)
qcauchy((1:10)/10)

## -----------------------------------------------------------------------------
n <- 10
a <- 2
b <- 5
N <- 3000
x.vc <- numeric(N)
y.vc <- numeric(N)
x.vc[1] <- 1

y.vc[1] <- rbeta(1,1+a,n-1+b)
for (i in 2:N){
    xi <- rbinom(1,10,y.vc[i-1])
    x.vc[i] <- xi
    y.vc[i] <- rbeta(1,xi+a,n-xi+b)
}
# par(mfcol=c(2,1))
plot(x.vc,type='l',col='red')
plot(y.vc,type='l')
# par(mfcol=c(1,2))
hist(x.vc)
hist(y.vc)

## -----------------------------------------------------------------------------
# Exercise 1
N <- 4000

calculate.X <- function(N, alpha){
    f <- function(x){
        dt(x,1)
    }
    g.X <- function(y,x){
        dt(y,abs(x)+alpha)
    }
    beta.func <- function(x,y){
        f(y)*g.X(x,y)/(f(x)*g.X(y,x))
    }
    X <- numeric(N)
    X[1] <- rt(1,1)
    for (i in 2:N){
        x0 <- X[i-1]
        Y <- rt(1,abs(x0)+alpha)
        U <- runif(1)
        beta <- beta.func(x0,Y)
        if (U<=beta){
            X[i] <- Y
        }else{
            X[i] <- x0
        }
    }
    phi <- cumsum(X)/(1:N)
    return(phi)
}

k <- 3
set.seed(12314)
phi1 <- calculate.X(N,1)
phi2 <- calculate.X(N,2)
phi3 <- calculate.X(N,3)

phi.matrix <- matrix(c(phi1,phi2,phi3),ncol=3)
start <- 2
num <- N-start
Rn <- numeric(num)
for (n in start:(N)){
    use.matrix <- phi.matrix[1:n,]
    Bn <- n/(k-1)*sum((colMeans(use.matrix) - mean(use.matrix))^2)
    part.matrix <- t(t(use.matrix) - colMeans(use.matrix))
    Wn <- sum(part.matrix^2)/k/(n-1)
    Rn[n-start+1] <- 1-1/n+1/n*(Bn/Wn)
}
plot(Rn[350:750],type = 'l',ylab='Rn')

## -----------------------------------------------------------------------------
# Exercise 2

calculate.phi <- function(N, alpha){
    n <- 10
    a <- 2
    b <- 5
    # N <- 3000
    x.vc <- numeric(N)
    y.vc <- numeric(N)
    x.vc[1] <- alpha
    
    y.vc[1] <- rbeta(1,alpha+a,n-alpha+b)
    for (i in 2:N){
        xi <- rbinom(1,10,y.vc[i-1])
        x.vc[i] <- xi
        y.vc[i] <- rbeta(1,xi+a,n-xi+b)
    }
    phi.x <- cumsum(x.vc)/(1:N)
    phi.y <- cumsum(y.vc)/(1:N)
    return(list(phi.x=phi.x,phi.y=phi.y))
}
k <- 3
N <- 3000
set.seed(12314)
out <- calculate.phi(N,1)
phi.y1 <- out$phi.y
phi.x1 <- out$phi.x
out <- calculate.phi(N,2)
phi.y2 <- out$phi.y
phi.x2 <- out$phi.x
out <- calculate.phi(N,3)
phi.y3 <- out$phi.y
phi.x3 <- out$phi.x

phi.x.matrix <- matrix(c(phi.x1,phi.x2,phi.x3),ncol=3)
phi.y.matrix <- matrix(c(phi.y1,phi.y2,phi.y3),ncol=3)

start <- 2
num <- N-start
Rn <- numeric(num)
for (n in start:(N)){
    use.x.matrix <- phi.x.matrix[1:n,]
    use.y.matrix <- phi.y.matrix[1:n,]
    Bn.x <- n/(k-1)*sum((colMeans(use.x.matrix) - mean(use.x.matrix))^2)
    Bn.y <- n/(k-1)*sum((colMeans(use.y.matrix) - mean(use.y.matrix))^2)
    Bn <- (Bn.x+Bn.y)/2
    part.x.matrix <- t(t(use.x.matrix) - colMeans(use.x.matrix))
    part.y.matrix <- t(t(use.y.matrix) - colMeans(use.y.matrix))
    Wn.x <- sum(part.x.matrix^2)/k/(n-1)
    Wn.y <- sum(part.y.matrix^2)/k/(n-1)
    Wn <- (Wn.x+Wn.y)/2
    Rn[n-start+1] <- 1-1/n+1/n*(Bn/Wn)
}
plot(Rn[100:500],type = 'l',ylab='Rn')

## -----------------------------------------------------------------------------
calculate.k <- function(k, a){
    d <- length(a)
    log.part <- (k+1)*log(sum(a^2))+lgamma((d+1)/2)+lgamma(k+3/2)-lfactorial(k)-k*log(2)-log(2*k+1)-log(2*k+2)-lgamma(k+d/2+1)
    return((-1)^k*exp(log.part))
}
calculate.k(c(0,1,2,3),c(3,4,5))

## -----------------------------------------------------------------------------
sum.to.k <- function(end.k,a){
    part <- calculate.k(c(0:end.k),a)
    return(list(sum=sum(part),part=cumsum(part)))
}

## -----------------------------------------------------------------------------
a <- c(1,2)
out <- sum.to.k(50,a)
out$sum
plot(out$part,type='l',ylab = 'sum')

## -----------------------------------------------------------------------------
my.function <- function(a,k){
    if(a==0){return(1000)}
    c.k <- function(a,k){sqrt(a^2*k/(k+1-a^2))}
    f.u <- function(u,k){(1+u^2/(k))^(-(k+1)/2)}
    left <- integrate(f.u,lower=0,upper=c.k(a,k-1),k=k-1)$value*2/sqrt(pi*(k-1))*exp(lgamma(k/2)-lgamma((k-1)/2))
    right <- integrate(f.u,lower=0,upper=c.k(a,k),k=k)$value*2/sqrt(pi*(k))*exp(lgamma((k+1)/2)-lgamma((k)/2))
    return(left-right)
}
k.li <- c(4:25,100,500,1000)
root.li <- numeric(length(k.li))
for (i in 1:length(k.li)){
    root.li[i] <- uniroot(my.function,c(1e-4,2),k=k.li[i])$root
}
# Exercise 4
func.4 <- function(a,k){
    c.k <- function(a,k){sqrt(a^2*k/(k+1-a^2))}
    out <- pt(c.k(a,k-1),df=k-1)-pt(c.k(a,k),df=k)
    return(out)
}

root.li.4 <- numeric(length(k.li))
for (i in 1:length(k.li)){
    root.li.4[i] <- uniroot(func.4,c(1e-4,2),k=k.li[i])$root
}
data.frame(k=k.li,'a.k'=root.li,'a-k-4'=root.li.4,error=root.li-root.li.4)

## -----------------------------------------------------------------------------
xdata <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

N <- 10
n1 <- 7
n2 <- 3
T0 <- c(0.54, 0.48, 0.33, 0.43, 0.91, 0.21, 0.85)
iter.num <- 500
lambda <- c(mean(xdata),numeric(iter.num))
for (i in 1:iter.num){
    part <- lambda[i]
    lambda[i+1] <- (sum(T0)+n2*(part+1)/part^2)/N
}
lambda[iter.num+1]
plot(lambda,type='l')

## -----------------------------------------------------------------------------
lambda.MLE <- (sum(T0)+3)/7
lambda.MLE

## ----warning = FALSE----------------------------------------------------------
library(lpSolve)
f.obj <- c(4,2,9)
f.con <- matrix (c(2, 1, 1, 1, -1, 3,1,0,0,0,1,0,0,0,1), nrow=5, byrow=TRUE)
f.dir <- c("<=", "<=", ">=", ">=", ">=")
f.rhs <- c(2, 3, 0, 0, 0)
solution <- lp("min", f.obj, f.con, f.dir, f.rhs)
solution$solution
solution

## ----warning = FALSE----------------------------------------------------------
library(dplyr)  # %>% 
formulas <- list(
    mpg ~ disp,
    mpg ~ I(1 / disp),
    mpg ~ disp + wt,
    mpg ~ I(1 / disp) + wt
)
li.md.1 <- list()
for (i in 1:4){
    formulas[[i]] %>% lm(mtcars) -> li.md.1[[i]]
}
li.md.2 <- lapply(formulas, lm, data=mtcars)
li.md.1 %>% head(2)
li.md.2 %>% head(2)

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
    rows <- sample(1:nrow(mtcars), rep = TRUE)
    mtcars[rows, ]
})
li.md.3 <- list()
for (i in 1:10){
    bootstraps[[i]] %>% lm(formula=mpg ~ disp) -> li.md.3[[i]]
}
bootstraps %>% lapply(lm,formula=mpg ~ disp) -> li.md.4
li.md.3 %>% head(2)
li.md.4 %>% head(2)

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
for (i in 1:4){
    paste0('li.md.',i) %>% parse(text = .) %>% eval() %>% lapply(rsq) %>% unlist() %>% head(4) %>% print()
}

## -----------------------------------------------------------------------------
trials <- replicate(
    100,
    t.test(rpois(10, 10), rpois(7, 10)),
    simplify = FALSE
)
sapply(trials,function(i){i$p.value}) -> p.value.1
p.value.1 %>% head(5)
sapply(trials,'[[','p.value') -> p.value.2
p.value.2 %>% head(5)

## -----------------------------------------------------------------------------
my.lapply <- function(X, FUN, FUN.VALUE){
    out1 <- Map(FUN,X)
    vapply(out1,identity,FUN.VALUE=FUN.VALUE)
}
x <- c(1,2,3,4)
my.lapply(x,function(i)i^2,FUN.VALUE = numeric(1))
sapply(x,function(i)i^2)

## -----------------------------------------------------------------------------
my.chisq.test <- function(x, y) {
  all.data <- rbind(x, y)
  margin1 <- rowSums(all.data)
  margin2 <- colSums(all.data)
  all.sum <- sum(all.data)
  part <- tcrossprod(margin1, margin2) / all.sum

  x.stat <- sum((all.data - part)^2 / part)
  df <- (length(margin1) - 1) * (length(margin2) - 1)
  p.value <- pchisq(x.stat, df = df, lower.tail = FALSE)

  list(x.stat = x.stat, df = df, p.value = p.value)
}
x <- c(1:6)
y <- c(6,9,10,5,22,2)
my.chisq.test(x,y)
chisq.test(cbind(x,y))

## -----------------------------------------------------------------------------
my.table <- function(x, y){
  
  x.x <- sort(unique(x))
  y.y <- sort(unique(y))
  
  x.l <- length(x.x)
  y.l <- length(y.y)
  
  dims <- c(x.l, y.l)
  pr <- x.l * y.l
  dn <- list(x = x.x, y = y.y)
  
  bin <- match(x, x.x) +
    x.l * match(y, y.y) - x.l
  part <- tabulate(bin, pr)
  
  out <- array(part, dim = dims, dimnames = dn)
  class(out) <- "table"
  return(out)
}

a <- sample(5, 100, TRUE)
b <- sample(5, 100, TRUE)
my.table(a, b)

## -----------------------------------------------------------------------------
library(Rcpp)
cf = "NumericMatrix gibbsC(int N, int thin, int n, int a, int b) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rbinom(1,n,y)[0];
      y = rbeta(1,x+a,n-x+b)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}"
cppFunction(cf)
n <- 10
N <- 10000
thin <- 20
a <- 2
b <- 5
sample.data <- gibbsC(N,thin,n,a,b)
head(sample.data)

## -----------------------------------------------------------------------------
func.R <- function(N,thin,n,a,b){
    sample.data.R <- matrix(nrow = N,ncol=2)
    x <- y <- 0
    for(i in 1:N) {
        for(j in 1: thin) {
          x = rbinom(1,n,y)[1];
          y = rbeta(1,x+a,n-x+b)[1];
        }
        sample.data.R[i, 1] = x;
        sample.data.R[i, 2] = y;
    }
    return(sample.data.R)
}
sample.data.R <- func.R(N,thin,n,a,b)
qqplot(sample.data[,1],sample.data.R[,1])
qqplot(sample.data[,2],sample.data.R[,2])

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(meanR=func.R(N,thin,n,a,b),meanC=gibbsC(N,thin,n,a,b))
summary(ts)[,c(1,3,5,6)]

