## -----------------------------------------------------------------------------
rm(list=ls())
library(knitr)

## ----highlight=T--------------------------------------------------------------
1+1==2

## ----highlight=F--------------------------------------------------------------
1+1==2

## -----------------------------------------------------------------------------
df <- cars
head(df)
kable(head(df),align = 'l')

## -----------------------------------------------------------------------------
plot(lm(speed ~ dist, data = df))
#par(mfrow=c(2,2))
plot(lm(speed ~ dist, data = df))
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
## a function generate random samples from pareto distribution by inverse transform method
rpareto<-function(n,a,b){
  u<-runif(n)
  x<-b*((1-u)^(-1/a))
  return(x)
}

set.seed(20000407)  ##choose a seed for my homework
n<-1000
x<-rpareto(n,2,2)  ##a=b=2
hist(x, prob = TRUE, xlim=c(0,30), main = expression(f(x)==8/(x^3)))
y <- seq(0, max(x), .01)
lines(y, 8/(y^3))

## -----------------------------------------------------------------------------
## the function rho(x) with parameter a and b
rho<-function(x,a,b){
  c<-(a-1)*(b-1)/(a+b-2)
  c<-c^(a+b-2)
  y<-(x^(a-1))*((1-x)^(b-1))/c
  return(y)
}
## a function generate random samples from beta distribution by acceptance-rejection method
my_rbeta<-function(n,a,b){
  k<-0
  y <- numeric(n)
  while (k < n) {
    u <- runif(1)
    x <- runif(1) #random variate from g(.)
    if (rho(x,a,b) > u) {
    #we accept x
      k <- k + 1
      y[k] <- x
    }
  }
  return(y)
}
x<-my_rbeta(1000,3,2)  ##n=1000,a=3,b=2
hist(x, prob = TRUE, xlim=c(0,1), main = expression(f(x)==12*x^2(1-x)))
y <- seq(0, 1, .01)
z<-12*(y^2)*(1-y)
lines(y,z)


## -----------------------------------------------------------------------------
EG<-function(n,r,beta){
  lambda<-rgamma(n,r,beta)
  y<-rexp(n,lambda)
}
Y_eg<-EG(1000,4,2) ##Generate 1000 random observations, r=4,beta=2

## -----------------------------------------------------------------------------
n<-1000
y<-rpareto(n,4,2)-2   ##r=4,beta=2

## -----------------------------------------------------------------------------
hist(y, prob = TRUE, xlim=c(0,6), main = expression(f(y)==64/(2+y)^5))
x <- seq(0, max(y), .01)
lines(x, 64/(2+x)^5)
rm(list=ls())

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)}
  else{
    a<-x[1]  ##Without loss of generality, we take the first number.
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))} ##Nested functions step by step
}

## -----------------------------------------------------------------------------
set.seed(407)  ##a seed for my homework
n<-c(1e4,2e4,4e4,6e4,8e4)
for(i in 1:5){
  test<-sample(1:n[i]) ##randomly permuted numbers of 1,...,n
  quick_sort(test)
}

## -----------------------------------------------------------------------------
a<-numeric(5)
for(i in 1:5){
  a[i]<-0
  for(j in 1:100){  ##100 simulations
    test<-sample(1:n[i])
    a[i]<-a[i]+system.time(quick_sort(test))[1]
  }
  a[i]<-a[i]/100
}
a

## -----------------------------------------------------------------------------
t<-n*log(n)
lm<-lm(a~t)
summary(lm)$coefficients[2, 4]  ##p-value

## -----------------------------------------------------------------------------
b<-lm$coefficients[[1]]  ##intercept
k<-lm$coefficients[[2]]  ##slope
plot(t,a)  ##scatter plot
abline(b,k)  ##regression line

## -----------------------------------------------------------------------------
e<-exp(1)
(2*e^2-6*e+2)/(-e^2+4*e-3)

## -----------------------------------------------------------------------------
MC.theta <- function(R = 10000, antithetic = FALSE) {
  u <- runif(R/2)
  if (antithetic) v <- 1 - u else v <- runif(R/2)
  u <- c(u, v)
  g <- exp(u)
  cdf <- mean(g)
  cdf
}

## -----------------------------------------------------------------------------
set.seed(407)
m <- 1000
MC1 <- MC2 <- Reduction <- numeric(m)
for (i in 1:m) {
  MC1[i] <- MC.theta(R = 1000, antithetic = FALSE) ##simple MC
  MC2[i] <- MC.theta(R = 1000, antithetic = TRUE) ##antithetic
}
c(mean(MC1),mean(MC2))

## -----------------------------------------------------------------------------
(var(MC1)-var(MC2))/var(MC1)

## -----------------------------------------------------------------------------
rm(list=ls())
x <- seq(1,12, .01)
w <- 2
g <- x^2*exp(-x^2/2)/sqrt(2*pi)
f1 <- exp(-x^2/2)/sqrt(2*pi)
f2 <- (x^2)*exp(-x)/2
gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
        expression(f[1](x)==e^{-x^2/2}/sqrt(2*pi)),
        expression(f[2](x)==x^2*e^{-x}/2))
#par(mfrow=c(1,2))
##figure A
plot(x, g, type = "l", ylab = "",
     ylim = c(0,0.5), lwd = w,col=1,main='(A)')
lines(x, f1, lty = 2, lwd = w,col=2)
lines(x, f2, lty = 3, lwd = w,col=3)
legend("topright", legend = gs,
       lty = 1:2, lwd = w, inset = 0.02,col=1:3)
##figure B
plot(x, g/f1, type = "l", ylab = "",
     ylim = c(0,10), lwd = w, lty = 2,col=2,main='(B)')
lines(x, g/f2, lty = 3, lwd = w,col=3)
legend("topright", legend = gs[-1],
       lty = 2:3, lwd = w, inset = 0.02,col=2:3)


## -----------------------------------------------------------------------------
set.seed(407) ## seed for my homework
m <- 1e4
est <- va <- numeric(2)
g <- function(x) {
  (x^2)*(exp(-x^2/2))*(x > 1)/sqrt(2*pi)
}
x1 <- rnorm(m) ## f1
fg1 <- g(x1)/(exp(-x1^2/2)/sqrt(2*pi))
est[1] <- mean(fg1)
va[1] <- var(fg1)
x2 <- rgamma(m,shape = 3,scale = 1) ## f2
fg2 <- g(x2) / ((x2^2)*exp(-x2)/2)
est[2] <- mean(fg2)
va[2] <- var(fg2)
## results
res <- rbind(est=round(est,5), va=round(va,5))
colnames(res) <- paste0('f',1:2)
res

## -----------------------------------------------------------------------------
set.seed(407)
M <- 10000
g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
u <- runif(M) ## inverse transform method
x <- -log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
theta.hat <- mean(fg)
se <- sd(fg)

## -----------------------------------------------------------------------------
set.seed(407)
m<-M/5
j<-seq(0,1,0.2)
a<--log(1 - j * (1 - exp(-1)))
theta_si<-var_si<-numeric(5)
for(i in 1:5){  ## stratified sampling, k=5
  g <- function(x) {
    exp(-x - log(1+x^2)) * (x > a[i]) * (x < a[i+1])
  }
  u <- runif(m/5,0,1) ## inverse transform method
  x <- -log(exp(-a[i]) - u * (1 - exp(-1))/5)
  fg <- g(x) / (5*exp(-x) / (1 - exp(-1)))
  theta_si[i] <- mean(fg)
  var_si[i] <- var(fg)
}
theta.si<-sum(theta_si)  
se.si<-sqrt(sum(var_si))
## results
res <- rbind(theta=round(c(theta.hat,theta.si),7), se=round(c(se,se.si),7))
colnames(res) <- paste0(c('Importance','stratified importance'))
res
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(2022) #choose a seed for my homework
## some parameters
m<-1000
n<-20
alpha<-0.05
miu<-1
sigma<-1
CL<-0 #empirical confidence level
## Monte Carlo
for(i in 1:m){
  X<-rlnorm(n,meanlog = miu,sdlog = sigma)
  miu_hat<-mean(log(X))
  S<-sqrt(var(log(X)))
  t<-qt(alpha/2,n-1,lower.tail = F)
  miu_delta<-t*S/sqrt(n)
  CL<-CL+(miu<miu_hat+miu_delta)*(miu>miu_hat-miu_delta)/m
}
CL

## -----------------------------------------------------------------------------
set.seed(2022)
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy)) > 5))
}
n<-c(10,100,1000)
m<-1000
alpha<-0.055
sigma1 <- 1
sigma2 <- 1.5
power<-matrix(0,nrow = 2,ncol = length(n))
for (i in 1:3){
  result <- replicate(m, expr={
    x <- rnorm(n[i], 0, sigma1)
    y <- rnorm(n[i], 0, sigma2)
    Fp <- var.test(x, y)$p.value
    c(count5test(x, y),as.integer(Fp<alpha))
    })
  power[,i]<-rowMeans(result)
}
colnames(power)<-c("n=10","n=100","n=1000")
rownames(power)<-c("Count Five test","F test")
power
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
library(boot)
set.seed(20221020)  #choose a seed for my homework
data<-aircondit[1:12,]  #our data
lambda_mle<- 1/mean(data)  #MLE of lambda
cat("The MLE of the hazard rate lambda is ",round(lambda_mle,4))

## -----------------------------------------------------------------------------
B <- 1000  #times for bootstrap
lambdastar <- numeric(B)
##bootstrap
for(b in 1:B){
  datastar <- sample(data,replace=TRUE)
  lambdastar[b] <- 1/mean(datastar)
}
##result
round(c(lambdastarhat=mean(lambdastar),
        bias=mean(lambdastar)-lambda_mle,
        se.boot=sd(lambdastar)),4)

## -----------------------------------------------------------------------------
set.seed(20221020)
##bootstrap CI of 1/lambda
boot.mean <- function(x,i) mean(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-numeric(2)
de <- boot(data, statistic=boot.mean, R = 999)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci.norm<-ci$norm[2:3];ci.basic<-ci$basic[4:5]
ci.perc<-ci$percent[4:5];ci.bca<-ci$bca[4:5]
res<-matrix(c(ci.norm,ci.basic,ci.perc,ci.bca),ncol = 2,byrow = T)
colnames(res)<-c("2.5%","97.5%")
rownames(res)<-c("normal","basic","percentile","BCa")
res

## -----------------------------------------------------------------------------
##Sample histogram
hist(data,breaks = 10,main = "sample histogram")
##1000 bootstrap histogram with mean, quantile 2.5% and 97.5% in EX7.4
hist(1/lambdastar,main = "bootstrap histogram")
abline(v=mean(1/lambdastar),col=1,lwd=2)
abline(v=quantile(1/lambdastar,0.025),col=2,lwd=2)
abline(v=quantile(1/lambdastar,0.975),col=3,lwd=2)

## -----------------------------------------------------------------------------
mu<-1;sigma<-1;n<-10;set.seed(20221020)
##Repeat 100 times to calculate the coverage of the three methods
m<-100
ci.norm<-ci.basic<-ci.perc<-matrix(NA,m,2)
boot.mean <- function(x,i) mean(x[i])
for(i in 1:m){
  dataA<-rnorm(n,mu,sigma)
  de <- boot(dataA,statistic=boot.mean, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}

##result
cat('Empirical coverage rates: norm =',mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),
'basic =',mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),
'perc =',mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu))

cat('Left miss: norm =',mean(ci.norm[,2]<mu),
'basic =',mean(ci.basic[,2]<mu),'perc=',mean(ci.perc[,2]<mu))

cat('Right miss: norm =',mean(ci.norm[,1]>mu),
'basic =',mean(ci.basic[,1]>mu),'perc=',mean(ci.perc[,1]>mu))
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
library(bootstrap);attach(scor)
head(scor)
dim(scor)

## -----------------------------------------------------------------------------
##original
n<-dim(scor)[1]
Sigma<-(1-1/n)*cov(scor) #Sigma
lambda<-eigen(Sigma)$values #eigenvalues
theta.hat <- lambda[1]/sum(lambda) #PV of first principal component

##jackknife
theta.jack <- numeric(n)
for(i in 1:n){
  Sigma.jack<-cov(scor[-i,])  #Sigma
  lambda.jack<-eigen(Sigma.jack)$values #eigenvalues
  theta.jack[i]<-lambda.jack[1]/sum(lambda.jack) #PV of first principal component
}

##results
bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1)*mean((theta.jack-theta.hat)^2))
round(c(original=theta.hat,bias.jack=bias.jack,se.jack=se.jack),3)
detach(scor)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
m<-n*(n-1)/2
e1 <- e2 <- e3 <- e4 <- matrix(0,nrow = n,ncol = n)

## fit models on leave-two-out samples
for (k in 1:(n-1)) {
  for (l in (k+1):n) {
    y <- magnetic[-c(k,l)]
    x <- chemical[-c(k,l)]
    # Linear model
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[c(k,l)]
    e1[k,l] <- sum((magnetic[c(k,l)] - yhat1)^2)/2
    # Quadratic model
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[c(k,l)] + J2$coef[3] * chemical[c(k,l)]^2
    e2[k,l] <- sum((magnetic[c(k,l)] - yhat2)^2)/2
    # Exponential model
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[c(k,l)]
    yhat3 <- exp(logyhat3)
    e3[k,l] <- sum((magnetic[c(k,l)] - yhat3)^2)/2
    # Log-Log model
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1]+J4$coef[2]*log(chemical[c(k,l)])
    yhat4 <- exp(logyhat4)
    e4[k,l] <- sum((magnetic[c(k,l)] - yhat4)^2)/2
  }
}
##result
c(sum(e1)/m,sum(e2)/m, sum(e3)/m,sum(e4)/m)

## -----------------------------------------------------------------------------
L2<-lm(magnetic ~ chemical + I(chemical^2))
summary(L2)
#par(mfrow = c(2, 2)) #layout for graphs
plot(L2$fit, L2$res) #residuals vs fitted values
abline(0, 0) #reference line
qqnorm(L2$res) #normal probability plot
qqline(L2$res) #reference line
#par(mfrow = c(1, 1)) #restore display
detach(ironslag)

## -----------------------------------------------------------------------------
set.seed(22088) #choose a seed for my homework
## data
x<-c(22,31,20.5,21,35,15.5,12,8)
y<-c(3.69,3.83,3.72,3.18,3.64,3.38,3.24,3)
## permutation
r0=cor(x,y,method = "spearman")
N<-999
reps=numeric(N)
for(i in 1:N){
  x1=sample(x)
  reps[i]=cor(x1,y,method = "spearman")
}
p=mean(abs(c(r0,reps))>=abs(r0))
## result
round(c(p,cor.test(x,y,method = "spearman")$p.value),4)
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
set.seed(12345) #choose a seed for my homework
## some functions
laplace<-function(x){  #standard laplace density
  return(exp(-abs(x))/2)
}
rw.Metropolis <- function(sigma, x0, N) {  #random walk Metropolis sampler
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  # increment simulated from a normal distribution
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (laplace(y)/laplace(x[i-1]))){
      x[i] <- y
      k <- k + 1  #accept
    }
    else {
      x[i] <- x[i-1]
    }
  }
  return(list(x=x, k=k))
}
## Implement
N <- 2000
sigma <- c(.05, .5, 2, 16) #different variances
x0 <- 25 #start
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
print(c(rw1$k, rw2$k, rw3$k, rw4$k)/(N-1)) #rate of candidate points accepted

## -----------------------------------------------------------------------------
#par(mfrow=c(2,2)) #display 4 graphs together
refline <- c(log(0.05),-log(0.05)) #quantiles
rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
for (j in 1:4) {
  plot(rw[,j],type="l", xlab=bquote(sigma == .(round(sigma[j],3))),ylab="X",ylim=range(rw[,j]))
  abline(h=refline)
}
#par(mfrow=c(1,1))

## -----------------------------------------------------------------------------
## Gelman-Rubin method function
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

sigma <- 2 #parameter of proposal distribution
k <- 4 #number of chains to generate
n <- 10000 #length of chains
b <- 1000 #burn-in length
#choose overdispersed initial values
x0 <- c(-10, -5, 5, 10)
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k){
  X[i, ] <- rw.Metropolis(sigma, x0[i], n)$x
}
#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi)){
  psi[i,] <- psi[i,] / (1:ncol(psi))
}
#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psi[,1:j])
}
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
library(carData)
library(car)
set.seed(12345)
## Gibbs sampler function
bivariate.normal.chain<-function(mu,sigma,rho,N,initialize=c(0,0)){
  Z <- matrix(0, N, 2) #the chain, a bivariate sample
  s1 <- sqrt(1-rho^2)*sigma[1]
  s2 <- sqrt(1-rho^2)*sigma[2]
  ###### generate the chain #####
  Z[1, ] <- initialize #initialize
  for (i in 2:N) {
    y <- Z[i-1, 2]
    m1 <- mu[1] + rho * (y - mu[2]) * sigma[1]/sigma[2]
    Z[i, 1] <- rnorm(1, m1, s1)
    x <- Z[i, 1]
    m2 <- mu[2] + rho * (x - mu[1]) * sigma[2]/sigma[1]
    Z[i, 2] <- rnorm(1, m2, s2)
  }
  return(Z)
}
#initialize constants and parameters
N <- 5000 #length of chain
burn <- 1000 #burn-in length
rho <- 0.9 #correlation
mu<-c(0,0) #mean
sigma<-c(1,1) #unit standard deviations
Z<-bivariate.normal.chain(mu,sigma,rho,N)
b <- burn + 1
z <- Z[b:N, ]
plot(z, main="", cex=.5, xlab=bquote(Z[1]),ylab=bquote(Z[2]), ylim=range(z[,2]))
##Linear
Y<-z[,2]
X<-z[,1]
model<-lm(Y~X)
res<-model$residuals
#normality
ks.test(res,"pnorm",mean=mean(res),sd=sqrt(var(res)))
#constant variance
ncvTest(model)

## -----------------------------------------------------------------------------
#par(mfrow=c(2,2))
plot(model)
#par(mfrow=c(1,1))

## -----------------------------------------------------------------------------
set.seed(12345)
k <- 4 #number of chains to generate
n <- 5000 #length of chains
b <- 500 #burn-in length
#choose overdispersed initial values
#generate the chains
X <- matrix(0, nrow=k, ncol=n)
Y <- matrix(0, nrow=k, ncol=n)
#initialize
initialize<-matrix(c(5,5,-5,-5,5,-5,-5,5),nrow = 2)
for (i in 1:k){
  Z<-bivariate.normal.chain(mu,sigma,rho,n,initialize=initialize[,k])
  X[i, ] <- Z[,2]
  Y[i, ] <- Z[,1]
}
## X
#compute diagnostic statistics
psiX <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psiX)){
  psiX[i,] <- psiX[i,] / (1:ncol(psiX))
}
#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psiX[,1:j])
}
plot(rhat[(b+1):n], type="l", xlab="X", ylab="R")
abline(h=1.2, lty=2)
## Y
#compute diagnostic statistics
psiY <- t(apply(Y, 1, cumsum))
for (i in 1:nrow(psiY)){
  psiY[i,] <- psiY[i,] / (1:ncol(psiY))
}
#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psiY[,1:j])
}
plot(rhat[(b+1):n], type="l", xlab="Y", ylab="R")
abline(h=1.2, lty=2)
# Clean the memory
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
## functions for T-statistic
TS0<-function(x,m,y){
  ml1<-lm(m~x)
  ml2<-lm(y~m+x)
  alpha_hat<-summary(ml1)$coefficients[2,1]
  alpha_var<-(summary(ml1)$coefficients[2,2])^2
  beta_hat<-summary(ml2)$coefficients[2,1]
  beta_var<-(summary(ml2)$coefficients[2,2])^2
  ab_var<-(alpha_hat^2)*beta_var+(beta_hat^2)*alpha_var
  ab_se<-sqrt(ab_var)
  return(alpha_hat*beta_hat/ab_se)
}
TS<-function(ml1,ml2){
  alpha_hat<-summary(ml1)$coefficients[2,1]
  alpha_var<-(summary(ml1)$coefficients[2,2])^2
  beta_hat<-summary(ml2)$coefficients[2,1]
  beta_var<-(summary(ml2)$coefficients[2,2])^2
  ab_var<-(alpha_hat^2)*beta_var+(beta_hat^2)*alpha_var
  ab_se<-sqrt(ab_var)
  return(alpha_hat*beta_hat/ab_se)
}

## N:simulation numbers; n:sample size; R:permutation numbers
set.seed(22088) #choose a seed for my homework
N<-500;n<-20;am<-1;ay<-1;gamma<-1;R<-99;res<-numeric(3)

##alpha=0,beta=0
alpha<-0;beta<-0
p<-numeric(N)
for(i in 1:N){  #simulation
  x<-rexp(n)
  m<-am+alpha*x+rnorm(n)
  y<-ay+beta*m+gamma*x+rnorm(n)
  T0<-TS0(x,m,y)
  reps=numeric(R)
  for(j in 1:R){  #permutation
    m1=sample(m)
    ml1<-lm(m1~x)
    ml2<-lm(y~m1+x)
    reps[j]=TS(ml1,ml2)
  }
  p[i]=mean(abs(c(T0,reps))>=abs(T0))
}
res[1]<-mean(p<0.05)  #Type-1 error

##alpha=0,beta=1
alpha<-0;beta<-1
p<-numeric(N)
for(i in 1:N){
  x<-rexp(n)
  m<-am+alpha*x+rnorm(n)
  y<-ay+beta*m+gamma*x+rnorm(n)
  T0<-TS0(x,m,y)
  reps=numeric(R)
  for(j in 1:R){
    m1=sample(m)
    ml1<-lm(m1~x)
    ml2<-lm(y~m+x)
    reps[j]=TS(ml1,ml2)
  }
  p[i]=mean(abs(c(T0,reps))>=abs(T0))
}
res[2]<-mean(p<0.05)

##alpha=1,beta=0
alpha<-1;beta<-0
p<-numeric(N)
for(i in 1:N){
  x<-rexp(n)
  m<-am+alpha*x+rnorm(n)
  y<-ay+beta*m+gamma*x+rnorm(n)
  T0<-TS0(x,m,y)
  reps=numeric(R)
  for(j in 1:R){
    m1=sample(m)
    ml1<-lm(m~x)
    ml2<-lm(y~m1+x)
    reps[j]=TS(ml1,ml2)
  }
  p[i]=mean(abs(c(T0,reps))>=abs(T0))
}
res[3]<-mean(p<0.05)

## -----------------------------------------------------------------------------
##result
res<-matrix(res,ncol = 3)
colnames(res)<-c("a=b=0","a=0,b=1","a=1,b=0")
rownames(res)<-"Type-1 error"
res

## -----------------------------------------------------------------------------
set.seed(22088)
## function
f<-function(N,b1,b2,b3,f0){
  x1<-rpois(N,lambda = 1)
  x2<-rexp(N,rate = 1)
  x3<-sample(0:1,N,replace=TRUE)
  g<-function(alpha){
    tmp<-exp(-alpha-b1*x1-b2*x2-b3*x3)
    p<-1/(1+tmp)
    mean(p)-f0
  }
  solution<-uniroot(g,c(-20,0))
  alpha0<-solution$root
  return(alpha0)
}
## use the function
N<-1e6;b1<-0;b2<-1;b3<--1;f0<-c(0.1,0.01,0.001,0.0001)
alpha<-NULL
for (i in 1:length(f0)) {
  alpha[i]<-f(N,b1,b2,b3,f0[i])
}
## plot
plot(alpha,f0,main = "f0 vs. alpha")
## clean the memory
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
num<-0
set.seed(22088)
for(lambda in seq(0.01,10,0.01)){
  for (n in seq(10,100,1)) {
    u<-rexp(n,lambda)
    v<-u+runif(n,0,3)
    molecules<--(n^2)/(lambda^2)+n*sum((v-u)^2*exp(-lambda*u-lambda*v)/((exp(-lambda*u)-exp(-lambda*v))^2))
    denominator<-(sum((u*exp(-lambda*u)-v*exp(-lambda*v)+(exp(-lambda*u)-exp(-lambda*v))/lambda)/(exp(-lambda*u)-exp(-lambda*v))))^2
    if(abs(molecules/denominator)>=1){num=num+1}
  }
}
num

## -----------------------------------------------------------------------------
u<-c(11,8,27,13,16,0,23,10,24,2)
v<-u+1
mle<-matrix(0,ncol = 2,nrow = 1)

## Direct MLE
mlogL<-function(lambda=1){
  return(-sum(log(exp(-lambda*u)-exp(-lambda*v))))
}
mle[1]<-optimize(mlogL,c(0,5))$minimum

## EM Algorithm
lambda0<-0
lambda<-1
n<-length(u)
times<-0
while (abs(lambda-lambda0)>0.0001) {
  lambda0<-lambda
  ## M-step
  lambda<-n/sum((u*exp(-lambda0*u)-v*exp(-lambda0*v)+(exp(-lambda0*u)-exp(-lambda0*v))/lambda0)/(exp(-lambda0*u)-exp(-lambda0*v)))
  times<-times+1
}
mle[2]<-lambda

## result
colnames(mle)<-c("direct","EM")
rownames(mle)<-"MLE"
mle;times

## -----------------------------------------------------------------------------
v<-c(1,2,3)
dim(v)

## -----------------------------------------------------------------------------
x<-matrix(c(1,2,3,4),ncol = 2)
is.array(x)

## -----------------------------------------------------------------------------
df<-data.frame(x)
attributes(df)

## -----------------------------------------------------------------------------
row0<-df[NULL,]
col0<-df[,FALSE]
dim(row0);dim(col0)

## -----------------------------------------------------------------------------
## clean the memory
rm(list=ls())

## -----------------------------------------------------------------------------
rm(list=ls())
scale01<-function(x){
  rng<-range(x,na.rm = TRUE)
  (x-rng[1])/(rng[2]-rng[1])
}
scale_cars<-data.frame(lapply(cars, scale01))
head(scale_cars)

## -----------------------------------------------------------------------------
#new_scale01<-function(x){
#  if(is.numeric(x)==T){
#    scale01(x)
#  }
#  else{
#    x
#  }
#}
#scale_iris<-data.frame(lapply(iris, new_scale01))
scale_iris<-data.frame(lapply(iris, function(x) if (is.numeric(x)) scale01(x) else x))
head(scale_iris)

## -----------------------------------------------------------------------------
## numeric data frame
vapply(cars,sd,numeric(1))
## mixed data frame
#index<-vapply(iris, is.numeric, logical(1))
#vapply(iris[index],sd,numeric(1))
vapply(iris[vapply(iris, is.numeric, logical(1))],sd,numeric(1))

## ----warning=F----------------------------------------------------------------
library(Rcpp)
library(StatComp22088)
library(microbenchmark)
## Functions by R and C++
#cppFunction('NumericMatrix gibbsC(double mu1,double sigma1,double mu2,double sigma2,double rho,int N) {
#  NumericMatrix Z(N, 2);
#  double s1=0,s2=0;
#  s1 = sqrt(1-rho*rho)*sigma1;
#  s2 = sqrt(1-rho*rho)*sigma2;
#  Z(0,0) = 0;
#  Z(0,1) = 0;   
#  double y = 0, m1 = 0, x=0, m2=0;
#  for(int i = 1; i < N; i++) {
#    y = Z(i-1, 1);
#    m1 = mu1 + rho * (y - mu2) * sigma1/sigma2;
#    Z(i, 0) = rnorm(1, m1, s1)[0];
#    x = Z(i, 0);
#    m2 = mu2 + rho * (x - mu1) * sigma2/sigma1;
#    Z(i, 1) = rnorm(1, m2, s2)[0];
#  }
#  return(Z);
#}')
gibbsR <- function(mu,sigma,rho,N){
  Z <- matrix(0, N, 2) #the chain, a bivariate sample
  s1 <- sqrt(1-rho^2)*sigma[1]
  s2 <- sqrt(1-rho^2)*sigma[2]
  Z[1, ] <- c(0,0)
  for (i in 2:N) {
    y <- Z[i-1, 2]
    m1 <- mu[1] + rho * (y - mu[2]) * sigma[1]/sigma[2]
    Z[i, 1] <- rnorm(1, m1, s1)
    x <- Z[i, 1]
    m2 <- mu[2] + rho * (x - mu[1]) * sigma[2]/sigma[1]
    Z[i, 2] <- rnorm(1, m2, s2)
  }
  return(Z)
}

## -----------------------------------------------------------------------------
set.seed(22088)
N <- 5000 #length of chain
burn <- 1000 #burn-in length
rho <- 0.9 #correlation
mu<-c(0,0) #mean
sigma<-c(1,1) #unit standard deviations
## Gibbs by R
Z<-gibbsR(mu,sigma,rho,N)
b <- burn + 1
dtR <- Z[b:N, ]
## Gibbs by C++
Z<-gibbsC(mu[1],sigma[1],mu[2],sigma[2],rho,N)
b <- burn + 1
dtC <- Z[b:N, ]
## QQ-plot
qqplot(dtR[,1],dtC[,1],xlab = "Gibbs by R",ylab = "Gibbs by C++")
qqplot(dtR[,2],dtC[,2],xlab = "Gibbs by R",ylab = "Gibbs by C++")
## computation time
ts <- microbenchmark(gibbsR=gibbsR(mu,sigma,rho,N),gibbsC=gibbsC(mu[1],sigma[1],mu[2],sigma[2],rho,N))
summary(ts)[,c(1,3,5,6)]
rm(list=ls())

