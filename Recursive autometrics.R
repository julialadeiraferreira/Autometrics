
 
## Autometrics (automatic general-to-specific selection)
## is a variable selection approach for linear regression models



library(zoo)
library(stargazer)
library(xts)
library(gets)
library(aTSA)
library(forecast)
library(stats)
library(tseries)




#define y
y <- ts(data=teste_t$tri, frequency = 4, start=c(2003,2), end=c(2019,3))
d1 <- ts(data=teste_t$d1, frequency = 4, start=c(2003,2), end=c(2019,3))
d2 <- ts(data=teste_t$d2, frequency = 4, start=c(2003,2), end=c(2019,3))
d3 <- ts(data=teste_t$d3, frequency = 4, start=c(2003,2), end=c(2019,3))
# adf test: ho = has a unit root


## define x
x_tri <- ts(data=teste_t[,3:29], frequency = 4, start=c(2003,2), end=c(2019,3))

# This is a time series application, so adf test is requires
for (i in 1:39){
  print(dimnames(x)[[2]][i]) #nome da variavel no adf
  adf.test(as.vector(x[,i]))
}

adf.test(as.vector(y))
acf(y)
pacf(y)
nsdiffs(y) # numero de vezes que diferencia para sazonal



### Recursive autometrics
### For each step, the variables are reselected. The first selection started with
### sample size = 44

for (t in 1:22) {
  y1 <- y[1:44+t] 
  x_tri1 <- x_tri[1:44+t,1:20]  
  modelauto <- getsFun(y1, x_tri1, untransformed.residuals=NULL,
                       user.estimator=list(name="ols"), gum.result=NULL, t.pval=0.05,
                       do.pet=TRUE, ar.LjungB=NULL, arch.LjungB=NULL,
                       normality.JarqueB=NULL, user.diagnostics=NULL,
                       gof.function=list(name="infocrit", method="sc"),
                       gof.method=c("min", "max"), keep=NULL, include.gum=FALSE,
                       include.1cut=FALSE, include.empty=FALSE, max.paths=NULL, turbo=FALSE,
                       tol=1e-07, LAPACK= FALSE, max.regs=NULL, print.searchinfo=TRUE,
                       alarm=FALSE)
  var0 <- modelauto$specific.spec
  var <- list(var,var0)
  print(var)
}



## Since info goes till var[[119]].... Change n to your sample
l <- teste_m[,var[[2]]+1]
count <- 1
for (a in 3:119){
  if (var[[a]]>var[[a-1]]){
    l <- cbind(l,teste_m[,var[[a]]+1])
  } else{
    m <- l
    l <- teste_m[,var[[a]]+1]
    count <- count +1
  }
}
# l <- teste_m[,var[[2]]+1]
#l <- cbind(teste_m[,var[[a+1]]])




## Recursive Gets (mean and arch)
var_mod <- list()
for (t in 1:22) {
  y2 <- y[1:44+t] 
  x_tri2 <- x_tri[1:44+t,1:20] 
  mod0 <- arx(y2, mc = TRUE, ar = 0, mxreg = x_tri2, arch = 1:3)
  getsmod0 <- getsm(mod0)
  var0_mod <- getsmod0$specific.spec
  var_mod <- append(var_mod,var0_mod)
  print(var_mod)
}
