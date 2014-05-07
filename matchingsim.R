#install.packages(Matching)
library(Matching)

x2<-rnorm(10000)
x3<-rnorm(10000)
x<-0.5*x2 + 0.6*x3
pr = 1/(1+exp(-x))
t = rbinom(10000,1,pr)
y = 2*t + rnorm(10000)

attweight<-ifelse(d==1, 1,(glm1$fitted/(1-glm1$fitted)))

glm1<-glm(t ~ x2 + x3, family=binomial(link="logit"))

Tr=t
Y=y
m1<-Match(Tr=Tr, Y=Y, X=glm1$fitted)

lm1<-lm(y ~ t, weights=attweight)

nreps=500
lmpreds=c()
matchpreds=c()
printnums=seq(from=0, to=nreps, by=10)
for(i in 1:nreps){
  x2<-rnorm(1000)
  x3<-rnorm(1000)
  x<-0.5*x2 + 0.6*x3
  pr = 1/(1+exp(-x))
  t = rbinom(1000,1,pr)
  y = 2*t + rnorm(1000)
  attweight<-ifelse(t==1, 1,(glm1$fitted/(1-glm1$fitted)))
  glm1<-glm(t ~ x2 + x3, family=binomial(link="logit"))
  Tr=t
  Y=y
  m1<-Match(Tr=Tr, Y=Y, X=glm1$fitted)
  lm1<-lm(y ~ t, weights=attweight)
  lmpreds=c(lmpreds, lm1$coef[2])
  matchpreds=c(matchpreds, m1$est)
  if(i %in% printnums) cat(i)
  else cat(".")
}
mean(coefs)


