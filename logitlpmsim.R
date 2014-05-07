
nreps=1000
glms<-vector('list', length(nreps))
lms<-vector('list', length(nreps))
tots<-vector('list', length(nreps))
cs<-sample(-2:2,1000,replace=T)
printnums=seq(from=0, to=nreps, by=10)
for(i in 1:nreps){
  x<-rnorm(10000)
  x1<-rbinom(10000,1,runif(1,0,1))
  x2<-rbinom(10000,1,runif(1,0,1))
  z<-sample(cs,1)*x + sample(cs,1)*x1+ sample(cs,1)*x2 +rnorm(10000)
  pr = 1/(1+exp(-z))
  y = rbinom(10000,1,pr)
  glms[[i]]<-glm(y~x+x1+x2,family=binomial(link='logit'))
  lms[[i]]<-lm(y~x+x1+x2)
  tots[[i]]<-sum(y==1)
  if(i %in% printnums) cat(i)
  else cat(".")
}



nreps=1000
glms2<-vector('list', length(nreps))
lms2<-vector('list', length(nreps))
tots2<-vector('list', length(nreps))
cs<-sample(-2:2,1000,replace=T)
decider<-runif(1000,0,1)
printnums=seq(from=0, to=nreps, by=10)
for(i in 1:nreps){
  x<-rnorm(10000)
  x1<-rbinom(10000,1,sample(0.9))
  x2<-rbinom(10000,1,sample(0.6))
  z<-sample(cs,1)*x + sample(cs,1)*x1+ sample(cs,1)*x2 +rnorm(10000)
  y = ifelse((z>sample(decider,1)),1,0)
  glms2[[i]]<-glm(y~x+x1,family=binomial(link='logit'))
  lms2[[i]]<-lm(y~x+x1)
  tots2[[i]]<-sum(y==1)
  if(i %in% printnums) cat(i)
  else cat(".")
}

z2<-runif(1000,-5,5)
pr2 = 1/(1+exp(-z2))


thecomp<-cbind(unlist(lapply(glms,function(x) BIC(x))),unlist(lapply(lms,function(x) BIC(x))),unlist(lapply(tots, function(x) x/10000)))
thecomp<-as.matrix(thecomp)
thecomp<-cbind(thecomp, (thecomp[,2]-thecomp[,1]))
thecomp<-as.data.frame(thecomp)

p<-ggplot(thecomp)
p<-p+geom_point(aes(x=V3,y=V4),color="#CCCCCC")+geom_smooth(aes(x=V3,y=V4),color="#2671C4")+theme_bw()+xlab("Proportion Sample=1")+ylab("Delta PLM-Logit BIC")


thecomp2<-cbind(unlist(lapply(glms2,function(x) BIC(x))),unlist(lapply(lms2,function(x) BIC(x))),unlist(lapply(tots2, function(x) x/10000)))
thecomp2<-as.matrix(thecomp2)
thecomp2<-cbind(thecomp2, (thecomp2[,2]-thecomp2[,1]))
thecomp2<-as.data.frame(thecomp2)

p.2<-ggplot(thecomp2)
p.2<-p.2+geom_point(aes(x=V3,y=V4),color="#CCCCCC")+geom_smooth(aes(x=V3,y=V4),color="#2671C4")+theme_bw()+xlab("Proportion Sample=1")+ylab("Delta PLM-Logit BIC")

p2<-ggplot()
p2<-p2+geom_smooth(aes(x=z2,y=pr2),color="#2671C4")+theme_bw()+xlab("x")+ylab("P(x)")+ggtitle("The Logit Curve")


z2<-runif(1000,-5,5)
pr2 = 1/(1+exp(-z2))
