source("regression_gprior (1).R")
MCMC_4 <- function(X,y,S){
      p <- ncol(X)
      BETA<-Z<-matrix(NA,S,p)
      SIG2 <- LP <- NULL 
      z<-rep(1,dim(X)[2] )
      lpy.c<-lpy.X(y,X[,z==1,drop=FALSE])
      for(s in 1:S){
            for(j in sample(1:p)){
                  zp<-z ; zp[j]<-1-zp[j]
                  lpy.p<-lpy.X(y,X[,zp==1,drop=FALSE])
                  r<- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
                  z[j]<-rbinom(1,1,1/(1+exp(-r)))
                  if(z[j]==zp[j]) {lpy.c<-lpy.p}
            }
            beta<-z
            if(sum(z)>0)
                  {
                  beta[z==1]<-lm.gprior(y,X[,z==1,drop=FALSE],S=1)$beta
                  s2<-lm.gprior(y,X[,z==1,drop=FALSE],S=1)$s2
                  }
            BETA[s,]<-beta
            SIG2[s] <-s2
            LP     <- rbind(LP, sum(dnorm(x = y, mean = beta, sd = sqrt(s2), log = T)))
      }
      return(list(BETA=BETA,SIG2=SIG2,LP=LP))
}