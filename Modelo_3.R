########################################################33
################### Modelo 3
##########################################################
# funciones auxiliares
tr <- function(X) sum(diag(X))
rmvnorm <- function(n,mu,Sigma) 
{
      p<-length(mu)
      res<-matrix(0,nrow=n,ncol=p)
      if( n>0 & p>0 ) {
            E<-matrix(rnorm(n*p),n,p)
            res<-t(  t(E%*%chol(Sigma)) +c(mu))
      }
      res
}
##################################################
sample_bet_3 <- function(X, y, p, iCor, sig2, tau20){
      V.beta <- solve( t(X)%*%iCor%*%X/sig2 + ((1/tau20)*diag(1,nrow = p)))
      E.beta <- V.beta%*%( t(X)%*%iCor%*%y/sig2)
      t(rmvnorm(1,E.beta,V.beta))
}
##
sample_sig2_3 <- function(X, y, iCor, nu0, s20, bet, n) {
      1/rgamma(n = 1, shape = 0.5*(n + nu0), rate = 0.5*(nu0*s20+t(y-X%*%bet)%*%iCor%*%(y-X%*%bet)))
}
################################3
##### Tercer modelo
###############################
MCMC_3 <- function(X,y,B){
      # Constantes 
      n      <- nrow(X)
      p      <- ncol(X)
      DY <-abs(outer( (1:n),(1:n) ,"-")) # para construir la matriz de correlacion
      # Previa
      tau20  <- 50
      nu0    <- 1
      ap     <- 0
      bp     <- 1
      # inicialización
      bet    <- solve(t(X)%*%X)%*%t(X)%*%y #OLS
      sig20  <- sum(t(y-X%*%bet)%*%(y-X%*%bet))/(n-p) #OLS
      set.seed(2023)
      sig2   <- 1/rgamma(n = 1, shape = 0.5*(nu0), rate = 0.5*(nu0*sig20))
      set.seed(2023)
      rho <- runif(1,ap,bp)
      # Almacenamiento
      SIG2 <- BET <- LP <- RHO <- NULL
      odens <- B/10000 #información
      #tasa de aceptación
      ac    <-0 
      ncat <- floor(B/10)
      # Actualizar
      set.seed(2023)
      for (b in 1:B){
            # beta
            Cor    <- rho^DY
            iCor   <- solve(Cor)
            bet    <- sample_bet_3(X, y, p, iCor, sig2, tau20)
            #sig2
            sig2   <- sample_sig2_3(X, y, iCor, nu0, sig20, bet, n)
            # simular rho (metropolis)
            # 1. propuesta
            rho.p <- abs(runif(1,rho-.1,rho+.1))
            rho.p <- min(rho.p, 2-rho.p)
            # 2. tasa de aceptacion
            lr <- -.5*( determinant(rho.p^DY,log=TRUE)$mod - determinant(rho^DY,log=TRUE)$mod + 
                              tr( (y-X%*%bet)%*%t(y-X%*%bet)%*%(solve(rho.p^DY) - solve(rho^DY)) )/sig2 )
            # 3. actualizar valor
            if( log(runif(1)) < lr ) { 
                  rho <-rho.p
                  ac<-ac+1 
            }
            # almacenar y log-veorsimilitud
            if(b%%odens==0) {
                  BET    <- rbind(BET,  t(bet))
                  SIG2   <- rbind(SIG2, sig2)
                  RHO    <- rbind(RHO,  rho)
                  LP <- rbind(LP, mvtnorm:: dmvnorm(x = y,mean = X%*%bet, sigma = sig2*Cor, log= TRUE))
            }
             #Progreso
            if (b%%ncat == 0) cat(100*round(b/B, 1), "% completado ... \n", sep = "" )
          
      }
      # retorno
      list(SIG2 = SIG2, BET = BET, LP = LP, RHO = RHO)
}