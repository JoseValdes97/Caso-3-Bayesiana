sample_bet <- function(X, y, sig2, lambda, p)
{
      return(c(mvtnorm::rmvnorm(n = 1, mean = solve(1/sig2*t(X)%*%X+lambda/sig2*diag(1,nrow=p))%*%(1/sig2*t(X)%*%y), sigma = solve(1/sig2*t(X)%*%X+lambda/sig2*diag(1,nrow=p)))))
}

sample_sig2 <- function(X, y, nu0, sig20, lambda, bet, p,n) 
{
      return(1/rgamma(n = 1, shape = 0.5*(n + p + nu0), rate = 0.5*(nu0*sig20+lambda*t(bet)%*%bet+t(y-X%*%bet)%*%(y-X%*%bet))))
}

sample_lambda <- function(X, al, bl, sig2, bet, p)
{
      return(rgamma(n = 1, shape = 0.5*p+al, rate = 0.5*(1/sig2*t(bet)%*%bet+bl)))
}
# Primer Modelo 
MCMC_1 <- function(X,y,B,verbose = TRUE){
      # Constantes 
      n      <- nrow(X)
      p      <- ncol(X)
      # Previa e inicialización
      nu0    <- 1
      al     <- 1
      bl     <- 2
      bet    <- solve(t(X)%*%X)%*%t(X)%*%y
      sig20  <- sum((y - X%*%bet)^2)/(n-p)
      lambda <- rgamma(n=1,shape=al,rate=bl)
      sig2   <- 1/rgamma(n = 1, shape = 0.5*(nu0), rate = 0.5*(nu0*sig20))
      # Almacenamiento
      LAMBDA <- SIG2 <- BET <- LP <- NULL
       
      # Actualizar
      set.seed(2023)
      for (b in 1:B){
            sig2   <- sample_sig2(X, y, nu0, sig20, lambda, bet, p,n)
            lambda <- sample_lambda(X, al, bl, sig2, bet, p)
            beta   <- sample_bet(X, y, sig2, lambda, p)
            SIG2   <- rbind(SIG2, sig2)
            BET    <- rbind(BET,  bet)
            LAMBDA <- rbind(LAMBDA, lambda)
            LP     <- rbind(LP, sum(dnorm(x = y, mean = bet, sd = sqrt(sig2), log = T)))
      # progreso
      
      }
# retorno
      list(SIG2 = SIG2, BET = BET, LAMBDA = LAMBDA, LP = LP)
}