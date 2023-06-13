sample_bet_1 <- function(X, y, sig2, lambda, p){
      c(mvtnorm::rmvnorm(n = 1, mean = solve((1/sig2)*t(X)%*%X+(lambda/sig2)*diag(1,nrow=p))%*%(t(X)%*%y)*(1/sig2), sigma = solve((1/sig2)*t(X)%*%X+(lambda/sig2)*diag(1,nrow=p))))
}

sample_sig2_1 <- function(X, y, nu0, sig20, lambda, bet, p, n) {
      1/rgamma(n = 1, shape = 0.5*(n + p + nu0), rate = 0.5*(nu0*sig20+lambda*t(bet)%*%bet+t(y-X%*%bet)%*%(y-X%*%bet)))
}

sample_lambda_1 <- function(X, al, bl, sig2, bet, p){
      rgamma(n = 1, shape = (0.5*p)+al, rate = ((1/(2*sig2))*t(bet)%*%bet)+bl)
}
# Primer Modelo 
MCMC_1 <- function(X,y,B){
      # Constantes 
      n      <- nrow(X)
      p      <- ncol(X)
      # Previa e inicialización
      nu0    <- 1
      al     <- 1
      bl     <- 2
      bet    <- solve(t(X)%*%X)%*%t(X)%*%y
      sig20  <- sum(t(y-X%*%bet)%*%(y-X%*%bet))/(n-p)
      set.seed(2023)
      lambda <- rgamma(n = 1, shape = al, rate = bl)
      set.seed(2023)
      sig2   <- 1/rgamma(n = 1, shape = 0.5*(nu0), rate = 0.5*(nu0*sig20))
      # Almacenamiento
      LAMBDA <- SIG2 <- BET <- LP <- NULL
       
      # Actualizar
      set.seed(2023)
      for (b in 1:B){
            bet    <- sample_bet_1(X, y, sig2, lambda, p)
            sig2   <- sample_sig2_1(X, y, nu0, sig20, lambda, bet, p, n)
            lambda <- sample_lambda_1(X, al, bl, sig2, bet, p)
            LAMBDA <- rbind(LAMBDA, lambda)
            SIG2   <- rbind(SIG2, sig2)
            BET    <- rbind(BET,  bet)
            LP     <- rbind(LP, sum(dnorm(x = y, mean = X%*%bet, sd = sqrt(sig2), log = T)))
      # progreso
      
      }
# retorno
      list(SIG2 = SIG2, BET = BET, LAMBDA = LAMBDA, LP = LP)
}