sample_bet_2 <- function(X, y, sig2, phi2, p){
      c(mvtnorm::rmvnorm(n = 1, mean = solve((1/sig2)*t(X)%*%X+(1/phi2)*diag(1,nrow = p))%*%((1/sig2)*t(X)%*%y), sigma = solve((1/sig2)*t(X)%*%X+(1/phi2)*diag(1,nrow = p))))
}

sample_sig2_2 <- function(X, y, nu0, sig20, bet, p, n) {
      1/rgamma(n = 1, shape = 0.5*(n + nu0), rate = 0.5*(nu0*sig20+t(y-X%*%bet)%*%(y-X%*%bet)))
}

sample_phi2 <- function(p, bet, tau20){
      VGAM::rinv.gaussian(n = p, mu = 1/(abs(bet)*tau20), lambda = 1/tau20^2)
}

MCMC_2 <- function(X,y,B){
      # Constantes 
      n      <- nrow(X)
      p      <- ncol(X)
      # Previa e inicialización
      tau20  <- 5
      nu0    <- 1
      bet    <- solve(t(X)%*%X)%*%t(X)%*%y
      sig20  <- sum(t(y-X%*%bet)%*%(y-X%*%bet))/(n-p)
      set.seed(2023)
      sig2   <- 1/rgamma(n = 1, shape = 0.5*(nu0), rate = 0.5*(nu0*sig20))
      # Almacenamiento
      SIG2 <- BET <- LP <- NULL
      
      # Actualizar
      set.seed(2023)
      for (b in 1:B){
            phi2   <- sample_phi2(p, bet, tau20)
            bet    <- sample_bet_2(X, y, sig2, phi2, p)
            sig2   <- sample_sig2_2(X, y, nu0, sig20, bet, p, n)
            SIG2   <- rbind(SIG2, sig2)
            BET    <- rbind(BET,  bet)
            LP     <- rbind(LP, sum(dnorm(x = y, mean = X%*%bet, sd = sqrt(sig2), log = T)))
            # progreso
      }
      # retorno
      list(SIG2 = SIG2, BET = BET, LP = LP)
}