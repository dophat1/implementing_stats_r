
log_lik_poisson <- function(X, y, beta){
  eta <- X %*% beta
  muy <- exp(eta)
  ll_poisson <- sum(y * eta - muy - lfactorial(y))
  return(ll_poisson)
}