
log_lik_poisson <- function(X, y, beta){
  eta <- X %*% beta
  muy <- exp(eta)
  ll_poisson <- sum(y * eta - muy - lfactorial(y))
  return(ll_poisson)
}

score_poisson <- function(X,y,beta){
  eta <- X %*% beta
  muy <- exp(eta)
  score <- t(X) %*% (y - muy)
  return(score)
}