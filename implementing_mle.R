set.seed(1)
n <- 50
x1 <- rnorm(n, mean = 5, sd = 2)
y <- 3 + 2*x1 + rnorm(n, mean = 0, sd = 1.5)
X <- cbind(1, x1)
dim(X)

# Likelihood function 
loglik <- function(beta, sigma2, y, X) {
  n <- length(y)
  resid = y - X %*% beta
  (-n/2) * log(2*pi) - n/2 * log(sigma2) - 1/(2*sigma2) * sum(resid^2)
}

#So: output of loglik is one number, representing the (log) plausibility of the observed data under that specific (β,σ2)(\beta,\sigma^2)
#(β,σ2) — bigger (less negative) is more plausible, smaller (more negative) is less plausible.
fit <- lm(y ~ x1)
beta_hat <- coef(fit)
sigma2_hat <- mean(resid(fit)^2)   # MLE estimate of sigma^2 (note: NOT the same divisor as summary()'s estimate)

loglik(beta_hat, sigma2_hat, y, X)

loglik(c(0, 0), sigma2_hat, y, X)

# score function
score <- function(beta, sigma2, y, X) {
  n <- length(y)
  resid <- y - X %*% beta
  score_beta <- 1/sigma2 * t(X) %*% resid          # length-2 vector, using X, resid, sigma2
  score_sigma2 <- -n/(2*sigma2) + 1/(2*sigma2**2) * sum(resid^2)       # single number, using n, sigma2, resid
  c(score_beta, score_sigma2)   # combine into one vector
}

score(beta_hat, sigma2_hat, y, X)

# hessian matrix

hessian <- function(beta, sigma2, y, X) {
  n <- length(y)
  resid <- y - X %*% beta
  
  H_bb <- 1/sigma2 * t(X) %*% X              # 2x2
  H_bs <- 1/sigma2**2 * t(X) %*% resid             # 2x1
  H_ss <- -n/(2*sigma2**2) + 1/(sigma2^3) *sum(resid^2)          # 1x1 (scalar)
  
  top <- cbind(H_bb, H_bs)         # glue 2x2 and 2x1 side by side -> 2x3
  bottom <- cbind(t(H_bs), H_ss)   # glue 1x2 and 1x1 side by side -> 1x3
  rbind(top, bottom)               # stack -> 3x3
}

H <- hessian(beta_hat, sigma2_hat, y, X)
H
eigen(H)$values
