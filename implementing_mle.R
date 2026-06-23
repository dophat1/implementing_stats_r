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

