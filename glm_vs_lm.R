library(MASS)
str(Boston)
head(Boston)

model_lm <- lm(
  medv ~ rm + age + tax + ptratio + nox + indus,
  data = Boston
)
summary(model_lm)

model_glm <- glm(
  medv ~ rm + age + tax + ptratio + nox + indus,
  data = Boston,
  family = gaussian(link = "identity")
)
summary(model_glm)

X <- model.matrix(~ rm + age + tax + ptratio + nox + indus, data = Boston)
y <- Boston$medv
  
loglik <- function(params){
  n <- length(y)
  beta <- params[1:7]
  sigma2 <- params[8]
  resid <- y - X %*% beta
  
  (-n/2) * log(2*pi) - n/2 * log(sigma2) - 1/(2*sigma2) * sum(resid^2)
}

negloglik <- function(params){
  -loglik(params)
}

start <- c(0,0,0,0,0,0,0,1)

result <- optim(start, negloglik, control = list(maxit = 10000))
result_bfgs <- optim(start, negloglik, method = "BFGS", control = list(maxit = 5000))

result_bfgs$par
result_bfgs$convergence

