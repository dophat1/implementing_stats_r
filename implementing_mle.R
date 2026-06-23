set.seed(1)
n <- 50
x1 <- rnorm(n, mean = 5, sd = 2)
y <- 3 + 2*x1 + rnorm(n, mean = 0, sd = 1.5)
X <- cbind(1, x1)
dim(X)
