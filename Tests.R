# This is a script to save your own tests for the function
source("FunctionsLR.R")

# first test case
Y = c(0, 1, 2, 3, 4, 3, 2, 1, 0, 2, 3, 4, 1, 2, 0, 4)
X = matrix(rnorm(15*20), 15)
Yt = c(1, 0, 3, 2)
Xt = matrix(rnorm(3*20), 3)
X = cbind(1, X)
Xt = cbind(1, Xt)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1)
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 500, eta = 0.1, lambda = 1) # more iterations
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 1) # larger eta
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.01, lambda = 1) # smaller eta
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 5) # larger lambda
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 0.5) # smaller lambda
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')

# compatibility checks to see if error messages will be returned appropriately
out = LRMultiClass(matrix(rnorm(16*20), 16), Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1)
out = LRMultiClass(X, Y, matrix(rnorm(4*20), 4), Yt, numIter = 50, eta = 0.1, lambda = 1)
out = LRMultiClass(X, c(0, 1, 2, 3), Xt, Yt, numIter = 50, eta = 0.1, lambda = 1)
out = LRMultiClass(X, Y, Xt, c(1, 2), numIter = 50, eta = 0.1, lambda = 1)
out = LRMultiClass(X, Y, matrix(rnorm(4*10), 4), Yt, numIter = 50, eta = 0.1, lambda = 1)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = -0.1, lambda = 1)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = -1)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = matrix(0, 20, 2))
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = matrix(0, 2, 5))

# 2nd test case
Y = rbinom(1000, size = 10, prob = 0.5) 
X = matrix(rnorm(999*500), 999)
Yt = rbinom(200, size = 10, prob = 0.5) 
Xt = matrix(rnorm(199*500), 199)
X = cbind(1, X)
Xt = cbind(1, Xt)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1)
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 500, eta = 0.1, lambda = 1) # more iterations
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 1) # larger eta
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.01, lambda = 1) # smaller eta
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 5) # larger lambda
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.5, lambda = 0.5) # smaller lambda
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')

# tests relating to the letter example
letter_train <- read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])
letter_test <- read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])
X = cbind(1, X)
Xt = cbind(1, Xt)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = matrix(10, p, K))
plot(out$objective, type = 'o', xlab = "number of iterations", ylab = "objective values")
plot(out$error_train, type = 'o', xlab = "number of iterations", ylab = "training error")
plot(out$error_test, type = 'o')
