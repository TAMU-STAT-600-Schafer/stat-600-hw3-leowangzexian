# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  n = nrow(X); p = ncol(X); K = length(unique(y)) # K determined based on the supplied input
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (!all(X[, 1] == 1)) {
    stop("The first column of X should contain all 1s.") # the first column of X is for the intercept
  }
  if (!all(Xt[, 1] == 1)) {
    stop("The first column of Xt should contain all 1s.") # the first column of Xt is for the intercept
  }
  # Check for compatibility of dimensions between X and Y
  if (n != length(y)) {
    stop("The number of rows of X should be the same as the length of y.") # returns error message if the dimensions of X and y do not match
  }
  # Check for compatibility of dimensions between Xt and Yt
  if (nrow(Xt) != length(yt)) {
    stop("The number of rows of Xt should be the same as the length of yt.") # returns error message if the dimensions of Xt and yt do not match
  }
  # Check for compatibility of dimensions between X and Xt
  if (ncol(X) != ncol(Xt)) {
    stop("The number of columns in X should be the same as the number of columns in Xt.") # returns error message if the dimensions of X and Xt do not match
  }
  # Check eta is positive
  if (eta <= 0) {
    stop("Eta should be positive.") # ensures that the learning rate is positive in order to proceed
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop("Lambda should be non-negative") # ensures that the ridge regulariser is non-negative in order to proceed
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if (is.null(beta_init)) {
    beta = matrix(0, p, K)
  } else {
    if (nrow(beta_init) != p | ncol(beta_init) != K) {
      stop("The dimensions of beta_init supplied are not correct.") # returns error message if the dimensions of beta_init are not p times K
    }
    beta = beta_init # initialises beta_init if it passes the compatibility check
  }
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  error_train = numeric(numIter + 1); error_test = numeric(numIter + 1); objective = numeric(numIter + 1) # initialise vectors for storing outputs
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
 
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}

# this function computes P and returns P as a matrix for all possible k
prob = function(X, beta) {
  expm = exp(X %*% beta)
  return(expm / rowSums(expm))
}

# this function returns the value of the objective function
obj = function(X, y, lambda, beta) {
  P = prob(X, beta)
  return( - sum(log(P[cbind(1:n, y + 1)])) + 0.5 * lambda * sum(beta^2) ) # sums the log probabilities of the class for each sample plus the ridge penalty term
}
