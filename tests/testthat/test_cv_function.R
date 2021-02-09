# --------------------------------------------------
# Test Script - Output from cv.SplitGLM Function
# --------------------------------------------------

# Required libraries
library(SplitGLM)
library(mvnfast)

# Context of test script
context("Verify output of cross-validation function.")

# There should be an error if we want to compute the IF TS, and no returns are provided
test_that("Error in the cross-validation function.", {

  # Sigmoid function
  sigmoid <- function(t){
    return(exp(t)/(1+exp(t)))
  }
  
  # Function to return the offset
  offset.compute <- function(M, beta, Sigma, proportion){
    
    # Number of coefficients
    p <- length(beta)
    
    # Computing proportion for given offset
    x.data <- rmvn(M, mu = rep(0, p), sigma = Sigma)
    coef.fit <- x.data %*% beta
    
    proportion.difference <- function(offset, coef.fit, proportion){
      prob.test <- sigmoid(coef.fit + offset)
      computed.proportion <- mean(round(prob.test, 0))
      return(abs(computed.proportion-proportion))
    }
    
    # Offset computation
    optimal.offset <- optimize(f=proportion.difference, interval=c(-20,20),
                               coef.fit=coef.fit, proportion=proportion)$minimum
    
    return(optimal.offset)
  }
  
  # Data simulation
  set.seed(1)
  n <- 50
  N <- 2000
  p <- 100
  beta.active <- c(abs(runif(p, 0, 1/2))*(-1)^rbinom(p, 1, 0.3))
  # Parameters
  p.active <- 40
  beta <- c(beta.active[1:p.active], rep(0, p-p.active))
  Sigma <- matrix(0, p, p)
  Sigma[1:p.active, 1:p.active] <- 0.5
  diag(Sigma) <- 1
  
  # Offset computation
  M <- 1000
  proportion.1 <- 0.3
  offset <- offset.compute(M, beta, Sigma, proportion.1)
  
  # Train data
  x.train <- rmvn(n, mu = rep(0, p), sigma = Sigma) 
  prob.train <- sigmoid(x.train %*% beta + offset)
  y.train <- rbinom(n, 1, prob.train)
  mean(y.train)
  # Test data
  x.test <- rmvn(N, mu = rep(0, p), sigma = Sigma)
  prob.test <- sigmoid(x.test %*% beta + offset)
  y.test <- rbinom(N, 1, prob.test)
  mean(y.test)
  
  # SplitGLM - CV (Multiple Groups)
  split.out <- cv.SplitGLM(x.train, y.train,
                           glm_type="Logistic",
                           G=10, include_intercept=TRUE,
                           alpha_s=3/4, alpha_d=1,
                           n_lambda_sparsity=50, n_lambda_diversity=50,
                           tolerance=1e-3, max_iter=1e3,
                           n_folds=5,
                           active_set=FALSE,
                           n_threads=5)
  split.coef <- coef(split.out)
  
  expect_vector(split.coef)

})




