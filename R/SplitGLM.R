#' 
#' @useDynLib SplitGLM
#' @importFrom Rcpp sourceCpp
#' 
#' 
#' @importFrom stats coef predict
#' 
#' @title Split Generalized Linear Model
#' 
#' @description \code{SplitGLM} performs computes the coefficients for split generalized linear models.
#' 
#' @param x Design matrix.
#' @param y Response vector.
#' @param glm_type Description of the error distribution and link function to be used for the model. Must be one of "Linear", "Logistic",
#' "Gamma" or "Poisson".
#' @param G Number of groups into which the variables are split. Can have more than one value.
#' @param include_intercept Boolean variable to determine if there is intercept (default is TRUE) or not.
#' @param alpha_s Elastic net mixing parmeter. Default is 3/4.
#' @param alpha_d Mixing parameter for diversity penalty. Default is 1.
#' @param lambda_sparsity Number of candidates for the sparsity penalty parameter. Default is 100.
#' @param lambda_diversity Number of candidates for the sparsity penalty parameter. Default is 100.
#' @param tolerance Convergence criteria for the coefficients. Default is 1e-3.
#' @param max_iter Maximum number of iterations in the algorithm. Default is 1e5.
#' @param active_set Active set convergence for the algorithm. Default is FALSE.
#' 
#' @return An object of class SplitGLM.
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{coef.SplitGLM}}, \code{\link{predict.SplitGLM}}
#' 
#' @examples 
#' \donttest{
#' # Data simulation
#' set.seed(1)
#' n <- 50
#' N <- 2000
#' p <- 1000
#' beta.active <- c(abs(runif(p, 0, 1/2))*(-1)^rbinom(p, 1, 0.3))
#' # Parameters
#' p.active <- 100
#' beta <- c(beta.active[1:p.active], rep(0, p-p.active))
#' Sigma <- matrix(0, p, p)
#' Sigma[1:p.active, 1:p.active] <- 0.5
#' diag(Sigma) <- 1
#' 
#' # Train data
#' x.train <- mvnfast::rmvn(n, mu = rep(0, p), sigma = Sigma) 
#' prob.train <- exp(x.train %*% beta)/
#'               (1+exp(x.train %*% beta))
#' y.train <- rbinom(n, 1, prob.train)
#' mean(y.train)
#' # Test data
#' x.test <- mvnfast::rmvn(N, mu = rep(0, p), sigma = Sigma)
#' prob.test <- exp(x.test %*% beta)/
#'              (1+exp(x.test %*% beta))
#' y.test <- rbinom(N, 1, prob.test)
#' mean(y.test)
#' 
#' # SplitGLM - Multiple Groups
#' split.out <- SplitGLM(x.train, y.train,
#'                       glm_type="Logistic",
#'                       G=10, include_intercept=TRUE,
#'                       alpha_s=3/4, alpha_d=1,
#'                       lambda_sparsity=1, lambda_diversity=1,
#'                       tolerance=1e-3, max_iter=1e3,
#'                       active_set=FALSE)
#' split.coef <- coef(split.out)
#' # Predictions
#' split.prob <- predict(split.out, newx=x.test, type="prob", group_index=NULL)
#' split.class <- predict(split.out, newx=x.test, type="class", group_index=NULL)
#' plot(prob.test, split.prob, pch=20)
#' abline(h=0.5,v=0.5)
#' mean((prob.test-split.prob)^2)
#' mean(abs(y.test-split.class))
#' 
#' }
#' 

SplitGLM <- function(x, y, 
                     glm_type = "Linear", 
                     G = 10, include_intercept=TRUE, 
                     alpha_s = 3/4, alpha_d = 1,
                     lambda_sparsity, lambda_diversity,
                     tolerance = 1e-3, max_iter = 1e5,
                     active_set = FALSE){
  
  # Check response data
  y <- Check_Response(y, glm_type)

  # Check data
  Check_Data(x, y,
             glm_type,
             G,
             alpha_s, alpha_d,
             lambda_sparsity, lambda_diversity,
             tolerance, max_iter)
  
  # Shuffling the data
  n <- nrow(x)
  random.permutation <- sample(1:n, n)
  x.permutation <- x[random.permutation, ]
  y.permutation <- y[random.permutation]
  
  # Setting the model type
  type.cpp <- switch(glm_type,
                     "Linear" = 1,
                     "Logistic" = 2,
                     "Gamma" = 3,
                     "Poisson" = 4)
  
  # Setting to include intercept parameter for CPP computation
  include_intercept.cpp <- sum(include_intercept)
  
  # Setting for active set convergence for CPP computation
  active_set.cpp <- sum(active_set)
  
  # Source code computation
  split.out <- Split_WEN_Main(x.permutation, y.permutation, 
                              type.cpp, 
                              G, include_intercept.cpp, 
                              alpha_s, alpha_d,
                              lambda_sparsity, lambda_diversity,
                              tolerance, max_iter,
                              active_set.cpp)
  
  # Object construction
  split.out <- construct.SplitGLM(split.out, match.call(), G, glm_type)
  
  # Return source code output
  return(split.out)
}


