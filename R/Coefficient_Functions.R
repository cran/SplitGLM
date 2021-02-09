#' 
#' @title Coefficients for SplitGLM Object
#' 
#' @description \code{coef.SplitGLM} returns the coefficients for a SplitGLM object.
#' 
#' @method coef SplitGLM
#'
#' @param object An object of class SplitGLM.
#' @param group_index The group for which to return the coefficients. Default is the ensemble.
#' @param ... Additional arguments for compatibility.
#' 
#' @return The coefficients for the SplitGLM object.
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{SplitGLM}}
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
#' # SplitGLM - CV (Multiple Groups)
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
coef.SplitGLM <- function(object, group_index = NULL, ...){
  
  # Check input data
  if(!any(class(object) %in% "SplitGLM"))
    stop("The object should be of class \"SplitGLM\"")
  
  if(object$G==1)
    return(object$coef)
  
  if(is.null(group_index))
    return(object$coef) else{
      if(!(group_index %in% 1:object$G))
        stop("The group index is not valid.")
      
      return(c(object$Intercept[group_index],object$Betas[,group_index]))
    }
  
}

#' 
#' @title Coefficients for cv.SplitGLM Object
#' 
#' @method coef cv.SplitGLM
#' 
#' @description \code{coef.cv.SplitGLM} returns the coefficients for a cv.SplitGLM object.
#'
#' @param object An object of class cv.SplitGLM.
#' @param group_index The group for which to return the coefficients. Default is the ensemble coefficients.
#' @param ... Additional arguments for compatibility.
#' 
#' @return The coefficients for the cv.SplitGLM object.
#' 
#' @export
#' 
#' @author Anthony-Alexander Christidis, \email{anthony.christidis@stat.ubc.ca}
#' 
#' @seealso \code{\link{cv.SplitGLM}}
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
#' # SplitGLM - CV (Multiple Groups)
#' split.out <- cv.SplitGLM(x.train, y.train,
#'                          glm_type="Logistic",
#'                          G=10, include_intercept=TRUE,
#'                          alpha_s=3/4, alpha_d=1,
#'                          n_lambda_sparsity=50, n_lambda_diversity=50,
#'                          tolerance=1e-3, max_iter=1e3,
#'                          n_folds=5,
#'                          active_set=FALSE,
#'                          n_threads=1)
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
coef.cv.SplitGLM <- function(object, group_index = NULL, ...){
  
  # Check input data
  if(!any(class(object) %in% "cv.SplitGLM"))
    stop("The object should be of class \"cv.SplitGLM\"")
  
  if(object$G==1)
    return(object$coef)
  
  if(is.null(group_index))
    return(object$coef) else{
      
      if(!(group_index %in% 1:object$G))
        stop("The group index is not valid.")
      
      return(c(object$Intercept[group_index,object$Optimal_Index],object$Betas[,group_index,object$Optimal_Index]))
    }
  
}