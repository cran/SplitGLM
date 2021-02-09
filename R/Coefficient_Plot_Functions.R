#' 
#' @title Plot of coefficients paths for cv.SplitGLM Object
#' 
#' @method plot cv.SplitGLM
#' 
#' @description \code{plot.cv.SplitGLM} returns the coefficients for a cv.SplitGLM object.
#'
#' @param x An object of class cv.SplitGLM.
#' @param group_index The group for which to return the coefficients. Default is the ensemble coefficients.
#' @param plot_type Plot of coefficients, "Coef" (default), or cross-validated error or deviance, "CV-Error".
#' @param active_only Only include the variables selected in final model (default is TRUE).
#' @param path_type Plot of coefficients paths as a function of either "Log-Lambda" (default) or "L1-Norm".
#' @param labels Include the labels of the variables (default is FALSE).
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
#'                          
#' # Plot of coefficients paths (function of Log-Lambda)
#' plot(split.out, plot_type="Coef", path_type="Log-Lambda", group_index=1, labels=FALSE)
#' 
#' # Plot of coefficients paths (function of L1-Norm)
#' plot(split.out, plot_type="Coef", path_type="L1-Norm", group_index=1, labels=FALSE)
#' 
#' # Plot of CV error
#' plot(split.out, plot_type="CV-Error")
#' 
#' }
#' 
plot.cv.SplitGLM <- function(x, group_index = NULL, 
                             plot_type = c("Coef", "CV-Error")[1],
                             active_only = TRUE, path_type = c("Log-Lambda", "L1-Norm")[1], labels = TRUE,
                             ...){
  
  # Check input data
  if(!any(class(x) %in% "cv.SplitGLM"))
    stop("The x should be of class \"cv.SplitGLM\"")
  
  # Checking input for group index
  if(!is.null(group_index))
    if(!(group_index %in% 1:x$G))
      stop("The group index is not valid.")
  
  # Check input data for plot type
  if(!any(plot_type %in% c("Coef", "CV-Error")))
    stop("The plot type must be one of \"Coef\" or \"CV-Error\"")
  
  # Check input data for plot type
  if(!any(path_type %in% c("Log-Lambda", "L1-Norm")))
    stop("The plot type must be one of \"Log-Lambda\" or \"L1-Norm\"")
  
  # Plot of coefficients paths
  if(plot_type=="Coef"){
  
    # Function to extract active coefficients
    active.coef <- function(x, group_index){
      
      if(x$G==1)
        return(which(x$Betas[,x$Optimal_Index]!=0)) else{
          
          if(!is.null(group_index))
            return(which(x$Betas[, group_index, x$Optimal_Index]!=0)) else
              return(which(coef(x)[-1]!=0))
        }
    }
    
    # Extracting the coefficients matrix
    coef.matrix <- matrix(ncol=0, nrow=length(x$Lambda_Sparsity))
    if(x$G==1){
      
      for(j in 1:length(coef(x)[-1]))
        coef.matrix <- cbind(coef.matrix, x$Betas[j, ])
      
    } else{
      
      if(!is.null(group_index)){
        
        for(j in 1:length(coef(x)[-1]))
          coef.matrix <- cbind(coef.matrix, x$Betas[j,group_index,])
        
      } else{
        
        for(j in 1:length(coef(x)[-1]))
          coef.matrix <- cbind(coef.matrix, apply(x$Betas[j,,], 2, mean))
      }
    }
      
    # Active coefficients
    if(active_only)
      coef.matrix <- coef.matrix[, active.coef(x, group_index)]
    
    # Plot type
    if(path_type=="Log-Lambda"){
      
      x.axis.val <- log(x$Lambda_Sparsity)
      plot.x.label <- (expression(log*phantom(x)*lambda[s]))
      xpos <- min(x.axis.val)
      pos <- 2
      
    } else if(path_type=="L1-Norm"){
        
        x.axis.val <- apply(coef.matrix, 1, function(x) return(sum(abs(x))))
        plot.x.label <- "L1 Norm"
        xpos <- max(x.axis.val)
        pos <- 4
        
      }
    
    # Coefficients paths plot
    matplot(x.axis.val, coef.matrix, lty=2, type="l",
            xlab=plot.x.label, ylab="Coefficients", lwd=2,
            panel.first=grid(), cex.lab=1.25)
    abline(v=x.axis.val[x$Optimal_Index], lty=3)
    abline(h=0, lty=3)
    
    # Adding the labels 
    if(labels){
      ypos <- coef.matrix[1,]
      if(active_only)
        text(xpos, ypos, cex=.5, pos=pos, labels=active.coef(x, group_index)) else
          text(xpos, ypos, cex=.5, pos=pos, labels=1:length(coef(x)[-1]))
    }
    
  } else if(plot_type=="CV-Error"){ # Plot of CV Error
    
    # Function for error bars
    error.bars <- function(x, upper, lower){
    
      xlim <- range(x)
      barw <- diff(xlim)*0.02
      segments(x, upper, x, lower, ...)
      segments(x - barw, upper, x + barw, upper)
      segments(x - barw, lower, x + barw, lower)
      range(upper, lower)
    }
    
    # Label for y-axis
    if(x$glm_type=="Logistic")
      label.y.plot <- "CV Deviance" else
        label.y.plot <- "CV Error"
    
    # CV error plot
    plot(log(x$Lambda_Sparsity), x$CV_Errors, type="b", pch=20, lwd=2, col="red",
         xlab=(expression(log*phantom(x)*lambda[s])), ylab=label.y.plot)
    abline(v=log(x$Lambda_Sparsity_Min), lty=3)
  }
}






