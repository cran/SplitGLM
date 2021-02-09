# -----------------------------------------------------------------------
# Checking Input Data for cv.SplitGLM Object
# -----------------------------------------------------------------------
Check_Data <- function(x, y, 
                       glm_type, 
                       G, 
                       alpha_s, alpha_d,
                       lambda_sparsity, lambda_diversity,
                       tolerance, max_iter){
  
  # Check for design matrix and response vector
  if(all(!inherits(x, "matrix"), !inherits(x, "data.frame"))) {
    stop("x should belong to one of the following classes: matrix, data.frame.")
  } else if(all(!inherits(y, "matrix"), !inherits(y, "numeric"), !inherits(y, "integer"))) { 
    stop("y should belong to one of the following classes: matrix, numeric, integer.")
  } else if(any(anyNA(x), any(is.nan(x)), any(is.infinite(x)))) {
    stop("x should not have missing, infinite or nan values.")
  } else if(any(anyNA(y), any(is.nan(y)), any(is.infinite(y)))) {
    stop("y should not have missing, infinite or nan values.")
  } else {
    if(inherits(y, "matrix")) {
      if(ncol(y)>1){
        stop("y should be a vector")
      }
      # Force to vector if input was a matrix
      y <- as.numeric(y)
    }
    len_y <- length(y)
    if(len_y != nrow(x)) {
      stop("y and x should have the same number of rows.")
    }
  }
  
  # Check number of groups
  if(!inherits(G, "numeric")) {
    stop("G should be numeric.")
  } else if(any(!G == floor(G), G < 1)) {
    stop("G should be an integer, one or greater.")
  }
  # Check tolerance
  if(!inherits(tolerance, "numeric")) {
    stop("tolerance should be numeric.")
  } else if(!all(tolerance < 1, tolerance > 0)) {
    stop("tolerance should be between 0 and 1.")
  }
  # Check alpha_s value
  if(!inherits(alpha_s, "numeric")) {
    stop("alpha_s should be numeric.")
  } else if(!all(alpha_s <= 1, alpha_s > 0)) {
    stop("alpha_s should be between 0 and 1.")
  }
  # Check alpha_d value
  if(!inherits(alpha_d, "numeric")) {
    stop("alpha_d should be numeric.")
  } else if(!all(alpha_d <= 1, alpha_d > 0)) {
    stop("alpha_d should be between 0 and 1.")
  }
  # Check maximum number of iterations
  if(!inherits(max_iter, "numeric")) {
    stop("max_iter should be numeric.")
  } else if(any(!max_iter == floor(max_iter), max_iter <= 0)) {
    stop("max_iter should be a positive integer.")
  }
  # Check input for lambda_sparsity 
  if(!inherits(lambda_sparsity, "numeric")) {
    stop("lambda_sparsity should be numeric.")
  } else if(lambda_sparsity < 0)  {
    stop("lambda_sparsity should be a positive.")
  }
  # Check input for lambda_diversity 
  if(!inherits(lambda_diversity, "numeric")) {
    stop("lambda_diversity should be numeric.")
  } else if(lambda_diversity < 0)  {
    stop("lambda_diversity should be a positive.")
  }
}

# -----------------------------------------------------------------------
# Checking Input Data for cv.SplitGLM Object
# -----------------------------------------------------------------------
CV_Check_Data <- function(x, y, 
                          glm_type, 
                          G, 
                          alpha_s, alpha_d,
                          n_lambda_sparsity, n_lambda_diversity,
                          tolerance, max_iter,
                          n_folds,
                          n_threads){
  
  # Check for design matrix and response vector
  if(all(!inherits(x, "matrix"), !inherits(x, "data.frame"))) {
    stop("x should belong to one of the following classes: matrix, data.frame.")
  } else if(all(!inherits(y, "matrix"), !inherits(y, "numeric"), !inherits(y, "integer"))) {
    stop("y should belong to one of the following classes: matrix, numeric, integer.")
  } else if(any(anyNA(x), any(is.nan(x)), any(is.infinite(x)))) {
    stop("x should not have missing, infinite or nan values.")
  } else if(any(anyNA(y), any(is.nan(y)), any(is.infinite(y)))) {
    stop("y should not have missing, infinite or nan values.")
  } else {
    if(inherits(y, "matrix")) {
      if(ncol(y)>1){
        stop("y should be a vector")
      }
      # Force to vector if input was a matrix
      y <- as.numeric(y)
    }
    len_y <- length(y)
    if(len_y != nrow(x)) {
      stop("y and x should have the same number of rows.")
    }
  }
  
  # Check number of groups
  if(!inherits(G, "numeric")) {
    stop("G should be numeric.")
  } else if(any(!G == floor(G), G < 1)) {
    stop("G should be an integer, one or greater.")
  }
  # Check tolerance
  if(!inherits(tolerance, "numeric")) {
    stop("tolerance should be numeric.")
  } else if(!all(tolerance < 1, tolerance > 0)) {
    stop("tolerance should be between 0 and 1.")
  }
  # Check alpha_s value
  if(!inherits(alpha_s, "numeric")) {
    stop("alpha_s should be numeric.")
  } else if(!all(alpha_s <= 1, alpha_s > 0)) {
    stop("alpha_s should be between 0 and 1.")
  }
  # Check alpha_d value
  if(!inherits(alpha_d, "numeric")) {
    stop("alpha_d should be numeric.")
  } else if(!all(alpha_d <= 1, alpha_d > 0)) {
    stop("alpha_d should be between 0 and 1.")
  }
  # Check maximum number of iterations
  if(!inherits(max_iter, "numeric")) {
    stop("max_iter should be numeric.")
  } else if(any(!max_iter == floor(max_iter), max_iter <= 0)) {
    stop("max_iter should be a positive integer.")
  }
  # Check input for number of candidates for sparsity value
  if(!inherits(n_lambda_sparsity, "numeric")) {
    stop("n_lambda_sparsity should be numeric")
  } else if(any(!n_lambda_sparsity == floor(n_lambda_sparsity), n_lambda_sparsity <= 0)) {
    stop("n_lambda_sparsity should be a positive integer")
  }
  # Check input for number of candidates for diversity value
  if(!inherits(n_lambda_diversity, "numeric")) {
    stop("n_lambda_diversity should be numeric")
  } else if(any(!n_lambda_diversity == floor(n_lambda_diversity), n_lambda_diversity <= 0)) {
    stop("n_lambda_diversity should be a positive integer")
  }
  # Check input for number of folds
  if(!inherits(n_folds, "numeric")) {
    stop("n_folds should be numeric")
  } else if(any(!n_folds == floor(n_folds), n_folds <= 0)) {
    stop("n_folds should be a positive integer")
  }
  # Check input for number of threads
  if(!inherits(n_threads, "numeric")) {
    stop("n_threads should be numeric")
  } else if(any(!n_threads == floor(n_threads), n_threads <= 0)) {
    stop("n_threads should be a positive integer")
  }
}

# -----------------------------------------------------------------------
# Checking Response Data for cv.SplitGLM Object
# -----------------------------------------------------------------------

# Modifying response input data
Check_Response <- function(y, glm_type){

  if(glm_type=="Logistic"){
    
    if(length(unique(y))!=2)
      stop("The response vector \"y\" must contain at most 2 classes if \"glm_type\" is \"Logistic\".") else{
        
        if(!all(y %in% c(0,1)))
          return(ifelse(y==y[1], 1, 0)) else
            return(y)
      }

  } else if(glm_type=="Poisson"){
    
    if(round(y)!=y || any(y<0))
      stop("The response vector \"y\" must contain count data (non-negative) if \"glm_type\" is \"Poisson\".") else
        return(y)
    
  } else if(glm_type=="Gamma"){
    
    if(any(y<0))
      stop("The response vector \"y\" must contain only non-negative values if \"glm_type\" is \"Gamma\".") else
        return(y)
  } else if(glm_type=="Linear")
    return(y)
}