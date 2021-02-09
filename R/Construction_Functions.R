# -----------------------------------------------------------------------
# Object Construction for SplitGLM object
#
# object: the SplitGLM object
# fn_call: the function call
# G: the number of groups
# -----------------------------------------------------------------------
construct.SplitGLM <- function(object, fn_call, G, glm_type){
  
  class(object) <- append("SplitGLM", class(object))
  object$call <- fn_call
  
  object$glm_type <- glm_type
  object$G <- G

  if(G==1){
    object$coef <- c(object$Intercept, object$Betas)
  } else{
    object$coef <- c(mean(object$Intercept), rowMeans(object$Betas))
  }
  
  return(object)
}

# -----------------------------------------------------------------------
# Object Construction for cv.SplitGLM object
#
# object: the cv.SplitGLM object
# fn_call: the function call
# G: the number of groups
# -----------------------------------------------------------------------
construct.cv.SplitGLM <- function(object, fn_call, G, glm_type){
  
  class(object) <- append("cv.SplitGLM", class(object))
  object$call <- fn_call
  
  object$glm_type <- glm_type
  object$G <- G
  
  if(G==1){
    object$coef <- c(object$Intercept[object$Optimal_Index], object$Betas[,object$Optimal_Index])
  } else{
    object$coef <- c(mean(object$Intercept[,object$Optimal_Index]), rowMeans(object$Betas[,,object$Optimal_Index]))
  }
  
  return(object)
}