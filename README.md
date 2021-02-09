
[![Build Status](https://travis-ci.com/AnthonyChristidis/SplitGLM.svg?branch=master)](https://travis-ci.com/AnthonyChristidis/SplitGLM) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/SplitGLM)](https://cran.r-project.org/package=SplitGLM) [![Downloads](http://cranlogs.r-pkg.org/badges/SplitGLM)](https://cran.r-project.org/package=SplitGLM)

SplitGLM
========

This package provides functions for fitting split generalized linear models.

------------------------------------------------------------------------

### Installation

You can install the **stable** version on [R CRAN](https://cran.r-project.org/package=SplitGLM).

``` r
install.packages("SplitGLM", dependencies = TRUE)
```

You can install the **development** version from [GitHub](https://github.com/AnthonyChristidis/SplitGLM)

``` r
library(devtools)
devtools::install_github("AnthonyChristidis/SplitGLM")
```

### Usage

``` r
# Required Libraries
library(mvnfast)

# Sigmoid function
sigmoid <- function(t){
  return(exp(t)/(1+exp(t)))
}

# Data simulation
set.seed(1)
n <- 50
N <- 2000
p <- 1000
beta.active <- c(abs(runif(p, 0, 1/2))*(-1)^rbinom(p, 1, 0.3))
# Parameters
p.active <- 100
beta <- c(beta.active[1:p.active], rep(0, p-p.active))
Sigma <- matrix(0, p, p)
Sigma[1:p.active, 1:p.active] <- 0.5
diag(Sigma) <- 1

# Train data
x.train <- rmvn(n, mu = rep(0, p), sigma = Sigma) 
prob.train <- sigmoid(x.train %*% beta)
y.train <- rbinom(n, 1, prob.train)

# Test data
x.test <- rmvn(N, mu = rep(0, p), sigma = Sigma)
prob.test <- sigmoid(x.test %*% beta + offset)
y.test <- rbinom(N, 1, prob.test)
mean(y.test)
sp.sen.par <- y.test==0

# SplitGLM - CV (Multiple Groups)
split.out <- cv.SplitGLM(x.train, y.train,
                         type="Logistic",
                         G=10, include_intercept=TRUE,
                         alpha_s=3/4,
                         n_lambda_sparsity=100, n_lambda_diversity=100,
                         tolerance=1e-3, max_iter=1e3,
                         n_folds=5,
                         active_set=FALSE,
                         full_diversity=TRUE,
                         n_threads=1)

# Coefficients
split.coef <- coef(split.out)

# Predictions
split.prob <- predict(split.out, newx=x.test, type="prob")

# Plot of output
plot(prob.test, split.prob, pch=20)
abline(h=0.5,v=0.5)

# MR
split.class <- predict(split.out, newx=x.test, type="class")
mean(abs(y.test-split.class))
```

### License

This package is free and open source software, licensed under GPL (&gt;= 2).
