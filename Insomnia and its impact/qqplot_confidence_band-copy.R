## Displays the normal qqplot for the input data vector. Computes the confidence bands based
## on the Kolmogorov-Smirnov statistic or Tail-sensitive simulation. For the KS bands, the
## function from extRemes package is used. https://rdrr.io/cran/extRemes/man/qqnorm.html
##
## x --- data vector
## method --- specify whether to use KS bands or TS bands
## mu,sigma --- parameters for the normal distribution.
## if both mu and sigma are given, compute the ts bands assuming the parameters are known with
## the given mu and sigma. 
## if either of mu or sigma is not given, compute the ts bands with the unknown parameters.
## mu and sigma are estimated by sample mean and sample standard deviation.
## M --- number of simulation. Default is 5000.
## alpha --- coverage rate is specified to be 1-alpha. Default is 0.05.
qqnorm_bands <- function(x, method = c("ks", "ts"), mu, sigma, M = 5000, alpha = 0.05) {
  library(extRemes)
  if (method == "ks") {
    qqnorm(x)
  } else if (method == "ts") {
    n <- length(x)
    p <- (1:length(x) - 0.5)/length(x)
    q <- qnorm(p)
    x <- sort(x)
    if (!missing(mu) && !missing(sigma)) {
      # known parameters
      x <- (x-mu)/sigma
    } else {
      # unknown parameters
      x <- (x-mean(x))/sd(x)
    }
    C <- rep(0,M)
    for (m in 1:M) {
      if (!missing(mu) && !missing(sigma)) {
        # known parameters
        y <- runif(n = n, min = 0, max = 1)
      } else {
        # unknown parameters
        xm <- rnorm(n = n, mean = 0, sd = 1)
        zm <- (xm-mean(xm))/sd(xm)
        y <- pnorm(zm)
      }
      y <- sort(y)
      # Find minimum confidence level C_m for each simulation
      B <- rep(0, n)
      for (i in 1:n) {
        B[i] <- min(c(qbeta(p = y[i], shape1 = i, shape2 = n-i+1), 1 - qbeta(p = y[i], shape1 = i, shape2 = n-i+1)))  
      }
      C[m] <- 2 * min(B)
    }
    # Set gamma to be the 100 alpha percentile of C_1,...C_M
    gamma <- quantile(C, probs = alpha)
    l <- rep(0, n)
    u <- rep(0, n)
    for (i in 1:n) {
      # lower TS band
      l[i] <- qnorm(qbeta(p = gamma/2, shape1 = i, shape2 = n-i+1))
      # upper TS band
      u[i] <- qnorm(qbeta(p = 1-gamma/2, shape1 = i, shape2 = n-i+1))
    }
    plot(q, x, xlab = "Standard Normal Quantiles", ylab = "Standardized Sample Quantiles", pch = 20)
    lines(l, x, lty = 2, col = "blue")
    lines(u, x, lty = 2, col = "blue")
  }
}

# Some experiments
x <- rnorm(50)
qqnorm_bands(x = x, method = "ks", mu = 0, sigma = 1)
x <- rexp(50)
qqnorm_bands(x = x, method = "ts", mu = 0, sigma = 1)