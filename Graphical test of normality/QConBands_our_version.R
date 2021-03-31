# This is a split version of the original function which separates the part to compute 
# the gamma (and the band) and the part to test whether we reject the normality based 
# on the sample data.
# -----------------------------------------------------------------------------------------

# for robust estimates for the mean and sd
library(robustbase) 
Qn.scale<-function(x){
  Qn(x,finite.corr=FALSE)
}
Qn.location<-function(x){
  s_Qn(x,mu.too=TRUE)[[1]]
}

## Computes the standardized confidence bands for testing normality 
## based on the Kolmogorov-Smirnov statistic or Tail-sensitive simulation. 
## For the KS bands, the function from extRemes package is adapted. 
## https://rdrr.io/cran/extRemes/man/qqnorm.html 
##
## sample.size --- sample size of the data to test the normality.
## method --- specify whether to use KS bands or TS bands or both.
## estimate --- estimators to use. Either MLEs or (median, Q_n), the robust estimators for 
##    the location and scale suggested by Croux, C. and Rousseeuw, P.J. (1993). The 
##    functions from robustbase package are adapted.
##    if note specified, then the parameters are assumed to be known.
## M --- number of simulation. Default is 5000.
## alpha --- coverage rate is specified to be 1-alpha. Default is 0.05.
##
##
## return a list of objects:
##  ts.bands --- dataframe consisting of the TS confidence bands. 1st column is the lower band,
##    2nd column is the upper band.
##  ks.bands --- dataframe consisting of the KS confidence bands. 1st column is the lower band,
##    2nd column is the upper band.
norm_bands <- function(sample.size, method = c("ks", "ts"), estimate = c("known","mle","robust"), 
                         M = 5000, alpha = 0.05) {
  
  method <- match.arg(method)
  estimate <- match.arg(estimate)
  
  out <- list()
  n <- sample.size
  # theoretical quantile
  p <- (1:n - 0.5)/n
  q <- qnorm(p)
  
  
  # KS bands
  if (method == "ks") {
    k <- 0.895/(sqrt(n) * (1 - 0.01/sqrt(n) + 0.85/n))
    l <- suppressWarnings(qnorm(max(p - k,0)))
    u <- suppressWarnings(qnorm(min(p + k,1)))
    out[["lower"]] <- l
    out[["upper"]] <- u
  } 
  
  # TS bands
  if (method == "ts") {
    C <- rep(0,M)
    for (m in 1:M) {
      if (estimate == "known") {
        # known parameters
        y <- runif(n = n, min = 0, max = 1)
      } else {
        # unknown parameters
        xm <- rnorm(n = n, mean = 0, sd = 1)
        if (estimate == "mle") {
          mu <- mean(xm)
          sigma <- sd(xm)
        }
        if (estimate == "robust") {
          mu <- Qn.location(xm)
          sigma <- Qn.scale(xm)
        }
        zm <- (xm-mu)/sigma
        y <- pnorm(zm)
      }
      y <- sort(y)
      # Find minimum confidence level C_m for each simulation
      p.value <- rep(0, n)
      for (i in 1:n) {
        beta <- pbeta(q = y[i], shape1 = i, shape2 = n-i+1)
        p.value[i] <- min(c(beta, 1 - beta))  
      }
      C[m] <- 2 * min(p.value)
    }
    # Set gamma to be the 100*alpha percentile of C_1,...C_M
    gamma <- quantile(C, probs = alpha)
    l <- rep(0, n)
    u <- rep(0, n)
    for (i in 1:n) {
      l[i] <- qnorm(qbeta(p = gamma/2, shape1 = i, shape2 = n-i+1))
      u[i] <- qnorm(qbeta(p = 1-gamma/2, shape1 = i, shape2 = n-i+1))
    }
    ts.bands <- cbind("lower"=l, "upper"=u)
    out[["lower"]] <- l
    out[["upper"]] <- u
  }
  return(out)
}




## tests whether to reject the normality of the sample with the given bands. Also provides the 
## option to plot the qqplot along with the confidence bands.
##
## mu,sigma --- parameters for the hypothesized normal distribution
## upper --- upper confidence bands
## lower --- lower confidence bands
## title --- title for the plot.
## plot --- whether to display the plot.
##
## return a list of objects:
##  reject.point --- data points that fall outside of the confidence bands.
##  reject.index --- the index of the ordered data points that fall outside of the confidence bands.
##  is.reject --- indicates whether rejecting that data comes from normal distributuion. 
is.reject <- function(x, mu, sigma, upper, lower, estimate = c("known","mle","robust"), title = "", plot = F) {
  out <- list()
  # theoretical quantile
  p <- (1:length(x) - 0.5)/length(x)
  q <- qnorm(p)

  
  if (missing(mu) || missing(sigma)) {
    # unknown parameters
    if (estimate == "mle") {
      mu <- mean(x)
      sigma <- sd(x)
    }
    if (estimate == "robust") {
      mu <- Qn.location(x)
      sigma <- Qn.scale(x)
    }
  }
  x <- sort(x)
  # standardize sample
  x <- (x-mu)/sigma
  
  if (plot) {
    plot(q, x, xlim = c(-3,3), ylim = c(-3,3), xlab = "Standard Normal Quantiles",
         ylab = "Standardized Sample Quantiles", main = title, pch = 20)
    lines(q, lower, lty = 2, col = "blue")
    lines(q, upper, lty = 2, col = "blue")
  }
  
  is.reject <- c()
  reject.point <- x[(lower > x) | (x > upper)]
  reject.index <- which((lower > x) | (x > upper))
  is.reject <- length(reject.index) > 0
  out[["reject.point"]] <- reject.point
  out[["reject.index"]] <- reject.index
  out[["is.reject"]] <- is.reject
  
  return(out)
}
