#full dataset
library(extraDistr)

n <- 100
sample.size <- n

dat <- matrix(nrow = n, ncol = 10)
dat[,1] <- rlnorm(sample.size) # log normal
dat[,2] <- rchisq(sample.size, 1) # chisquare 1
dat[,3] <- rchisq(sample.size, 5) # chisquare 2
dat[,4] <- rchisq(sample.size, 100) # chisquare 100 #0 power?
dat[,5] <- rt(sample.size, 4) # t distribution 4
dat[,6] <- rlogis(sample.size) # logistic distribution
dat[,7] <- rpois(sample.size, 15) # poisson 15 #0 power?
dat[,8] <- runif(sample.size, 1,18) # unif 1-18
dat[,9] <- rlaplace(sample.size,0,50) # laplace u 0 b 50
dat[,10] <- rslash(sample.size) # r slash 

# parameters
# kernel density estimation function
f_hat.function <- function(x, sample, h) {
  n <- length(sample)
  k <- dnorm((x - sample) / h)
  f <- sum(k)
  f <- f / (n * h)
  return (f)
}


## for all data
power <- c()
log_f_hat <- c()
e_obs <- c()

for (z in 1:10) { ## For each data column
  sample <- dat[,z]
  n <- length(sample)
  mean.trimmed.estimate <- mean(sample, trim = 0.1) # trimmed mean
  sd.robust.estimate <- sum(abs(sample-mean(sample)))/n * 1.4826 # robust estimate of standard deviation - mean standard deviation (multiplied by 1.4826)
  h_ns <- 1.06 * sd.robust.estimate * (n ^ (-1/5)) # normal scale bandwidth h
  
  for (i in 1:n) { 
    log_f_hat[i] = log(f_hat.function(sample[i], sample, h_ns)) # log f hat
    e_obs[i] = log_f_hat[i] - log(dnorm(sample[i], mean = mean.trimmed.estimate, sd = sd.robust.estimate)) #error - deviation of kernel estimate from theoretical normal
  }
  s_obs <- sum(e_obs^2) ## Observed sum of squared error - test statistic
  
  iterations <- 1000 #500 for efficiency
  pvalues <- c()
  
  # power analysis - 50 iterations
  for (power.index in 1:50) {
    s_monte <- c()
    
    # monte carlo simulation
    for (j in 1:iterations) {
      monte.normal.sample <- rnorm(n, mean = 0, sd = sd.robust.estimate) ## simulated normal sample with robust sd estimate
      
      monte.mean <- mean(monte.normal.sample, trim = 0.1) # trimmed mean for this normal sample
      monte.sd <- sum(abs(monte.normal.sample-mean(monte.normal.sample)))/n * 1.4826 # sd for this normal sample
      monte.h_ns <- 1.06 * monte.sd * (n ^ (-1/5)) # bandwidth for this normal sample
      
      log_monte_f <- c()
      e_monte <- c()
      for (k in 1:n) {
        log_monte_f[k] = log(f_hat.function(monte.normal.sample[k], monte.normal.sample, monte.h_ns))
        e_monte[k] = log_monte_f[k] - log(dnorm(monte.normal.sample[k], mean = monte.mean, sd = monte.sd))
      }
      
      s_monte <- c(s_monte, sum(e_monte^2)) #Vector of sum of squared error for all instances of monte carlo simulations
    }
    
    # pvalues
    this.pvalue <- sum(s_monte >= s_obs) / iterations # monte carlo pvalue - equals to the sum of squared errors for monte carlo simulations greater or equal to the observed one
    pvalues <- c(pvalues, this.pvalue)
  }
  
  #power vector with alpha level of 0.05
  power <- c(power, mean(pvalues < 0.05))
}



power