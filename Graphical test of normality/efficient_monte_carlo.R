library(rmutil)
library(extraDistr)
library(ks)

# parameters
n <- 100
sample.size <- n
iterations <- 1000 # number of iterations for Monte Carlo procedure
nSim <- 1000 # number of simulations for one calculation of powers
npower <- 50 # number of calculations of powers


# kernel density estimation
f_hat.function <- function(x, sample, h) {
  n <- length(sample)
  k <- dnorm((x - sample) / h)
  f <- sum(k)
  f <- f / (n * h)
  return (f)
}

# Compute the statistic for Monte Carlo procedure
monte.carlo.statistic <- function(x) {
  n <- length(x)
  monte.mean <- mean(x, trim = 0.1) # trimmed mean
  monte.sd <- sd.robust.estimate(x) # sd for this normal sample
  z <- (x-monte.mean)/monte.sd
  monte.h_ns <- 1.06 * 1 * (n ^ (-1/5)) # bandwidth for this normal sample
  
  log_monte_f <- log(kde(x = z, h = monte.h_ns, eval.points = z)$estimate)
  e_monte <- log_monte_f - log(dnorm(z, mean = 0, sd = 1))
  
  s_monte <- sum(e_monte^2) #sum of squared error
  return(s_monte)
}

# Compute the robust estimate of the standard deviation - mean standard deviation (multiplied by 1.4826)
sd.robust.estimate <- function(x) {
  return(sum(abs(x-mean(x)))/n * 1.4826)
}


# monte carlo simulation
s_monte <- matrix(nrow = iterations, ncol = 10)
for (j in 1:iterations) {
  for (i in 1:10) {
    monte.normal.sample <- rnorm(n, mean = 0, sd = 1) ## simulated normal sample with robust sd estimate
    s_monte[j,i] <- monte.carlo.statistic(monte.normal.sample) #Vector of sum of squared error for all instances of monte carlo simulations
  }
}

# power calculations - npower times
pw.df <- matrix(nrow = npower, ncol = 10)
for (power.index in 1:npower) {
  power <- rep(0, 10)
  for (sim in 1:nSim) {
    dat <- matrix(nrow = n, ncol = 10)
    dat[,1] <- rlnorm(sample.size) # log normalkde
    dat[,2] <- rchisq(sample.size, 1) # chisquare 1
    dat[,3] <- rchisq(sample.size, 5) # chisquare 5
    dat[,4] <- rchisq(sample.size, 100) # chisquare 100 #0 power?
    dat[,5] <- rt(sample.size, 4) # t distribution 4
    dat[,6] <- rlogis(sample.size) # logistic distribution
    dat[,7] <- rpois(sample.size, 15) # poisson 15 #0 power?
    dat[,8] <- runif(sample.size, 1,18) # unif 1-18
    dat[,9] <- rlaplace(sample.size,0,50) # laplace u 0 b 50
    dat[,10] <- rnorm(sample.size, 5, 2) # normal (5,2)
    
    # observed statistic
    s_obs <- apply(dat,2,monte.carlo.statistic)

    # pvalues
    pvalues <- rep(0, length(dat[1,]))
    for (k in 1:length(pvalues)) {
      # monte carlo pvalue - equals to the sum of squared errors for monte carlo simulations greater or equal to the observed one
      pvalues[k] <- sum(s_monte[,k] >= s_obs[k]) / iterations 
    }
    
    power <- power + (pvalues < 0.05) * 1 / nSim
  }
  cat(power.index, ": ", power, "\n")
  pw.df[power.index,] <- power
}



# label the distribution
colnames(pw.df) <- c("log normal", "chisq(1)", "chisq(5)", "chisq(100)", "T(4)", "logistic", "Poisson(15)", "uniform(1,18)",
                     "laplace(0,50)", "normal(5,2)")
names(power) <- c("log normal", "chisq(1)", "chisq(5)", "chisq(100)", "T(4)", "logistic", "Poisson(15)", "uniform(1,18)",
                  "laplace(0,50)", "normal(5,2)")

# save the data
write.csv(x=pw.df, file="efficient_monte_carlo_powers_n=100.csv")

# read the data
pw.df <- read.csv("efficient_monte_carlo_powers_n=100.csv")
pw.df <- pw.df[,2:11]

# mean and sd of power
colMeans(pw.df) # efficient Monte Carlo
apply(X = pw.df, MARGIN = 2, FUN = sd)
