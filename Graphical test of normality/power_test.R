source("QconBands_Aldor-Noiman.R")
source("QConBands_our_version.R")
library(rmutil)
library(extraDistr)


# power for data from 10 distributions
pw <- rep(0, 10)

num.trials <- 1000
sample.size <- 100

# just for sake of computing bands. The values are arbitrary since it does not affect simulation.
x <- rep(1, sample.size) 

# author's function
#bands <- QQ.UNcb(x.sample = x, plot = F)

# our function
bands <- norm_bands(sample.size = sample.size, method = "ks", estimate = "robust", M = 1000)

for (i in 1:num.trials) {
  cat("run ", i, "\n")
  dat <- matrix(nrow = sample.size, ncol = 10)
  dat[,1] <- rlnorm(sample.size)
  dat[,2] <- rchisq(sample.size, 1)
  dat[,3] <- rchisq(sample.size, 5)
  dat[,4] <- rchisq(sample.size, 100)
  dat[,5] <- rt(sample.size, 4)
  dat[,6] <- rlogis(sample.size)
  dat[,7] <- rpois(sample.size, 15)
  dat[,8] <- runif(sample.size, 1,18)
  dat[,9] <- rlaplace(sample.size,0,50)
  dat[,10] <- rnorm(sample.size, 5, 2)
  
  
  for (j in 1:length(dat[1,])) {
    x <- as.vector(dat[,j])
    out <- is.reject(x = x, upper = bands$upper, lower = bands$lower, estimate = "robust")
    if (out$is.reject == TRUE) {
      pw[j] = pw[j] + 1/num.trials
    }
  }
  cat(pw, "\n")
}

names(pw) <- c('log normal', 'chisq(1)', 'chisq(5)', 'chisq(100)', 'T(4)', 'logistic', 'poisson(15)',
               'uniform(1,18)', 'laplace(0,50)', 'normal(5,2)')
data.frame(pw)
