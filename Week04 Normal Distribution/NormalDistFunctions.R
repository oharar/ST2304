# This function calculate the log-likelihood for a vector of observations,
#  assuming it follows a normal distribution with the specified mean(mu) and 
# standard deviation (sigma)
CalcNormlogLh <- function(mu, sigma, data) {
  lhood <- sum(dnorm(data, mean=mu, sd=sigma, log=TRUE))
  lhood
}

# Simulate N data points from a normal disitrbution, then calculate the mean
SimNormMeans <- function(mu, sigma, N, NReps=10) {
  replicate(NReps, mean(rnorm(N, mu, sigma)))
}

# Calcualte the confidence interval for the mean of data
CalcNormalMeanCI <- function(data) {
  muhat <- mean(data)
  # Use MLE for sigma, but var() uses the 'better' estimate  
  sigmahat <- sd(data)*sqrt(1-1/length(data))
  
  SE <- sigmahat/sqrt(length(data))
  CI <- c(muhat + qt(0.025, length(data)-1)*SE, muhat + qt(0.975, length(data)-1)*SE)
  CI
}



# This function simulates the data for a t-test, and returns the difference between the 2 groups
# XX is the group of the variable, i\either 0 or 1
# YY is the response
# nSims should be set to be much larger than 5
SimttestLhood <- function(XX, YY, nSims=5) {
  if(any(!XX%in%c(0,1))) stop("XX should be 0 or 1")
  if(length(XX)!=length(YY)) stop("XX na dYY should be same length")
  # Calculate statistics
  n <- length(YY)
  mu0 <- mean(YY[XX==0])
  mu1 <- mean(YY[XX==1])
  diff <- mu1 - mu0
  mu <- mu0 + XX*diff
  sigma2 <- var(YY-mu)*(n-1)/n
  
  # Simulate the data, and calculate the difference between the two groups
  simdat <- function(n, x, mu, s2) {
    SimY <- rnorm(n, mu, sqrt(s2))
    sim.diff <- mean(SimY[x==1]) - mean(SimY[x==0])
    sim.diff
  }
  DiffDist <- replicate(nSims, simdat(n=n, x=XX, mu=mu, s2=sigma2))
  DiffDist
}
