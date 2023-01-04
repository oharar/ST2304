# Function to simulate data from globe tossing experiment
# Arguments
#   probability - probability of "success", default: 0.5
#   NTrials - number of globe tosses. Default: 10
simGlobe <- function(probability=0.5, NTrials=10, nSims=1) {
  OneSim <- function(Prob, NTr) {    
    sim <- rbinom(NTr, 1, Prob)
  # this next line changes so the function will return 
  # zero counts, rather than leave them off
  table(factor(c("Sea", "Land")[1+sim], 
                levels = c("Sea", "Land")))
  }
  if(nSims==1) {
    res <- OneSim(Prob=probability, NTr=NTrials)
  } else {
    res <- replicate(nSims, OneSim(Prob=probability, NTr=NTrials))
  }
  return(res)
} 


# Function to calculate MLE for a binomial distribution from globe tossing experiment
# Arguments
#   NLand - number of "Land"s
#   NTrials - number of globe tosses. Default: 10
mleGlobe <- function(NLand, NTrials) {
# Some defensive programming, to protect against errors
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(length(NTrials)!=1 & length(NTrials)!=length(NLand)) stop("NTrials should be a single number, or the same length as NLand")
  if(any(!is.wholenumber(NLand))) stop("Land must be an integer")
  if(any(!is.wholenumber(NTrials))) stop("NTrials must be an integer")
  if(any(NLand<0)) stop("Land must be at least 0")
  if(any(NTrials<1)) stop("NTrials must be at least 1")
  if(any(NLand>NTrials)) stop("Land cannot be greater than NTrials")

  # calculate the mle
  NLand/NTrials
} 


# Function to calculate CI for the probability of success for a binomial distribution from globe tossing experiment
# Arguments
#   NLand - number of "Land"s
#   NTrials - number of globe tosses. Default: 10
CIGlobe <- function(NLand, NTrials) {
  # Some defensive programming, to protect against errors
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(length(NTrials)!=1 & length(NTrials)!=length(NLand)) stop("NTrials should be a single number, or the same length as NLand")
  if(any(!is.wholenumber(NLand))) stop("Land must be an integer")
  if(any(!is.wholenumber(NTrials))) stop("NTrials must be an integer")
  if(any(NLand<0)) stop("Land must be at least 0")
  if(any(NTrials<1)) stop("NTrials must be at least 1")
  if(any(NLand>NTrials)) stop("Land cannot be greater than NTrials")
  
  MLE <- NLand/NTrials
  # calculate the CI
  if(length(MLE)>1) {
    res <- t(sapply(MLE, function(mle, NN) qbinom(c(0.025, 0.975), size = NN, prob = mle)/NN, NN=NTrials))
    colnames(res) <- c("Lower", "Upper")
  } else {
    res <- qbinom(c(0.025, 0.975), size = NTrials, prob = NLand/NTrials)/NTrials
    names(res) <- c("Lower", "Upper")
  }
  res
} 


# Function to calculate assymptotic CI for the probability of success for a 
# binomial distribution from globe tossing experiment
# Arguments
#   NLand - number of "Land"s
#   NTrials - number of globe tosses. Default: 10
AssymptoticCIGlobe <- function(NLand, NTrials) {
  # Some defensive programming, to protect against errors
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(length(NTrials)!=1 & length(NTrials)!=length(NLand)) stop("NTrials should be a single number, or the same length as NLand")
  if(any(!is.wholenumber(NLand))) stop("Land must be an integer")
  if(any(!is.wholenumber(NTrials))) stop("NTrials must be an integer")
  if(any(NLand<0)) stop("Land must be at least 0")
  if(any(NTrials<1)) stop("NTrials must be at least 1")
  if(any(NLand>NTrials)) stop("Land cannot be greater than NTrials")
  
  MLE <- NLand/NTrials
  SE <- sqrt(MLE*(1-MLE)/NTrials)
  
  res <- MLE - 1.96*SE
  # calculate the CI
  if(length(MLE)>1) {
    res <- cbind(MLE - 1.96*SE, MLE + 1.96*SE)
    colnames(res) <- c("Lower", "Upper")
  } else {
    res <- c(MLE - 1.96*SE, MLE + 1.96*SE)
    names(res) <- c("Lower", "Upper")
  }
  res
} 


# Function to calculate assymptotic CI for the probability of success for a 
# binomial distribution from globe tossing experiment
# Arguments
#   NLand - number of "Land"s
#   NTrials - number of globe tosses. Default: 10
StdErrGlobe <- function(NLand, NTrials) {
  # Some defensive programming, to protect against errors
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(length(NTrials)!=1 & length(NTrials)!=length(NLand)) stop("NTrials should be a single number, or the same length as NLand")
  if(any(!is.wholenumber(NLand))) stop("Land must be an integer")
  if(any(!is.wholenumber(NTrials))) stop("NTrials must be an integer")
  if(any(NLand<0)) stop("Land must be at least 0")
  if(any(NTrials<1)) stop("NTrials must be at least 1")
  if(any(NLand>NTrials)) stop("Land cannot be greater than NTrials")
  
  MLE <- NLand/NTrials
  sqrt(MLE*(1-MLE)/NTrials)
} 


