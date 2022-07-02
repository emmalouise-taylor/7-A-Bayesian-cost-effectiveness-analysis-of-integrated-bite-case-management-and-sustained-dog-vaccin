Epi <- function(RabiesPrev = 0.3,  biteRate = 0.013, 
                InfvsNoInfBiteRatio = 1,
                nDog = 800000, nsims = 100){  
  
  nBites <- rpois(nsims,nDog*biteRate) 
  
  nInfBites <- rpois(nsims, nBites*RabiesPrev*InfvsNoInfBiteRatio)
  
  probPEP <- rbeta(nsims,ParamsPEP$res1,ParamsPEP$res2) 
  
  nExpHumansPEP <- rbinom(nsims,nInfBites,probPEP)
  nNonExpHumansPEP <- rbinom(nsims,(nBites-nInfBites),probPEP)
  nPEPTreatments <- nExpHumansPEP + nNonExpHumansPEP
  
  nExpHumansNoPEP <- nInfBites - nExpHumansPEP 
  
  nExpHumansVac <- rbinom(nsims, nExpHumansNoPEP,probPEP) 
  
  nInfHumans <- rbinom(nsims, nExpHumansNoPEP-nExpHumansVac, probInfHum) +
    rbinom(nsims, nExpHumansVac, probInfHum*(1-HumanVaccineEfficacy))
  
  return(cbind(nBites, nInfBites, nExpHumansNoPEP, nExpHumansVac, nInfHumans, nPEPTreatments))
  
}

Eff <- function(NDoses = 5, nInfHumans = 100, nPEPTreatments = 150, 
                nExpHumansNoPEP = 200, nBites = 400, nsims = 100){ 
  
  prob.eta <- rbeta(nsims,ParamsProbOffWork$res1,ParamsProbOffWork$res2)
  nOffWork <- rbinom(nsims,nPEPTreatments,prob.eta)*NDoses 
  lambda <- rlnorm(nsims,ParamsLengthOffWork$mulog,ParamsLengthOffWork$sigmalog)
  
  QALYnoInf <- (nBites - nPEPTreatments - nExpHumansNoPEP)*omegaPEP 
  QALYPEP <- nPEPTreatments*omegaPEP - (nOffWork*lambda)/365*omegaWork 
  QALYExpNoPEPVac <- (nExpHumansNoPEP-nInfHumans)*omegaExpNoPEP 
  QALYDeath <- nInfHumans*omegaDeath 
  
  e <- QALYnoInf + QALYPEP + QALYExpNoPEPVac + QALYDeath 
  
  return(cbind(e,nOffWork))
  #return(cbind(e))
}

Cost <- function(nOffWork = 5, psiDogvacc = 0.29, psivacc = 14.45, 
                 nDog = 800000, DogVacCov = .3, nsims = 100, isSetupYear = T){
  
  set.seed(1)
  CostDogVacc <- rpois(nsims,(nDog*DogVacCov)) * psiDogvacc 
  CostPEP <-  psivacc * nOffWork
  
  if (isSetupYear == T){
    c <- psitotalsurveilanceSetUp + psitotaldiagnosisSetUp + psitotaltrainingSetUp + CostDogVacc + CostPEP
  } else {
    c <- psitotalsurveilanceOnGoing + psitotaldiagnosisOnGoing + psitotaltrainingOnGoing + CostDogVacc + CostPEP
  }
  

  return(c)
  
}  

fun4  <- function(DogVacCov = .3, RabiesPrev = 0.3, 
                  biteRate = 0.013, InfvsNoInfBiteRatio = 1,
                  nDog = 8*10^5, nsims = 100, psiDogvacc = 0.29, 
                  psivacc =14.45, NDoses = 5, isSetupYear = T) {
  
  rs <- Epi(RabiesPrev, biteRate, InfvsNoInfBiteRatio, nDog, nsims)
  
  e <-Eff(nInfHumans = rs[,5], nExpHumansNoPEP = rs[,3], nBites = rs[,1], nPEPTreatments=rs[,6], nsims = nsims, 
          NDoses = NDoses)
  
  c <- Cost(nDog=nDog, psiDogvacc=psiDogvacc, psivacc=psivacc, 
            nsims = nsims,DogVacCov = DogVacCov, isSetupYear = isSetupYear, nOffWork=e[,2])
  
  return (cbind(e[,1],c))
}

## Copyright Gianluca Baio 2012
betaPar <- function(m,s){
  a <- m*( (m*(1-m)/s^2) -1 )
  b <- (1-m)*( (m*(1-m)/s^2) -1 )
  list(a=a,b=b)
}

############################################################
## Compute the value of parameters (mulog,sigmalog) for a logNormal distribution to have mean and sd (m,s)
## Copyright Gianluca Baio 2012
lognPar <- function(m,s) {
  s2 <- s^2
  mulog <- log(m) - .5*log(1+s2/m^2)
  s2log <- log(1+(s2/m^2))
  sigmalog <- sqrt(s2log)
  list(mulog=mulog,sigmalog=sigmalog)
}


############################################################
## Compute the parameters of a Beta distribution, given a prior guess for:
##  mode = the mode of the distribution
##  upp  = an upper bound value for the distribution
##  prob = the estimated probability that (theta <= upp)
## Based on "Bayesian ideas and data analysis", page 100. 
## Optimisation method to identify the values of a,b that give required conditions on the Beta distribution
## Copyright Gianluca Baio 2012
betaPar2 <- function(mode,upp,prob){
  N <- 10000
  b <- 1:N
  a <- (1+mode*(b-2))/(1-mode)
  sim <- qbeta(prob,a,b)
  m <- ifelse(prob>=.5,max(which(sim>=upp)),min(which(sim>=upp)))
  M <- ifelse(prob>=.5,min(which(sim<=upp)),max(which(sim<=upp)))
  
  b <- min(m,M)+(b/N)
  a <- (1+mode*(b-2))/(1-mode)
  sim <- qbeta(prob,a,b)
  m <- ifelse(prob>=.5,max(which(sim>=upp)),min(which(sim>=upp)))
  M <- ifelse(prob>=.5,min(which(sim<=upp)),max(which(sim<=upp)))
  a <- ifelse(m==M,a[m],mean(a[m],a[M]))
  b <- ifelse(m==M,b[m],mean(b[m],b[M]))
  
  step <- 0.001
  theta <- seq(0,1,step)
  density <- dbeta(theta,a,b)
  
  norm.dens <- density/sum(density)
  cdf <- cumsum(norm.dens)
  M <- min(which(cdf>=.5))
  m <- max(which(cdf<=.5))
  
  theta.mode <- theta[which(density==max(density))]
  theta.mean <- a/(a+b)
  theta.median <- mean(theta[m],theta[M])
  theta.sd <- sqrt((a*b)/(((a+b)^2)*(a+b+1)))
  beta.params <- c(a,b,theta.mode,theta.mean,theta.median,theta.sd)
  res1 <- beta.params[1]
  res2 <- beta.params[2]
  theta.mode <- beta.params[3]
  theta.mean <- beta.params[4]
  theta.median <- beta.params[5]
  theta.sd <- beta.params[6]
  list(
    res1=res1,res2=res2,theta.mode=theta.mode,theta.mean=theta.mean,theta.median=theta.median,theta.sd=theta.sd)
}

