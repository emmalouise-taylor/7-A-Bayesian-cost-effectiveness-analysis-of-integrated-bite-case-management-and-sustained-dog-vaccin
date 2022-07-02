
library(rstudioapi)
library(shiny)
library(shinyBS)
library(BCEA)
source("TEST_FUNCTION.R")
source("TEST_COSTS.R")

HumanVaccineEfficacy <- .9 
ParamsPEP <- betaPar2(0.54,.6,.95) 
probInfHum <- 0.19 
ParamsProbOffWork <- betaPar2(.8,.85,.95) 
ParamsLengthOffWork <- lognPar(2.9,1.25) 

omegaDeath <- 0 
omegaExpNoPEP <- .78 P
omegaPEP <- .78 
omegaWork <- .1 

DogVacCov <- .3 
nDog <- 800000   
RabiesPrev <- 8000/nDog  
nsims <- 100
 
results30 <- vector(mode="list", length = 5) 
results55 <- vector(mode="list", length = 5) 
results70 <- vector(mode="list", length = 5) 

array30 <- c(19670, 8473, 773, 25, 0)/nDog
array55 <- c(19784, 4067, 26, 0, 0)/nDog
array70 <- c(19289, 2743, 1, 0, 0)/nDog

SetupScenario <- c(F,F,F,F,F)
OngoingScenario <- T

for (i in 1:5){
  
   set.seed(i)
   results0[[i]] <- fun4(DogVacCov = 0, RabiesPrev=array0[i],
                          biteRate = 0.013, InfvsNoInfBiteRatio = 1,
                          nDog = 800000, nsims = 100, psiDogvacc = 0.29,
                          psivacc =14.45, NDoses = 5,  isSetupYear = SetupScenario[i])
    
  set.seed(i)
  results30[[i]] <- fun4(DogVacCov = .3, RabiesPrev=array30[i],
                         biteRate = 0.013, InfvsNoInfBiteRatio = 1,
                         nDog = 800000, nsims = 100, psiDogvacc = 0.29, 
                         psivacc =14.45, NDoses = 5,  isSetupYear = SetupScenario[i])
  set.seed(i) 
  results55[[i]] <- fun4(DogVacCov = .55,  RabiesPrev=array55[i],
                       biteRate = 0.013, InfvsNoInfBiteRatio = 1,
                       nDog = 800000, nsims = 100, psiDogvacc = 0.29, 
                       psivacc =14.45, NDoses = 5, isSetupYear = SetupScenario[i])
  set.seed(i)
  results70[[i]] <- fun4(DogVacCov = .7,  RabiesPrev=array70[i],
                         biteRate = 0.013, InfvsNoInfBiteRatio = 1,
                         nDog = 800000, nsims = 100, psiDogvacc = 0.29, 
                         psivacc =14.45, NDoses = 5,isSetupYear = SetupScenario[i])
}


cande0 <-  Reduce("+",results0)
cande30 <- Reduce("+",results30) #"reduce" operates across arrays
cande55 <- Reduce("+",results55)
cande70 <- Reduce("+",results70)



treats <- c("No Intervention", "IBCM + Vaccination 70%")
m <- bcea(cbind(cande0[,1], cande70[,1]), cbind(cande0[,2], cande70[,2]), 
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)


treats <- c("No Intervention", "IBCM + Vaccination 55%")
m <- bcea(cbind(cande0[,1], cande55[,1]), cbind(cande0[,2], cande55[,2]), 
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)


treats <- c("IBCM + Vaccination 30%","IBCM + Vaccination 55%", "IBCM + Vaccination 70%")#, "IBCM + Vaccination 70%") 
m <- bcea(cbind(cande70[,1], cande30[,1], cande55[,1]), cbind(cande70[,2], cande30[,2], cande55[,2]), 
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)














