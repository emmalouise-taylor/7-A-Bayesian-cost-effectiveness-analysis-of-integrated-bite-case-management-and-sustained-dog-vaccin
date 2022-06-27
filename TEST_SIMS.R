
setwd("C:/Users/et00452/OneDrive - University of Surrey/OldHomeDrive/PhD/Haiti - CDC HARSP/R  Code/BCEA Model")
library(rstudioapi)
library(shiny)
library(shinyBS)
library(BCEA)
source("TEST_FUNCTION.R")
source("TEST_COSTS.R")

# Fixed Model parameters
HumanVaccineEfficacy <- .9 ## 90% efficacy
ParamsPEP <- betaPar2(0.54,.6,.95) ## Out of the total sample of patients who were reported to HARSP (by any means), which percentage were reported to HARSP from a medical institution in Haiti share of bite victims who seek PEP - mode of 54%, upper of 60% with 95% confidence
probInfHum <- 0.19 ## prob of develop rabies if exposed and not treated
ParamsProbOffWork <- betaPar2(.8,.85,.95) ## mode of 80%, upper of 85% with 95% confidence
ParamsLengthOffWork <- lognPar(2.9,1.25) ## time off work in days 

## Utilities
omegaDeath <- 0 #QALYDeath <- nInfHumans*omegaDeath ## humans infect die
omegaExpNoPEP <- .78 #  QALYExpNoPEP <- (nExpHumansNoPEP-nInfHumans)*omegaExpNoPEP #humans not treated who dont develop rabies ie nbitten by a non rabid dog? 
omegaPEP <- .78 # undurraga under HARSP where there is an assumption some bite victims will not seek PEP ie not full adherence 
omegaWork <- .1 #  QALYPEP <- (nBites-nExpHumansNoPEP)*omegaPEP + (nOffWork*lambda)/365*omegaWork ## humans with PEP with off work discount


## From the dynamic model sims#
DogVacCov <- .3 ### DogVacCov - defined by the scenario
nDog <- 800000   ## nDogs - sum nDogs across all pixels
RabiesPrev <- 8000/nDog ##calculated by prev *  nDogs ##this is number of infected dogs before interventiopn ## RabiesPrev - sum infected dogs across all pixels (yearly prevalence) - # modify this form disease dynamics model 
nsims <- 100

#results0 <-  vector(mode="list", length = 5) 
results30 <- vector(mode="list", length = 5) ##define here empty list - this will be c and e's
results55 <- vector(mode="list", length = 5) ##define here empty list - this will be c and e's
results70 <- vector(mode="list", length = 5) ##define here empty list - this will be c and e's

#create an array for rabies prev taking prevalence values at each time step from the disease dynamics model / total dogs 
#array0 <- c(137032,137032,137032,137032,137032)/nDog
array30 <- c(19670, 8473, 773, 25, 0)/nDog
array55 <- c(19784, 4067, 26, 0, 0)/nDog
array70 <- c(19289, 2743, 1, 0, 0)/nDog

SetupScenario <- c(F,F,F,F,F)
OngoingScenario <- T
#nsims repeating BCEA part not the number of sims from DD model 
for (i in 1:5){
  
   # set.seed(i)
   # results0[[i]] <- fun4(DogVacCov = 0, RabiesPrev=array0[i],
   #                       biteRate = 0.013, InfvsNoInfBiteRatio = 1,
   #                       nDog = 800000, nsims = 100, psiDogvacc = 0.29,
   #                       psivacc =14.45, NDoses = 5,  isSetupYear = SetupScenario[i])
   # 
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

#creates matrix (e,c) aggregated costs and effectiveness across 5 year ( first column is "e", second is "c")
#cande0 <-  Reduce("+",results0)
cande30 <- Reduce("+",results30) #"reduce" operates across arrays
cande55 <- Reduce("+",results55)
cande70 <- Reduce("+",results70)


### NO INTERVENTION #### 
treats <- c("No Intervention", "IBCM + Vaccination 70%")
m <- bcea(cbind(cande0[,1], cande70[,1]), cbind(cande0[,2], cande70[,2]), ##RS[,column1(e)], RS[,column2(c)]]
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)

### NO INTERVENTION #### 
treats <- c("No Intervention", "IBCM + Vaccination 55%")
m <- bcea(cbind(cande0[,1], cande55[,1]), cbind(cande0[,2], cande55[,2]), ##RS[,column1(e)], RS[,column2(c)]]
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)

### ALL INTERVENTIONS CONSIDREED #######
treats <- c("IBCM + Vaccination 30%","IBCM + Vaccination 55%", "IBCM + Vaccination 70%")#, "IBCM + Vaccination 70%") 
m <- bcea(cbind(cande70[,1], cande30[,1], cande55[,1]), cbind(cande70[,2], cande30[,2], cande55[,2]), ##RS[,column1(e)], RS[,column2(c)]]
          ref=2, intervention=treats, Kmax = 19000)
summary(m,wtp=19000)

 # treats <- c("IBCM + Vaccination 30%", "IBCM + Vaccination 70%")#, "IBCM + Vaccination 70%") 
 # m <- bcea(cbind(cande70[,1],  cande30[,1]), cbind(cande70[,2], cande30[,2]), ##RS[,column1(e)], RS[,column2(c)]]
 #         ref=2, intervention=treats, Kmax = 50000)
 # summary(m,wtp=50000) 
 # 
 # treats <- c("IBCM + Vaccination 55%", "IBCM + Vaccination 70%")
 #  n <- bcea(cbind(cande70[,1], cande55[,1]), cbind(cande70[,2], cande55[,2]), ##RS[,column1(e)], RS[,column2(c)]]
 #          ref=2, intervention=treats, Kmax = 50000)
 #  summary(n,wtp=50000) 


##input of function are:
#    e = the matrix containing the simulations from the posterior distributions of the variable of clinical effectiveness 
#    (nsim*nint values)
#    c = the matrix containing the simulations from the posteriorr distribution of the variable of cost 
#    (nsim*nitn values)
#    ref = an indication of which intervention is to be taken as reference 
#    (default: that intervention in teh first column of eo rc)
#    intervetion = a vector of labels for the interventions being cmapred 
#    Kmax = the maximum value of K, the parameter of willingness to pay 


print(m)
plot(m)
summary(m,wtp=50000) 
ceac.plot(m)  

#the price that decision makers would be willing to pay to gain access to perfect information (y-axis)
evi.plot(m)

eib.plot(m)

##generates contour plot of CE plane and estimates the proportion of points lying in ea quadrant 
contour(m)

##shows sustainability area
contour2(m)

ceplane.plot(m,comparison=1,Kmax=50000)



colors <- c("#009999", "#0000FF")

#using ggplot for cea
library(ggplot2)
p <- ceplane.plot(m,comparison = NULL, wtp = 50000, pos = ("bottom"),
                  size = 2, 
                  ICER_sizes=2, graph= "ggplot2",  xlim = NULL,
                  ylim = NULL) +
  labs(y= "Cost differential for a five year intervention (US$)", x = "Effectivenss differential (utilities)")+
  labs(title = NULL)
p + theme(legend.text = element_text(size=7))


plot(cande55[,1] - cande70[,1], cande55[,2] - cande70[,2])
plot(cande55[,1] - cande30[,1], cande55[,2] - cande30[,2])




