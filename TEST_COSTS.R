
#capital costs - incurred once at set up 
#operational costs - ongoing costs incurred each year 

                        ############################################################
                                              ##DIAGNOSIS / year (USD)
#CAPITAL
psidiagcapital <- 4195 #includes microscope, incubator, freezer etc

#OPERATIONAL
psiequipmentmaintence <- 500
psilaboratoryrental <- 200
psiutilities <- 20
psidiagoperational <- psiequipmentmaintence + psilaboratoryrental + psiutilities

#PERSONEL
nlaboratorytech <- 2
psilaboratorytechsalary <- 1575 
psidiagpersonel <- nlaboratorytech *psilaboratorytechsalary

psitotaldiagnosisSetUp <- psidiagcapital + psidiagoperational + psidiagpersonel ##total cost of setting up diagnostics 
psitotaldiagnosisOnGoing <- psidiagoperational + psidiagpersonel #total cost of maintaining diagnostics  

                          ###########################################################
                                                      ##SURVEILLANCE/ Year
#CAPITAL 
psisurvcapital <- 1572 #includes animal capture equipment, comms material

#OPERATIONAL 
nvehicle <- 2 #number of vehicles
psivehiclegas <- 22 * nvehicle #cost of gas per vehicle 
psivehiclecost <- 88 + psivehiclegas # overall unit value ($88/vehicle) + cost gas
psitotalvehcilecost <- psivehiclecost * nvehicle #overall cost/vehicle * number of vehicles
utilities <- 1000
psiofficerental <- 4000 + utilities #rental of office space

#PERSONEL  
nvettech <- 3
techsalary <- 9000  # vet tech salary per month * nmonths
psitotalvettechcost <- nvettech * techsalary

ncoords <- 1 #number of project coordinators
psiprojectcoordsalary <- (4080)  #Project Coord salary per year
psitotalprojcoorcost <-  ncoords * psiprojectcoordsalary

psisurvpersonnel <-  psitotalvettechcost + psitotalprojcoorcost 
psisurvoperational <- psitotalvehcilecost + psiofficerental   

psitotalsurveilanceSetUp <- psisurvoperational + psisurvcapital + psisurvpersonnel #total cost of setting up surveillance  
psitotalsurveilanceOnGoing <- psisurvoperational + psisurvpersonnel  #total cost of maintaining surveillance 

                                      ###########################################################
                                                         ##TRAINING / year (USD)
##TOTAL COSTS FOR TRAINING PERSONNEL AT START UP 
#need to discount psipersonelduringprogramme for proportion of this 

##PERSONEL
fullnumberofpersonel <- 15 #number of personel in a single brigade - SETTING UP
reducednumberofpersonel <- 7 #number of personel in a single brigade - ONGOING - ASSUME LOSS OF HALF THE TEAM LEAVING 50% TO TRAIN

psipersonelduringprogramme <- 35 #The cost per day per participant is based on the salary/wage and fringe benefits of participants in the workshop. 

#**assume that we retain a percentage of personelle each year therefore only need to retrain a proportion of original staff
#therefore need settup to have 1 year of full cost of whole team then every year after to have reduced cost
#whereas ongoing has reduced cotst only from year 1 becuase its already paid out its full team cost historiclly 
psibrigadepersonelSUfull <- 2*(fullnumberofpersonel * psipersonelduringprogramme) #assumed two days of training
psibrigadepersonelSUreduced <- 2*(reducednumberofpersonel * psipersonelduringprogramme)  

numberofteachers <- 1
teachersalary <- 246
psiteacher <- numberofteachers * teachersalary

psitotaltrainingpersonelSU <- psibrigadepersonelSUfull + psiteacher 
psitotaltrainingpersonelOG <- psibrigadepersonelSUreduced + psiteacher 

## **OPERATIONAL TRAINING OF PERSONNEL
numberofclassdays <- 1
psiclassroomdays <- 104 * numberofclassdays # cost per class room * number class rooms needed
numberfielddays <- 1 # number of field days
psifielddays <- 104 * numberfielddays #cost of field days

psioperationaltraining <- psiclassroomdays + psifielddays # includes classroom and field days, training related transport, space rental

psitotaltrainingSetUp <- psioperationaltraining + psitotaltrainingpersonelSU
psitotaltrainingOnGoing <- psioperationaltraining + psitotaltrainingpersonelOG 