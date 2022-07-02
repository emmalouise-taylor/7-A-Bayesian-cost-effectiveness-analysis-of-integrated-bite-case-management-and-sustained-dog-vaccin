

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

psitotaldiagnosisSetUp <- psidiagcapital + psidiagoperational + psidiagpersonel 
psitotaldiagnosisOnGoing <- psidiagoperational + psidiagpersonel 

                          ###########################################################
                                                      ##SURVEILLANCE/ Year
#CAPITAL 
psisurvcapital <- 1572 

#OPERATIONAL 
nvehicle <- 2 
psivehiclegas <- 22 * nvehicle 
psivehiclecost <- 88 + psivehiclegas 
psitotalvehcilecost <- psivehiclecost * nvehicle 
utilities <- 1000
psiofficerental <- 4000 + utilities 

#PERSONEL  
nvettech <- 3
techsalary <- 9000  
psitotalvettechcost <- nvettech * techsalary

ncoords <- 1 
psiprojectcoordsalary <- (4080)  
psitotalprojcoorcost <-  ncoords * psiprojectcoordsalary

psisurvpersonnel <-  psitotalvettechcost + psitotalprojcoorcost 
psisurvoperational <- psitotalvehcilecost + psiofficerental   

psitotalsurveilanceSetUp <- psisurvoperational + psisurvcapital + psisurvpersonnel 
psitotalsurveilanceOnGoing <- psisurvoperational + psisurvpersonnel  

                                      ###########################################################
                                                         ##TRAINING / year (USD)


##PERSONEL
fullnumberofpersonel <- 15
reducednumberofpersonel <- 7 

psipersonelduringprogramme <- 35 
psibrigadepersonelSUfull <- 2*(fullnumberofpersonel * psipersonelduringprogramme) 
psibrigadepersonelSUreduced <- 2*(reducednumberofpersonel * psipersonelduringprogramme)  

numberofteachers <- 1
teachersalary <- 246
psiteacher <- numberofteachers * teachersalary

psitotaltrainingpersonelSU <- psibrigadepersonelSUfull + psiteacher 
psitotaltrainingpersonelOG <- psibrigadepersonelSUreduced + psiteacher 

## **OPERATIONAL TRAINING OF PERSONNEL
numberofclassdays <- 1
psiclassroomdays <- 104 * numberofclassdays 
numberfielddays <- 1
psifielddays <- 104 * numberfielddays 

psioperationaltraining <- psiclassroomdays + psifielddays 

psitotaltrainingSetUp <- psioperationaltraining + psitotaltrainingpersonelSU
psitotaltrainingOnGoing <- psioperationaltraining + psitotaltrainingpersonelOG 
