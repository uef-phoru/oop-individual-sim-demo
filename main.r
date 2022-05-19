library(R6) #Load the library R6

source("individual.r") #Load the Individual class

demoIndividual <- Individual$new(individualAssets = list('ncycles' = 30, 'cohortparameter' = 1)) #Create a demoIndividual object based on the Individual class

#Create a set of 2 cohorts
cnum = 0 #This is the index for the cohorts while loop
clist = list() #The list where our individuals are saved
cohortparameters <- list() #The list for cohort parameters

while(cnum < 2) {
  
  #Create a cohort of 10 individuals
  indnum = 0 #This is the index for the individuals while loop
  indlist = list() #The list where our individuals are saved
  cohortparameter = rbeta(1,2,3) #Random cohort parameter for the cohort
  cohortparameters = append(cohortparameters, list(cohortparameter))
  
  while(indnum < 10) {
    indlist = append(indlist, Individual$new(individualAssets = list('ncycles' = 30, 'cohortparameter' = cohortparameter))) #An individual is created and added to indlist
    indnum = indnum + 1 #Increase the index by one
  }
  
  clist = append(clist, list(indlist))
  cnum = cnum + 1
    
}

