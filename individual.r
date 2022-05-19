Individual <- R6Class("Individual",
                      public = list(
                        results = NULL, #Results to be extracted are saved to this class variable. Results are later referenced as self$results
                      
                        initialize = function(individualAssets) { #This is the constructor. It is run when the individual is created.
                          
                          
                          #Let's create the vectors for states, costs, and QALYs beforehand for convenience
                          #Since we want to extract them afterwards, we need to save them to results.
                          self$results$states <- rep("", individualAssets$ncycles)
                          self$results$costs <- rep(0, individualAssets$ncycles)
                          self$results$QALYs <- rep(0, individualAssets$ncycles)
                          
                          #During the first cycle our individual is alive
                          self$results$states[1] <- 'Life'
                          
                          #Assets can assigned to new variables for convenience:
                          cpar = individualAssets$cohortparameter
                          
                          for(cycle in 1:individualAssets$ncycles) { #Cycles start here, the individualAssets list must contain element ncycles, which is the number of cycles.
                            
                            
                            #Just some random costs from gamma distribution. If you are dead, you do not cause any costs. 
                            self$results$costs[cycle] <- rgamma(1, 200, 3) * (self$results$states[cycle] != 'Death')
                            
                            #Just some random utility from beta distribution. Zero QALYs for dead individuals.
                            self$results$QALYs[cycle] <- rbeta(1, 2, 3) * (self$results$states[cycle] != 'Death')
                            
                            #The model has three states: "Life", "Between life and death", "Death"
                            #...and there is no going back. Random noise from lognormal distribution is enough here. :)
                            #The cohort parameter influences the probabilities of staying in states 'Life' and 'Between life and death'
                            
                            transitionMatrix <- t(matrix(c(
                              # Life,Between  ,Death
                              c(rlnorm(1)*cpar,rlnorm(1),     rlnorm(1)), #Life
                              c(0,             rlnorm(1)*cpar,rlnorm(1)), #Between life and death
                              c(0,             0,             1)          #Death
                            ), nrow = 3))
                            
                            #Rows and columns can be named for easier access.
                            colnames(transitionMatrix) <- rownames(transitionMatrix) <- c('Life','Between','Death')
                            
                            #Let's update the probabilities again with random, if this was the first cycle in state 'Between life and death'
                            if(cycle > 1) { #This is essential since cycle 0 does not exist, try not to reference it!
                              if(self$results$states[cycle - 1] == 'Life') {
                                transitionMatrix[2,] <- rlnorm(3, 2, 1) * transitionMatrix[2,]
                              }
                            }
                            
                            #Not need to standardize the matrix probabilities between 0 and 1, but it can be done, e.g.
                            transitionMatrix <- t(apply(transitionMatrix,1, function(row) row/sum(row)))
                            
                            
                            
                            #And then the state for the next cycle is sampled. N.B.! If this is the final cycle, the states vector has no index cycle + 1!
                            if(cycle != individualAssets$ncycles) {
                              self$results$states[cycle + 1] <- sample(colnames(transitionMatrix), size = 1, prob=transitionMatrix[self$results$states[cycle],])
                            }
                          }
                        }
                      )
)