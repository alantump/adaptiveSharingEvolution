#Functions for the Simulations and Evolutionary algorithm 
#Codel for simualtions is adapted from:
#Bouhlel, I., Wu, C.M., Hanaki, N., Goldstone, R. (2018). Sharing is not erring: How environments can encourage psuedo-reciprocal sharing in human collective search. In T.T. Rogers, M. Rau, X. Zhu, & C. W. Kalish (Eds.), Proceedings of the 40th Annual Conference of the Cognitive Science Society (pp. 156-161).

#############################################################################################################################################################################################
#Basic functions for simulations 
#############################################################################################################################################################################################



#calculate score using inverse city-block distance between two vectors
score <-function(x,best){
  difference <- abs(x - best)
  return( (1+1)/(sum(difference) + 1)) #score is inverse distance with laplacian smoothing
}

# mutate a guess by adding or subtracting one from each value, or leaving as it is
mutate <-function(guesses, changeProbability = 0.5){
  changeVec <- rbinom(nrow(guesses), 1, changeProbability) #1 if change, 0 if no change. Drawn from a binomial distribution to see if mutation occurs or not for each agent
  #draw from a binomial distribution with possible values c(-1,0,1) to determine amount of change
  changeAmount <- matrix(rbinom(length(guesses),2,0.5) - 1, nrow = nrow(guesses), ncol= ncol(guesses)) #I dont like how this has been done (to often 0)
  change <- changeAmount*changeVec #multiple changeAmount by changeVec. This way, values of 0 in change vec result in no change
  guesses <- guesses + change
  return (guesses)
}

#apply rewards to a matrix of agent guesses, where each row is a different agent's guess and ideal is the global maximum
# acquireRewards <- function(agents, ideal){
#   rewards <- sapply(1:nrow(agents), FUN=function(x) score(agents[x,], ideal)) #apply reward function
#   #check for duplicate solutions. Returns a vector corresponding to the total number of agents with the same guess
#   duplicates <- sapply(1:nrow(agents), FUN=function(i) sum(apply(agents, 1, function(j) identical(j, agents[i,]))))
#   return(rewards/duplicates) #split rewards according to the number of agents with the identical guess
# }

#apply rewards to a matrix of agent guesses, where each row is a different agent's guess and ideal is the global maximum
#splittingSensitivity is the inverse of the competition parameter reported in the paper
acquireRewards <- function(agents, ideal, splittingSensitivity){
  rewards <- sapply(1:nrow(agents), FUN=function(x) score(agents[x,], ideal)) #apply reward function
  #Calculate similarity between player solutions
  similarities <- sapply(1:nrow(agents), FUN=function(i) sum(apply(agents, 1, function(j) exp(-splittingSensitivity * dist(rbind(j, agents[i,]), method = 'manhattan')))))
  return(rewards/similarities) #divide reward by similarity to account for splitting
}

#update observations
updateObservations <-function(guesses, rewards, observations, t){
  for (i in 1:nrow(guesses)){ #loop through agents
    observations[[i]] <- rbind(observations[[i]], c(guesses[i,], rewards[i], t,i)) #add c(guess, score) to the observations of the agent
  }
  return(observations)
}


#local visabilities
localVisibility <- function(agents, freeLocalInfoRadius){
  distances <- as.matrix(dist(agents, method='maximum', upper = T, diag=T)) #calculate all pairwise Chebyshev distances between agents
  visible <- distances <= freeLocalInfoRadius #convert to boolean based on whether the distance is less than or equal to the free local info radius
  return(visible)
}



#acquire free information, and add to observations
freeInfo <- function(visibilities, guesses, rewards, observations, t){
  for (i in 1:nrow(guesses)){ #loop through agents i
    for (j in 1:nrow(guesses)){ #loop through others j
      if (i!=j & visibilities[i,j]){ #not same agent and j is visible to i
        observations[[i]] <- rbind(observations[[i]], c(guesses[j,], rewards[j], t,j)) #add information from j to the observation's of i
      }}}
  return(observations)
}

#Share information, where shareVec is a binary vector indicating whether not the agent shares information
shareInformation <- function(shareVec, agents, scores, observations, t){
  for (i in 1:nrow(agents)){ #for agent i
    for (j in 1:nrow(agents)){ #for agent j
      if (i!=j & shareVec[i]){ # if agent i ≠ j and agent i is a sharer
        observations[[j]] <- rbind(observations[[j]], c(agents[i,], scores[i], t,i)) #add information from i to j
        }
    }
  }
  return(observations)
}

#finds index of max, resolving ties randomly
which.max.random <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  which(rank(x, ties.method = "random", na.last = FALSE) == length(x))
}
#standard error
se<-function(x){return(sd(x)/sqrt(length(x)))}

which.max.random <- nnet::which.is.max

#change the idealValues at each time step; used for dynamic environments
changeIdealValues<- function(idealValues, changeRate){
  if (rbinom(1,1,changeRate)==1){ #if successful, change global optimum
    changeAmount <- rbinom(length(idealValues),2,0.5) - 1
    idealValues <- idealValues + changeAmount
    return (idealValues)
  }else{ #otherwise, stay the same
    return (idealValues)
  }
}





#############################################################################################################################################################################################
#Simulation function
#############################################################################################################################################################################################
#Function to run a single replication using a single set of environmental parameters
runCondition<-function(numAgents, numDimensions, changeProbability, freeLocalInfoRadius, shareVec, ploting, changeRate, decayRate,splittingSensitivity, turns=100){ 
  reciproce_matrix<-matrix(0,numAgents,numAgents)
  get<-rep(0,numAgents)
  sum_scores <-rep(0,numAgents)
  
  #1. Intialize main data structures
  #array of agent guesses. Each row of matrix is an agent, each column is a dimension
  agents<-matrix(sample(1:numValues, numAgents*numDimensions,replace=TRUE),nrow=numAgents,ncol=numDimensions) #TODO: Store history of guesses to track evolution of search over time
  #observations: individual observations + free local visibilities + information shared by other agents
  observations <- lapply(1:numAgents, matrix, data= NA, ncol=numDimensions + 3, nrow=0) #each matrix is the total data available to each agent (guess for each dimension, plus the corresponding score, plus the round)
  #scores of each agent

  #2. Initialize environment and assign rewards based on initial guesses
  idealValues<-sample(1:numValues,numDimensions,replace=TRUE) #generate global maximum
  scores <- acquireRewards(agents, idealValues, splittingSensitivity) #assign rewards based on intial starting values
  observations <- updateObservations(agents, scores, observations, 1)#update observations with initial guesses
  #Add to scoreHistory
  if(ploting==1){  
    #add to scoreHistory
    scoreHistory <- data.frame(score = scores, round=rep(1, numAgents), agent=seq(1:numAgents), sharer=shareVec,num_visible=apply(localVisibility(agents, freeLocalInfoRadius),2,sum)-1,reci=rep(0,numAgents),get=rep(0,numAgents))
  }
    sum_scores =  sum_scores +scores
  
  distance_history<-matrix(NA,turns,numAgents)
  score_history<-matrix(NA,turns,numAgents)
  if(ploting==1){    distance_history[1,]<-  apply(abs(apply(agents,1,'-',idealValues)),2,sum) 
  score_history[1,]<-scores
  }
  

  #3. Start looping through iterations
  for (t in 2:turns){ #for each turn
    #acquire free local visibilities
    visibilities <- localVisibility(agents, freeLocalInfoRadius) #compute visibilities
    observations <- freeInfo(visibilities = visibilities, agents, scores, observations, t) #add free local information
    #Individual search            
    #Apply (reverse?) temporal discounting to observations  
    bestObservations <- t(sapply(1:numAgents, FUN=function(i) {
      decayed <- observations[[i]]
      decayed[,numDimensions+1] <- decayRate ** (t - observations[[i]][,numDimensions+2]) *  observations[[i]][,numDimensions+1]  #reward <-  decayRate^(timeDifferences) * reward
      best <- decayed[which.max.random(decayed[,numDimensions+1]), ,drop=FALSE]
    }))
    #Mutate near best observed reward
    agents <- mutate(bestObservations[,1:numDimensions], changeProbability = changeProbability) 
    #change the idealValues
    idealValues <- changeIdealValues(idealValues, changeRate)
    scores <- acquireRewards(agents, idealValues, splittingSensitivity) #assign rewards based on intial starting values
    observations <- updateObservations(agents, scores, observations, t)#update observations with individual mutations
    #Share information
    observations <- shareInformation(shareVec, agents, scores, observations, t) #add sharing information to observations 
     sum_scores <-  sum_scores +scores
    
     if(ploting==1){  
      reciproce_matrix_old<-reciproce_matrix
      for (i in 1:numAgents){
        obs<-observations[[i]]
        max_own_rew<-max(obs[obs[,numDimensions + 3]==i | obs[,numDimensions + 2]<t,numDimensions + 1])
        other_source<-obs[obs[,numDimensions + 3]!=i,numDimensions + 3]
        other_better_source<- other_source[ (obs[obs[,numDimensions + 3]!=i,numDimensions + 1])>max_own_rew]
      reciproce_matrix[i,other_better_source]<-reciproce_matrix[i,other_better_source]+1
      }
      
      reci<-rep(NA,numAgents)
      for (i in 1:numAgents){
        reci[i]<-mean(apply(rbind( reciproce_matrix[c(1:numAgents)!=i,i], reciproce_matrix[i,c(1:numAgents)!=i]),2,min)) #average how often I shared and got something back
        get[i]<-sum((reciproce_matrix-reciproce_matrix_old)[i,])
        }
      
      #add to scoreHistory
      dataEntry <- data.frame(score = scores, round=rep(t, numAgents), agent=seq(1:numAgents), sharer=shareVec,num_visible=apply(localVisibility(agents, freeLocalInfoRadius),2,sum)-1,reci=reci ,get=get)
      scoreHistory <- rbind(scoreHistory, dataEntry)
    }
    
  }
  
  #return data
  if(ploting==1){return(scoreHistory)
  }else{
    return(sum_scores)}
  
}

          
#############################################################################################################################################################################################
## The Evolutionary algorithm 
#############################################################################################################################################################################################
runEA<-function(numAgents,pop_size,number_of_generations,n_eval,changeRate,decayRate,type){
  result1=result2=NULL #initialize the output variables; TODO: initialize with pre-allocated size for speedup
  
  #Find initial sharing condition. If type=evolve, the initial population consists of non-sharers.
  #Otherwise the average proportion of sharers is a random number between 10%-90%.
  if(type=="evolve"){
    start_mean_sharing<-0
  } else{
    start_mean_sharing<-runif(1,0.1,0.9)
    
  }
  #The initial average innovation rate of is a random number between 0.1-0.9.
  start_mean_innovation<-runif(1,0.1,0.9)
  
  
  #Allocate genes with a given average:
  #Sharing gene
  a<-1  
  b<-(a/start_mean_sharing)-a
  sharing_chromosom<-rbinom(pop_size,1,start_mean_sharing)
  #Innovation gene
  a<-1
  b<-(a/start_mean_innovation)-a
  innovation_chromosom<-rbeta(pop_size,a,b)

  #Save results of the evolutionary algorithm
  result1<-rbind(result1,c(mean(sharing_chromosom)))
  result2<-rbind(result2,c(mean(innovation_chromosom)))
  
  #Loop through ach generatiom
  for(gen in 1:(number_of_generations-1)){
    
    #if innovation should be fixed:
    if (fixed_inno==1){
          innovation_chromosom = rep(innovation,pop_size)
    }
    
    probs<-rep(1,pop_size) #probability of sampling indiviudaks
    fitness<-rep(0,pop_size) #vector containg fitness results
    #repeat simulation n_eval times:
    for (evals in 1:n_eval){
      
      index=sample(1:pop_size,numAgents,prob=probs)#sample indivduals
      
      #each time a individual is picked the probaibity that it is picked again is a fifth of before 
      #because the probabilities a relative it should assure a relative uniform number of repetitions for each individual.
      #Note that the some randomness helps the EA to maintain some variance in the genpool.
      probs[index]<-probs[index]/5 
      ploting<-0 #No plotts during the evolutionary algorithm 
      
      #run simulation
      performance<-runCondition(numAgents, numDimensions, innovation_chromosom[index], freeLocalInfoRadius, sharing_chromosom[index], ploting, changeRate, decayRate,splittingSensitivity, turns=50) 
      
      fitness[index]<-  fitness[index]+performance #save performance (fitness)
    }

    selection_type<-0 #We decided for tournament selection (not very influencial)
    if (selection_type==1){
      ## Roulette selection
      winners1<-sample(pop_size,pop_size,replace=T,prob=rescale(fitness))
      ##
    }else{
      #k-turnement selection 
      k<-7
      winners1<-winners2<-rep(NA,pop_size)
      for (turns in 1:pop_size){
        indexes<-sample(1:pop_size,k) #sample tournament members
        winners1[turns]<-indexes[which.max.random(fitness[indexes])] #Who winns?
          
      }}
    sharing_chromosom<-(sharing_chromosom[winners1]) #Winners reproduce
    innovation_chromosom<-(innovation_chromosom[winners1])
  
    ###mutation
    mutation_rate<-0.002
    dummy_index<-runif(pop_size)<mutation_rate
    sharing_chromosom[dummy_index]<-rbinom(sum(dummy_index),1,0.5)

    mutation_rate<-0.02
    dummy_index<-runif(pop_size)<mutation_rate
    innovation_chromosom[dummy_index]<-  innovation_chromosom[dummy_index]+rnorm(sum(dummy_index),0,0.2)
    innovation_chromosom <- ifelse(innovation_chromosom>1,0.99,innovation_chromosom)
    innovation_chromosom <- ifelse(innovation_chromosom<0,0.01,innovation_chromosom)
    
    #Save result
    result1<-rbind(result1,c(mean(sharing_chromosom)))
    result2<-rbind(result2,c(mean(innovation_chromosom)))
    
  }
  
  cbind(result1,result2)
}

