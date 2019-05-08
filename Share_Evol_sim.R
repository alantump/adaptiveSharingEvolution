
#Script to run  the Evolutionary Algorithm to analyze the adaptiveness of sharing and  save the results

#load packages
rm(list=ls())

packages <- c('data.table', 'plyr',"ggplot2","foreach","parallel","doParallel")
lapply(packages, require, character.only=TRUE)
source("Share_Evol_functions.R")

##Set analysis type
analysis_type<-"static_sharing_and_inno" #(total simulations = 60)
#analysis_type<-"static_sharing_only"  #(total simulations = 180)
#analysis_type<-"dynamic_sharing_and_inno" #(total simulations = 180)



#########
#Set some paramters depending on analysis type:
#########
if (analysis_type=="static_sharing_and_inno"){
paramter_package<- list(decayRate=1)
paramter_package$fixed_inno <-0
paramter_package$changeRate_vec<-0
paramter_package$innovation_vec<-NA
}
###########
if (analysis_type=="static_sharing_only"){
paramter_package<- list(decayRate=1)
paramter_package$fixed_inno <-1
paramter_package$changeRate_vec<-0
paramter_package$innovation_vec<-c(0.25,0.5,1)
}
#############
if (analysis_type=="dynamic_sharing_and_inno"){
paramter_package<- list(decayRate=0.99)
paramter_package$fixed_inno <-0
paramter_package$changeRate_vec<-c(0,0.3,0.6)
paramter_package$innovation_vec<-NA
}
#############

# Command to destribute parallel processes on a server. Each clusterid analysis a certain paramter combination
clusterid <- as.integer(commandArgs(TRUE)[1])
if(is.na(clusterid)){clusterid<-1}


numDimensions<-10 #number of dimensions
numValues <- 10 # number of different values that a dimension could take
decayRate<-paramter_package$decayRate


pop_size<-300 #population size
number_of_generations<-200 #number of generations
how_often<-5 #how often should each individual be evaluated per generation

reps<-10 #number of populations
type<-"evolve" # Sets the initial sharing proportion of the popultion ("evolve"=0; "uniform"=10%-90%) 
fixed_inno<-paramter_package$fixed_inno # should innovation be fixed (1) or evolve (0) 

#Simulation parameters
freeLocalInfoRadius_vec<-c(0:4) #Visibility radius
numAgents_vec<-seq(2,14,4) #Group size
changeRate_vec<-paramter_package$changeRate_vec #speed of environmental change 
splittingSensitivity_vec <- c(0.5,1,128) #inverse of competition
innovation_vec <- paramter_package$innovation_vec #the likelihood to innovate on current solution
final_data<-expand.grid(numAgents=numAgents_vec,changeRate=changeRate_vec,freeLocalInfoRadius=freeLocalInfoRadius_vec,splittingSensitivity=splittingSensitivity_vec,innovation=innovation_vec,sharing=NA,sharing_sd=NA,Innovation=NA,Innovation_sd=NA)
nrow(final_data)

#Set analyzed paramters depending on clusterid
splittingSensitivity <- final_data$splittingSensitivity[clusterid]
   numAgents <- final_data$numAgents[clusterid]
  changeRate <- final_data$changeRate[clusterid]
  freeLocalInfoRadius <- final_data$freeLocalInfoRadius[clusterid]
  n_eval<-ceiling(pop_size/numAgents*how_often) #for gif =25
  innovation <- final_data$innovation[clusterid]
  
  
  cl<-makeCluster(reps) #to run all 10 populations in parallel 
  registerDoParallel(cl)
  clusterExport(cl, ls()) #export workspace into cluster
  xx<-foreach(r=1:reps) %dopar% { #each population in parralel
    runEA(numAgents,pop_size,number_of_generations,n_eval,changeRate,decayRate,type)
  }
  stopCluster(cl)#stop cluster
  
  #rearrange results
  result<-NULL
  for(ii in 1:2){
    result[[ii]]<-matrix(NA,number_of_generations,reps)
    for (i in 1:reps){
      result[[ii]][,i]<-t(xx[[i]][,ii])
    }
  }
  
  #store evolved population averages in final_data 
  final_data$sharing[clusterid] <- mean(apply(result[[1]],1,mean)[(number_of_generations-10):number_of_generations])
  final_data$sharing_sd[clusterid]<-mean(apply(result[[1]],1,sd)[(number_of_generations-10):number_of_generations])
  final_data$Innovation[clusterid]<-mean(apply(result[[2]],1,mean)[(number_of_generations-10):number_of_generations])
  final_data$Innovation_sd[clusterid]<-mean(apply(result[[2]],1,sd)[(number_of_generations-10):number_of_generations])
  

  
final_data$changeRate<-as.factor(final_data$changeRate)
final_data$numAgents<-as.factor(final_data$numAgents)




#Save results
save.image(paste0(analysis_type,"/",clusterid,analysis_type,".RData"))




