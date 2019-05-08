#Plots for share evolution

rm(list=ls())

#load packages
library(brms)
library(ggplot2)
library(viridis)
library(tidyr)
library(dplyr)
library(cowplot)


#######
#Sumarise results in data frames:
#######
where="Sim_results/"

#Static environment with evolving sharing and innovation
name="static_sharing_and_inno"
load(paste0(where,name,"/",1,name,".RData"))
final_data_static<-final_data

for (loop_parameters in 1:nrow(final_data)){
  load(paste0(where,name,"/",loop_parameters,name,".RData"))
  final_data_static[loop_parameters,]<-final_data[loop_parameters,]
}


#Static environment with evolving sharing and fixed innovation
name="static_sharing_only"
load(paste0(where,name,"/",1,name,".RData"))
final_data_fixed_inno<-final_data
for (loop_parameters in 1:nrow(final_data)){
  load(paste0(where,name,"/",loop_parameters,name,".RData"))
  final_data_fixed_inno[loop_parameters,]<-final_data[loop_parameters,]
}

#Dynamic environment 
name="dynamic_sharing_and_inno"
load(paste0(where,name,"/",1,name,".RData"))
final_data_dynamic<-final_data
for (loop_parameters in 1:nrow(final_data)){
  load(paste0(where,name,"/",loop_parameters,name,".RData"))
  final_data_dynamic[loop_parameters,]<-final_data[loop_parameters,]
}



##Examples of Evolution
name="static_sharing_and_inno"#fixed_inno"
example1=example2=NULL

#Find example where sharing evolved
index_evolved <- sample(c(1:nrow(final_data_static))[final_data_static$sharing>0.9], size = 1)
load(paste0(where,name,"/",index_evolved,name,".RData"))
example1[[1]]<-result[[1]]
example1[[2]]<-result[[2]]

#Find example where sharing didn't evolved 
index_not_evolved <- sample(c(1:nrow(final_data_static))[final_data_static$sharing<0.05], size = 1)
load(paste0(where,name,"/",index_not_evolved,name,".RData"))
example2[[1]]<-result[[1]]
example2[[2]]<-result[[2]]


###Make example plots:
pops <- data.frame(example1[[1]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","Sharing",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[1]],1,mean)) %>% dplyr::mutate(x=1:length(y))

p_share1 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=Sharing,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none")+ ylim(0,1)
p_share1

pops <- data.frame(example1[[2]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","inno",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[2]],1,mean)) %>% dplyr::mutate(x=1:length(y))

p_inno1<-ggplot() +
  geom_line(data = pops,aes(x=Generations,y=inno,color=pop,group=pop), size = 0.5,alpha = 0.7)+
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none") + ylab("Innovation") + ylim(0,1)
p_inno1

#second example
pops = data.frame(example2[[1]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","Sharing",X1:paste0("X",reps))   
means = data.frame(y=apply(example2[[1]],1,mean)) %>% dplyr::mutate(x=1:length(y))
p_share2=ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=Sharing,color=pop,group=pop),  size = 0.5,alpha = 0.7)+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none")+ ylim(0,1)
p_share2

pops = data.frame(example2[[2]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","inno",X1:paste0("X",reps))   
means = data.frame(y=apply(example2[[2]],1,mean)) %>% dplyr::mutate(x=1:length(y))
p_inno2=ggplot() +
  geom_line(data = pops,aes(x=Generations,y=inno,color=pop,group=pop),  size = 0.5,alpha = 0.7)+
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none") + ylab("Innovation")+ ylim(0,1)
p_inno2
examples<-plot_grid(p_share1,p_inno1,p_share2,p_inno2,align="hv",labels = "auto")
examples

ggsave("plots/evolution_example.pdf",examples, width = 6,height = 4,dpi = 600)




##### Result 2 sharing evloves in static environments
plot_data = final_data_static %>%  filter(changeRate==0) %>%  
  dplyr::mutate(splittingSensitivity = factor(ifelse(splittingSensitivity==0.5,"High (c=2)",ifelse(splittingSensitivity==1,"Medium (c=1)","Low (c=1/128)"))),
                splittingSensitivity = factor(splittingSensitivity,levels(splittingSensitivity)[c(2,3,1)]),
                Sharing=sharing) 


p1 = plot_data %>%
  ggplot()+  geom_tile(aes(freeLocalInfoRadius,numAgents,fill=Sharing))+
  facet_grid(~splittingSensitivity)+scale_fill_viridis(limits=c(0, 1)) +
  labs(subtitle = "Competition")+
  theme_classic(base_size = 14)+
  xlab("Visibility") +ylab("Group Size")+ theme(strip.background = element_blank(),
                                                strip.text.y = element_blank())
p1

p2 = plot_data %>%
  ggplot()+  geom_tile(aes(freeLocalInfoRadius,numAgents,fill=Innovation)) +
  facet_grid(~splittingSensitivity)+scale_fill_viridis(limits=c(0, 1)) +
  labs(subtitle = "Competition")+
  theme_classic(base_size = 14)+
  xlab("Visibility")+ ylab("Group size")+ theme(strip.background = element_blank(),
                                                strip.text.y = element_blank())
p2
static<-plot_grid(p1,p2,ncol=1,labels = "auto")
static

ggsave("plots/static.pdf",static, width = 7, height = 5,units = 'in')



##### Result 2 sharing needs inovation
plot_data = final_data_fixed_inno %>%  filter(changeRate==0,innovation%in%c(0.25,0.5,1)) %>%  
  dplyr::mutate(splittingSensitivity = factor(ifelse(splittingSensitivity==0.5,"High (c=2)",ifelse(splittingSensitivity==1,"Medium (c=1)","Low (c=1/128)"))),
                splittingSensitivity = factor(splittingSensitivity,levels(splittingSensitivity)[c(2,3,1)]),
                Sharing=sharing,
                innovation = factor(ifelse(innovation==1,"High Inno",ifelse(innovation==0.5,"Medium Inno","Low Inno"))),
                innovation = factor(innovation,levels(innovation)[c(1,3,2)])) 
p1 = plot_data %>%
  ggplot()+  geom_tile(aes(freeLocalInfoRadius,numAgents,fill=Sharing))+
  facet_grid(innovation~splittingSensitivity)+scale_fill_viridis(limits=c(0, 1)) +
  labs(subtitle = "Competition")+
  theme_classic(base_size = 14)+
  xlab("Visibility") +ylab("Group size")+ theme(strip.background = element_blank())
p1

ggsave("plots/fixed_inno.pdf",p1, width = 6,height = 4,units='in')



#### Dynamic environments
plot_data = final_data_dynamic %>%  
  dplyr::mutate(splittingSensitivity = factor(ifelse(splittingSensitivity==0.5,"High (c=2)",ifelse(splittingSensitivity==1,"Medium (c=1)","Low (c=1/128)"))),
                splittingSensitivity = factor(splittingSensitivity,levels(splittingSensitivity)[c(2,3,1)]),
                changeRate = factor(changeRate, levels=c("0.6","0.3", "0")),
                Sharing=sharing,
                numAgents = factor(numAgents))



p1 = plot_data %>%
  ggplot()+  geom_tile(aes(freeLocalInfoRadius,numAgents,fill=Sharing))+
  facet_grid(changeRate~splittingSensitivity)+scale_fill_viridis(limits=c(0, 1)) +
  labs(subtitle = "Competition")+
  xlab("Visibility") +ylab("Group Size")+ theme(strip.background = element_blank())

p2 = plot_data %>%
  ggplot()+  geom_tile(aes(freeLocalInfoRadius,numAgents,fill=Innovation)) +
  facet_grid(changeRate~splittingSensitivity)+scale_fill_viridis(limits=c(0, 1)) +
  labs(subtitle = "Competition")+
  xlab("Visibility")+ ylab("Group Size")+ theme(strip.background = element_blank())

dynamic<-plot_grid(p1,p2,ncol=1,labels = "auto")
dynamic

ggsave("plots/dynamic.pdf",dynamic,  width = 7,height = 7,units='in')





## Regression results

#scale data
final_data_scaled=final_data_dynamic %>% dplyr::mutate( freeLocalInfoRadius=scale(freeLocalInfoRadius),
                                                        splittingSensitivity=scale(-splittingSensitivity),
                                                        numAgents=scale(as.numeric(as.character(numAgents))),
                                                        changeRate=scale(as.numeric(as.character(changeRate))))

#run models for sharing and innovation
model1<-brm(sharing~freeLocalInfoRadius+splittingSensitivity+numAgents+changeRate,data=final_data_scaled, save_all_pars = T)
model2<-brm(Innovation~freeLocalInfoRadius+splittingSensitivity+numAgents+changeRate,data=final_data_scaled, save_all_pars = T)


#Rearrange results
dummy1<-data.frame(t(fixef(model1)))
dummy<-data.frame((fixef(model1)[2:5,]))
dummy$names<-names(dummy1)[2:5]


dummy2<-data.frame(t(fixef(model2)))
dummy3<-data.frame((fixef(model2)[2:5,]))
dummy3$names<-names(dummy2)[2:5]
paramter_plot<- rbind(dummy,dummy3)

paramter_plot$parameter<-c(rep("Sharing",length(dummy3$names)),rep("Innovation",length(dummy3$names)))

paramter_plot = paramter_plot %>% dplyr::mutate(names=ifelse(names=="freeLocalInfoRadius","Visibility",names),
                                                names=ifelse(names=="numAgents","Group size",names),
                                                names=ifelse(names=="splittingSensitivity","Competition",names),
                                                names=ifelse(names=="changeRate","Environmental change",names))

#make figure
p1 = ggplot(data=paramter_plot,aes(y=Estimate,x=names,type=parameter,color=parameter, shape = parameter)) + 
  geom_hline(yintercept=0,linetype="dashed", color="gray")+
  geom_point(position=position_dodge(0.25), size = 2)+
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5,color=parameter), width=0,position=position_dodge(0.25))+ 
  coord_flip() + 
  theme_classic(base_size = 14)+
  xlab("") + 
  ylab("Effect size") +
  
  scale_shape_discrete( name=  '')+
  scale_color_manual(values = c( "#1b9e77", "#7570b3"), name=  '')+
  theme(legend.position = c(0.15, 0.9),legend.background = element_rect(fill="white"))
p1


ggsave("plots/parameters.pdf",p1, width = 7,height = 3,units= 'in')


#Run simulation comparing performance of sharer vs. non-sharer (time intensive)
source("Share_Evol_functions.R")

#Set parameters for example
numDimensions=10
numValues <- 10 # number of different values that a dimension could take
decayRate=1
freeLocalInfoRadius=2
numAgents=6
changeRate=0
splittingSensitivity = 1
ploting=1
iterations <- 10000
innovation=1 

#run each once
performance_giver <- runCondition(numAgents, numDimensions, rep(innovation,numAgents), freeLocalInfoRadius, c(rep(0,numAgents-1),1), ploting, changeRate, decayRate,splittingSensitivity, turns=50)
performance_egos <- runCondition(numAgents, numDimensions, rep(innovation,numAgents), freeLocalInfoRadius, rep(0,numAgents), ploting, changeRate, decayRate,splittingSensitivity, turns=50)

#run many times:
for (x in 2:iterations){ #start from 2 since we run the first iteration once
  #Increment the relevant columns
  performance_giver[c('score', 'num_visible','reci','get')] <- performance_giver[c('score', 'num_visible','reci','get')] + runCondition(numAgents, numDimensions, rep(innovation,numAgents), freeLocalInfoRadius, c(rep(0,numAgents-1),1), ploting, changeRate, decayRate,splittingSensitivity, turns=50)[c('score', 'num_visible','reci','get')]
  performance_egos[c('score', 'num_visible','reci','get')]<-performance_egos[c('score', 'num_visible','reci','get')] + runCondition(numAgents, numDimensions, rep(innovation,numAgents), freeLocalInfoRadius, rep(0,numAgents), ploting, changeRate, decayRate,splittingSensitivity, turns=50)[c('score', 'num_visible','reci','get')]
if (x/100==round(x/100))print(x)
}

#make averages per iteration 
performance_giver[c('score', 'num_visible','reci','get')] <- performance_giver[c('score', 'num_visible','reci','get')] / iterations
performance_egos[c('score', 'num_visible','reci','get')]<-performance_egos[c('score', 'num_visible','reci','get')] / iterations


performance_giver = performance_giver %>% filter(sharer==1) #choose the sharer
performance_egos <- performance_egos %>% filter(agent==1) #choose a random agent

#combine simulation results
plot_data=  rbind(performance_giver,performance_egos) %>%dplyr::mutate(sharer=ifelse(sharer==1 ,"Sharer","Non-sharer"))%>%
  dplyr::group_by(sharer,round)  


#Make figure
p1 = plot_data %>% 
  ggplot(aes(x=round,y=score,color=factor(sharer),group=factor(sharer))) + theme_classic()+
  scale_color_manual(values = c(  "#56B4E9","#E69F00"), name=  '')+geom_line(size=1.2) + xlab("Trials") + ylab("Score")+
  scale_y_continuous(breaks=seq(0,0.3,0.1),limits = c(0,0.3))+ theme(legend.title=element_blank())+
  theme(legend.position = "none")

p2 = plot_data %>%
  ggplot(aes(x=round,y=num_visible,color=factor(sharer),group=factor(sharer)))+ theme_classic()  +
  scale_color_manual(values = c(  "#56B4E9","#E69F00"), name=  '')+ geom_line(size=1.2) +
  xlab("Trials")+ylab( "Visible Peers")+
  theme(legend.position = c(0.4, 0.9),legend.background = element_rect(fill="white"))+ ylim(0,numAgents)


p3<-plot_data%>%ggplot()+geom_line(aes(x=round,y=get,color=sharer),size=1.2)+ theme_classic()+
  xlab("Trials")+scale_color_manual(values = c(  "#56B4E9","#E69F00"), name=  '')+
  ylab( "Probability of Receiving\nSuperior Information from Peer") +  theme(legend.position = "none")
 
simulation_insights <- plot_grid(p2,p3,p1,ncol=3,labels = "auto", align = 'v')


ggsave("plots/cooperation_example.pdf",simulation_insights, width = 8.5*0.85,height = 3*0.9,units = 'in')

