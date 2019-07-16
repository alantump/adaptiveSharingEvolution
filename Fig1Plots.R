#Competition paramerter plot
#Charley Wu
library(ggplot2)
library(viridis)

#Splitting function
competitionSplit <- function(c, d){
  return(exp(-d/c))
}

#run simulations at different values
df <- data.frame()
comps <-c(1/128, 1 ,2)
s <- seq(0,8, by=0.1) #Distances

for (comp in comps){
  dummy <- data.frame(c = comp, dist = s, sim = competitionSplit(comp,s))
  df <- rbind(df, dummy)
}


df$c <- factor(df$c)
levels(df$c) <- c('1/128','1', '2')


p1 <- ggplot(df, aes(x = dist, y = sim, color = c, linetype = c))+
  geom_line(size = 1)+
  scale_color_brewer(palette = 'Set1')+
  theme_classic(base_size = 18)+
  ylab('Competition')+
  xlab('Distance')+
  scale_x_continuous(breaks=c(0,2,4,6,8))+
  scale_y_continuous(breaks=c(0,0.5,1))+
  theme(legend.position = c(0.7,0.7))
p1

ggsave(filename = 'plots/splittingRate.pdf', p1, width = 5, height = 5, unit = 'in')


####################
#Examples of beta distributions
#####################

x <- seq(0,1,length=100) #define grid

nugget <- 1 #aka psuedo-count for seeding beta distributions

#betaValues <-(1 / c(0.1, 0.25, .5,0.75, 0.9)) - 1
b1 <- dbeta(x, 0.5,0.5) 
b2 <- dbeta(x, 5,1)
b3 <- dbeta(x, 1,3)
b4 <- dbeta(x, 2,2)
b5 <- dbeta(x, 2,5)


betas <- data.frame(x=rep(x, 5), y=c(b1,b2,b3,b4, b5), value=rep(seq(1,5),each=100))
betas$value <- factor(betas$value)

#betas$y <-betas$y/max(betas$y) 


#all distributions on the same plot
p2 <- ggplot(data=betas, aes(x=x,y=y, color = value)) + 
  geom_line(size = .8)+
  theme_classic(base_size = 18)+
  #scale_color_manual(values= c("#E69F00", "#E69F00", "#56B4E9","#56B4E9"), name="")+
  #scale_linetype_manual(values = c("solid", "dotted", "dashed", "dotted", "dashed"), name = "")+
  xlab('P(x)')+
  ylab('Density')+
  scale_x_continuous(breaks=c(0,0.5,1))+
  scale_color_brewer(palette = 'Set1')+
  theme(strip.background=element_blank(), legend.key=element_blank(), legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_blank(), legend.position='none')
p2

ggsave(filename = 'plots/beta.pdf', p2, width = 3, height = 2.5, unit = 'in')

