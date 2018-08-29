#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "10 Aug 2018"

rm(list=ls())
graphics.off()

require(ggplot2)
require(gridExtra)


# Plot centrality data by trip

t_soc <- read.csv( '../Results/total_soc.csv')
rf_soc <- t_soc[ which(t_soc$trip==8),]
t_soc$individual <- as.factor(t_soc$individual)
#ggplot(t_soc, aes(x=t_soc$trip, y= degree))+geom_point()


######################################## split data by trip
data_split <- split(t_soc, t_soc$trip, drop=false)
trip <- c(1,2,3,4,5,6,7,8)
mean_d <- c(mean(data_split[[1]]$degree), mean(data_split[[2]]$degree),mean(data_split[[3]]$degree),mean(data_split[[4]]$degree),
           mean(data_split[[5]]$degree),mean(data_split[[6]]$degree),mean(data_split[[7]]$degree),mean(data_split[[8]]$degree))
mean_b <- c(mean(data_split[[1]]$betweenness), mean(data_split[[2]]$betweenness),mean(data_split[[3]]$betweenness),mean(data_split[[4]]$betweenness),
            mean(data_split[[5]]$betweenness),mean(data_split[[6]]$betweenness),mean(data_split[[7]]$betweenness),mean(data_split[[8]]$betweenness))
mean_c <- c(mean(data_split[[1]]$closeness), mean(data_split[[2]]$closeness),mean(data_split[[3]]$closeness),mean(data_split[[4]]$closeness),
            mean(data_split[[5]]$closeness),mean(data_split[[6]]$closeness),mean(data_split[[7]]$closeness),mean(data_split[[8]]$closeness))

################################### Make df for avrg data from trips
data_avg<- data.frame(cbind(trip,mean_d,mean_b,mean_c))


deg <- ggplot(data=t_soc, mapping = aes(x=trip, y= degree, color=individual))+geom_point(size=.5)+geom_line(size=.2)+ 
  theme_classic()+theme(legend.position="none")+
  labs(x='Trip', y='Degree') +scale_color_grey()+
  ggplot(data= data_avg, aes(x=trip, y= mean_degree, colour = 'red'))

clo<- ggplot(data=t_soc, mapping = aes(x=trip, y= closeness, color=individual))+geom_point(size=.5)+geom_line(size=.2)+ theme_classic()+theme(legend.position="none")+
  labs(x='Trip', y='Closeness')+scale_color_grey()

bet<- ggplot(data=t_soc, mapping = aes(x=trip, y= betweenness, color=individual))+geom_point(size=.5)+geom_line(size=.2)+ theme_classic()+theme(legend.position="none")+
  labs(x='Trip', y='Betweenness')+scale_color_grey()

#pdf('../Results/Plots/soc_time.pdf')
#grid.arrange(deg,bet,clo)
#dev.off()


################################## plot data

deg2<- ggplot()+
  geom_point(data=t_soc, mapping = aes(x=trip, y= degree))+
  theme_classic()+theme(legend.position="none")+
  geom_line(data= data_avg, aes(x=trip, y= mean_d, color = 'red'))
  
clo2<- ggplot()+
  geom_point(data=t_soc, mapping = aes(x=trip, y= closeness))+
  theme_classic()+theme(legend.position="none")+
  geom_line(data= data_avg, aes(x=trip, y= mean_c, color = 'red'))

bet2<- ggplot()+
  geom_point(data=t_soc, mapping = aes(x=trip, y= betweenness))+
  theme_classic()+theme(legend.position="none")+
  geom_line(data= data_avg, aes(x=trip, y= mean_b, color = 'red'))


################################# save data

pdf('../Results/Plots/soc_time2.pdf')
grid.arrange(deg2,bet2,clo2)
dev.off()
