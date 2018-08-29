#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "25 Jul 2018"



require(dplyr)
require(plyr)
require(MCMCglmm)
require(ggplot2)

rm(list=ls())
graphics.off()

# Repeatability models

#old <- Sys.time()
######################################################################
# load and split data
######################################################################



data_t <- read.csv('../Results/total_soc.csv')

trips <- unique(data_t$trip)
len_t <- length(trips)



############################## Normalize Data
# see Alpin for Normalizing procedures

data_t$closeness_norm <- sqrt(data_t$closeness)
# take sqrt closeness data

data_t$degree_norm <- sqrt(data_t$degree)
# sqrt degree data

data_t$betweenness_norm <- log(data_t$betweenness)
# log betweenness data

data_t$betweenness_norm[data_t$betweenness_norm == "-Inf"] <- 0
# log produved non intergers - made to 0 - double check

data_t$BirdID <- data_t$individual


split_data<- data_t[ which(data_t$trip<8),]

male_data <- split_data[ which(split_data$sex == 1),]
  
female_data <- split_data[ which(split_data$sex == 0),]

  

r_d2 <- MCMCglmm(degree_norm~1, random=~individual, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)

r_b2 <- MCMCglmm(betweenness_norm~1, random=~individual, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)

r_c2 <- MCMCglmm(closeness_norm~1, random=~individual, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)


len_dat <- 6
results <- data.frame('Run'=rep(0,len_dat),'Rep_Pos'=rep(0,len_dat),'Rep_HDP_L'=rep(0,len_dat),
                      'Rep_HDP_H'=rep(0,len_dat), 'Gender'=rep(0,len_dat))




####Male
r_dm <- MCMCglmm(degree_norm~1, random=~individual, data=male_data, nitt=100000, burnin=1000, verbose=FALSE)

Rdm2 <- r_dm$VCV[,"individual"]/(r_dm$VCV[,"individual"]+r_dm$VCV[,"units"])







r_bm <- MCMCglmm(betweenness_norm~1, random=~individual, data=male_data, nitt=100000, burnin=1000, verbose=FALSE)

Rbm2 <- r_bm$VCV[,"individual"]/(r_bm$VCV[,"individual"]+r_bm$VCV[,"units"])





r_cm <- MCMCglmm(closeness_norm~1, random=~individual, data=male_data, nitt=100000, burnin=1000, verbose=FALSE)
Rcm2 <- r_cm$VCV[,"individual"]/(r_cm$VCV[,"individual"]+r_cm$VCV[,"units"])



results$Run[1]<-'Degree'
results$Run[2]<-'Betweenness'
results$Run[3]<-'Closeness'

results$Gender[1]<-'Male'
results$Gender[2]<-'Male'
results$Gender[3]<-'Male'

results$Rep_Pos[1]<-posterior.mode(Rdm2)
results$Rep_Pos[2]<-posterior.mode(Rbm2)
results$Rep_Pos[3]<-posterior.mode(Rcm2)

results$Rep_HDP_L[1]<-HPDinterval(Rdm2)[1]
results$Rep_HDP_L[2]<-HPDinterval(Rbm2)[1]
results$Rep_HDP_L[3]<-HPDinterval(Rcm2)[1]

results$Rep_HDP_H[1]<-HPDinterval(Rdm2)[2]
results$Rep_HDP_H[2]<-HPDinterval(Rbm2)[2]
results$Rep_HDP_H[3]<-HPDinterval(Rcm2)[2]

####Female

r_df <- MCMCglmm(degree_norm~1, random=~individual, data=female_data, nitt=100000, burnin=1000, verbose=FALSE)
Rdf2 <- r_df$VCV[,"individual"]/(r_df$VCV[,"individual"]+r_df$VCV[,"units"])
posterior.mode(Rdf2)
HPDinterval(Rdf2)
r_bf <- MCMCglmm(betweenness_norm~1, random=~individual, data=female_data, nitt=100000, burnin=1000, verbose=FALSE)
Rbf2 <- r_bf$VCV[,"individual"]/(r_bf$VCV[,"individual"]+r_bf$VCV[,"units"])
posterior.mode(Rbf2)
HPDinterval(Rbf2)
r_cf <- MCMCglmm(closeness_norm~1, random=~individual, data=female_data, nitt=100000, burnin=1000, verbose=FALSE)

Rcf2 <- r_cf$VCV[,"individual"]/(r_cf$VCV[,"individual"]+r_cf$VCV[,"units"])
posterior.mode(Rcf2)
HPDinterval(Rcf2)


results$Run[4]<-'Degree'
results$Run[5]<-'Betweenness'
results$Run[6]<-'Closeness'

results$Gender[4]<-'Female'
results$Gender[5]<-'Female'
results$Gender[6]<-'Female'

results$Rep_Pos[4]<-posterior.mode(Rdf2)
results$Rep_Pos[5]<-posterior.mode(Rbf2)
results$Rep_Pos[6]<-posterior.mode(Rcf2)

results$Rep_HDP_L[4]<-HPDinterval(Rdf2)[1]
results$Rep_HDP_L[5]<-HPDinterval(Rbf2)[1]
results$Rep_HDP_L[6]<-HPDinterval(Rcf2)[1]

results$Rep_HDP_H[4]<-HPDinterval(Rdf2)[2]
results$Rep_HDP_H[5]<-HPDinterval(Rbf2)[2]
results$Rep_HDP_H[6]<-HPDinterval(Rcf2)[2]


pdf('../Results/Plots/gen_repeat.pdf')
ggplot(results, aes(x=Run, y=Rep_Pos, colour=Gender))+geom_point()+geom_errorbar(aes(ymin=Rep_HDP_L, ymax=Rep_HDP_H), width=0.1)+
  ylim(-.1,.8)+
  #geom_errorbar(aes(ymin=low, ymax=high), color='red', width=0.1)+
  labs( x='Measure of Centrality', y='Repeatability')+
  ggtitle("Repeatability of Node Centrality By Gender")+theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

save(mt, file= 'mt_d7.rda')



fp <-read.csv("../Data/pedigree.csv", header=T)
fp <- rename(fp, c("animal"="id"))

fp <- subset(fp, select=c('id', 'dam', 'sire'))

male_data <- rename(male_data, c('individual'='animal'))

ped_indv <- unique(fp$id)
male_data <- subset(male_data, male_data$animal %in% fp$id)

mt <- MCMCglmm(betweenness_norm~1, random=~animal+BirdID+trip+mat_id+soc_brood, data=male_data, ped=fp, nitt=1000000, burnin=10000, verbose=FALSE)
          
mt2 <- MCMCglmm(degree_norm~1, random=~animal+BirdID+trip+mat_id+soc_brood, data=male_data, ped=fp, nitt=1000000, burnin=10000, verbose=FALSE)

VP2=(mt$VCV[,"BirdID"]+mt$VCV[,"animal"]+mt$VCV[,"trip"]+mt$VCV[,"units"]+mt$VCV[,"mat_id"]+mt$VCV[,"soc_brood"])	
h2<-(mt$VCV[,"animal"])/VP2	
posterior.mode(h2)
#print(head(null_t))


VP3=(mt2$VCV[,"BirdID"]+mt2$VCV[,"animal"]+mt2$VCV[,"trip"]+mt2$VCV[,"units"]+mt2$VCV[,"mat_id"]+mt2$VCV[,"soc_brood"])	
h3<-(mt2$VCV[,"animal"])/VP3	
posterior.mode(h3)




dat13=data_t[which(data_t$year==2013),]
dat14=data_t[which(data_t$year==2014),]
dat15=data_t[which(data_t$year==2015),]
dat16=data_t[which(data_t$year==2016),]
dat17=data_t[which(data_t$year==2017),]




#print(head(null_t))

#null_t$run[c(1:10)] <- c(11:20)

#elapsed <- Sys.time() - old
#print(paste0("runtime: ", elapsed))
#head(split_data[[2]])
#test function running
#mt_d2 <- MCMCglmm(degree_norm~1, random=~animal+BirdID, data=dat_f, ped=fp, nitt=1000000, burnin=100000, verbose=FALSE)

#######################################
# Tests
#######################################
  
# num = c(1,2,3,4,5,6,7,8,9)
# let = c('a','b','c','d','e','f','g','h','i')
# df = data.frame(num,let)
# df
# data <- df[,1][sample(1:nrow(df)), ]
# df$num <- sample(df$num)
# df
