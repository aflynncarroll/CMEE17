#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "20 Mar 2018"

require(MCMCglmm)
require(pedantics)
require(ggplot2)
require(dplyr)
require(xtable)



rm(list=ls())
graphics.off()


# Analyse HPC run and save results to a table


############################ Read in date

data_files <- list.files(path="../Results/HPC_results/models_2/", pattern= '\\.rda$')
null<- read.csv("../Results/null_rep.csv")

########################### Create table of data

len_dat <- length(data_files)
results <- data.frame('Run'=rep(0,len_dat),'Rep_Pos'=rep(0,len_dat),'Rep_HDP_L'=rep(0,len_dat),
                        'Rep_HDP_H'=rep(0,len_dat),'Her_Pos'=rep(0,len_dat),'Her_HDP_L'=rep(0,len_dat),
                        'Her_HDP_H'=rep(0,len_dat),'Env_Pos'=rep(0,len_dat),'Env_HDP_L'=rep(0,len_dat),
                        'Env_HDP_H'=rep(0,len_dat),'Trip_Pos'=rep(0,len_dat),'Trip_HDP_L'=rep(0,len_dat),
                        'Trip_HDP_H'=rep(0,len_dat),'Mat_Pos'=rep(0,len_dat), 'Mat_HDP_L'=rep(0,len_dat),
                        'Mat_HDP_H'=rep(0,len_dat),'Soc_Pos'=rep(0,len_dat),'Soc_HDP_L'=rep(0,len_dat),
                        'Soc_HDP_H'=rep(0,len_dat), 'Den_Pos'=rep(0,len_dat), 'Den_HDP_L'=rep(0,len_dat),
                        'Den_HDP_H'=rep(0,len_dat) )
# m2 <- 
  
#extract_run <- function(i){  
#plot(m2$Sol)
#plot(m2$VCV)
#file <- data_files[i]
#load(paste('../Results/',file, sep=''))

#f_name<-gsub(pattern='.rda$','',file)
#m2 <- get(f_name)

############################# Run -trip-animal-
run1 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"trip"]+m2$VCV[,"animal"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"trip"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  tr<-(m2$VCV[,"trip"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  tr_v <- posterior.mode(tr)
  results[i,"Trip_Pos"] <- tr_v
  tr_in <- HPDinterval(tr)
  results[i,"Trip_HDP_L"] <- tr_in[1]
  results[i,"Trip_HDP_H"] <- tr_in[2]
  return(results)
  
}

results <- rbind(run1('mt_c1',1))
results <- rbind(run1('mt_b1',2))
results <- rbind(run1('mt_d1',3))


######################### trip -BirdID-animal

run2 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  en_v <- posterior.mode(en)
  results[i,"Env_Pos"] <- en_v
  en_in <- HPDinterval(en)
  results[i,"Env_HDP_L"] <- en_in[1]
  results[i,"Env_HDP_H"] <- en_in[2]
  return(results)
  
}

results <- rbind(run2('mt_c2',4))
results <- rbind(run2('mt_b2',5))
results <- rbind(run2('mt_d2',6))

############################### Run -BirdID-animal-trip

run3 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"trip"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  tr<-(m2$VCV[,"trip"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  en_v <- posterior.mode(en)
  results[i,"Env_Pos"] <- en_v
  en_in <- HPDinterval(en)
  results[i,"Env_HDP_L"] <- en_in[1]
  results[i,"Env_HDP_H"] <- en_in[2]
  
  tr_v <- posterior.mode(tr)
  results[i,"Trip_Pos"] <- tr_v
  tr_in <- HPDinterval(tr)
  results[i,"Trip_HDP_L"] <- tr_in[1]
  results[i,"Trip_HDP_H"] <- tr_in[2]
  return(results)
  
}

results <- rbind(run3('mt_c3',7))
results <- rbind(run3('mt_b3',8))
results <- rbind(run3('mt_d3',9))

############################## Run -BirdID-animal-trip-mat_id-

run4 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"trip"]+m2$VCV[,"units"]+m2$VCV[,"mat_id"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  tr<-(m2$VCV[,"trip"])/VP2	
  mat<-(m2$VCV[,"mat_id"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  en_v <- posterior.mode(en)
  results[i,"Env_Pos"] <- en_v
  en_in <- HPDinterval(en)
  results[i,"Env_HDP_L"] <- en_in[1]
  results[i,"Env_HDP_H"] <- en_in[2]
  
  tr_v <- posterior.mode(tr)
  results[i,"Trip_Pos"] <- tr_v
  tr_in <- HPDinterval(tr)
  results[i,"Trip_HDP_L"] <- tr_in[1]
  results[i,"Trip_HDP_H"] <- tr_in[2]
  
  mat_v <- posterior.mode(mat)
  results[i,"Mat_Pos"] <- mat_v
  mat_in <- HPDinterval(mat)
  results[i,"Mat_HDP_L"] <- mat_in[1]
  results[i,"Mat_HDP_H"] <- mat_in[2]
  return(results)
  
}

results <- rbind(run4('mt_c4',10))
results <- rbind(run4('mt_b4',11))
results <- rbind(run4('mt_d4',12))

########################## Run -BirdID-animal-trip-mat_id-soc_brood-

run5 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"trip"]+m2$VCV[,"units"]+m2$VCV[,"mat_id"]+m2$VCV[,"soc_brood"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  tr<-(m2$VCV[,"trip"])/VP2	
  mat<-(m2$VCV[,"mat_id"])/VP2	
  soc<-(m2$VCV[,"soc_brood"])/VP2
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  en_v <- posterior.mode(en)
  results[i,"Env_Pos"] <- en_v
  en_in <- HPDinterval(en)
  results[i,"Env_HDP_L"] <- en_in[1]
  results[i,"Env_HDP_H"] <- en_in[2]
  
  tr_v <- posterior.mode(tr)
  results[i,"Trip_Pos"] <- tr_v
  tr_in <- HPDinterval(tr)
  results[i,"Trip_HDP_L"] <- tr_in[1]
  results[i,"Trip_HDP_H"] <- tr_in[2]
  
  mat_v <- posterior.mode(mat)
  results[i,"Mat_Pos"] <- mat_v
  mat_in <- HPDinterval(mat)
  results[i,"Mat_HDP_L"] <- mat_in[1]
  results[i,"Mat_HDP_H"] <- mat_in[2]
  
  soc_v <- posterior.mode(soc)
  results[i,"Soc_Pos"] <- soc_v
  soc_in <- HPDinterval(soc)
  results[i,"Soc_HDP_L"] <- soc_in[1]
  results[i,"Soc_HDP_H"] <- soc_in[2]
  return(results)
  
}

results <- rbind(run5('mt_c5',13))
results <- rbind(run5('mt_b5',14))
results <- rbind(run5('mt_d5',15))

################################### Run -density-animal-

run6 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"density"]+m2$VCV[,"animal"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"density"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  den<-(m2$VCV[,"density"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  den_v <- posterior.mode(den)
  results[i,"Den_Pos"] <- den_v
  den_in <- HPDinterval(den)
  results[i,"Den_HDP_L"] <- den_in[1]
  results[i,"Den_HDP_H"] <- den_in[2]
  return(results)
  
}

results <- rbind(run6('mt_c6',16))
results <- rbind(run6('mt_b6',17))
results <- rbind(run6('mt_d6',18))



########################## Run -BirdID-animal-density

run7 <- function(run, x){
  
  load(paste('../Results/HPC_results/models_2/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"density"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  den<-(m2$VCV[,"density"])/VP2	
  # calculating heritability permanent environment and repeatability
  results[i,"Run"] <- run
  rep_v <- posterior.mode(R2)
  results[i,"Rep_Pos"] <- rep_v
  rep_in <- HPDinterval(R2)
  results[i,"Rep_HDP_L"] <- rep_in[1]
  results[i,"Rep_HDP_H"] <- rep_in[2]
  # for repeatability (same as before)
  
  her_v <- posterior.mode(h2)
  results[i,"Her_Pos"] <- her_v
  her_in <-HPDinterval(h2)
  results[i,"Her_HDP_L"] <- her_in[1]
  results[i,"Her_HDP_H"] <- her_in[2]
  # for heritability - mean and confidence interval
  
  en_v <- posterior.mode(en)
  results[i,"Env_Pos"] <- en_v
  en_in <- HPDinterval(en)
  results[i,"Env_HDP_L"] <- en_in[1]
  results[i,"Env_HDP_H"] <- en_in[2]
  
  den_v <- posterior.mode(den)
  results[i,"Den_Pos"] <- den_v
  den_in <- HPDinterval(den)
  results[i,"Den_HDP_L"] <- den_in[1]
  results[i,"Den_HDP_H"] <- den_in[2]
  return(results)
  
}

results <- rbind(run7('mt_c7',19))
results <- rbind(run7('mt_b7',20))
results <- rbind(run7('mt_d7',21))




confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

ci_c <- confidence_interval(null$closeness_repeat, 0.95) 
ci_b <- confidence_interval(null$betweenness_repeat, 0.95) 
ci_d <- confidence_interval(null$degree_repeat, 0.95) 

############################ Make tables

rf<- results[c(13,14,15),]
rf$low <- c(ci_c[1],ci_b[1],ci_d[1])
rf$high <- c(ci_c[2],ci_b[2],ci_d[2])
rf$type <- c('Closeness','Betweenness', 'Degree')

run_5 <- results[13:15,]
run_5 <- run_5[, !(colnames(run_5) %in% c("Den_Pos","Den_HDP_L", "Den_HDP_H"))]
run_5 <- arrange(run_5, desc(Run))
run_5_1<-subset(run_5, select=c(1:10))
run_5_2<-subset(run_5, select=c(1,11:19))


######################## Export tables/plots in LaTeX format

xtable(run_5_1)
xtable(run_5_2)
cols <- c('Measured'='black','Null'='red')


############ plot
pdf('../Results/Plots/repeat.pdf')
ggplot(rf, aes(x=type, y=Rep_Pos))+geom_point()+geom_errorbar(aes(ymin=Rep_HDP_L, ymax=Rep_HDP_H, color='Measured'), width=0.1)+
  ylim(-.1,.8)+
  geom_errorbar(aes(ymin=low, ymax=high,color='Null'),  width=0.1)+
  labs( x='Measure of Centrality', y='Repeatability')+
  ggtitle("Repeatability of Node Centrality")+theme_classic()+
  scale_colour_manual(name="Data",values=cols)+ scale_fill_manual(name="Bar",values=cols)+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
#for (x in len_dat)
#  extract_run(x)

den <- results[19:21,]
den2 <- select(den, Run, Rep_Pos,Rep_HDP_L,Rep_HDP_H,Her_Pos,Her_HDP_L,Her_HDP_H,Env_Pos,Env_HDP_L,Env_HDP_H,Den_Pos,Den_HDP_L,Den_HDP_H)
xtable(den2)

######################## Save data
write.csv(results, file = "../Results/hpc_table.csv")
