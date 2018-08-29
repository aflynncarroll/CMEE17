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

# analyze hpc run for heritability of data with age inclusion




############################### Read in Data

data_files <- list.files(path="../Results/HPC_results/model_age/", pattern= '\\.rda$')
null<- read.csv("../Results/null_rep.csv")


############################### Create df to put variance analysis data
len_dat <- length(data_files)
results <- data.frame('Run'=rep(0,len_dat),'Rep_Pos'=rep(0,len_dat),'Rep_HDP_L'=rep(0,len_dat),
                      'Rep_HDP_H'=rep(0,len_dat),'Her_Pos'=rep(0,len_dat),'Her_HDP_L'=rep(0,len_dat),
                      'Her_HDP_H'=rep(0,len_dat),'Age_Pos'=rep(0,len_dat), 'Age_HDP_L'=rep(0,len_dat),
                      'Age_HDP_H'=rep(0,len_dat),'Env_Pos'=rep(0,len_dat),'Env_HDP_L'=rep(0,len_dat),
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

############################ Run with -age-animal-BirdID-

run1 <- function(run, x){
  
  load(paste('../Results/HPC_results/model_age/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"age"]+m2$VCV[,"animal"]+m2$VCV[,"units"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  age<-(m2$VCV[,"age"])/VP2	
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
  
  age_v <- posterior.mode(age)
  results[i,"Age_Pos"] <- age_v
  age_in <- HPDinterval(age)
  results[i,"Age_HDP_L"] <- age_in[1]
  results[i,"Age_HDP_H"] <- age_in[2]
  return(results)
  
}

results <- rbind(run1('mt_c_a',1))
results <- rbind(run1('mt_b_a',2))
results <- rbind(run1('mt_d_a',3))


########################## run with BirdID-animal-trip-mat_id-soc_brood-age-

run5 <- function(run, x){
  
  load(paste('../Results/HPC_results/model_age/',run, '.rda', sep=''))
  m2 <- mt
  #m2 <- get(run)
  i <- x
  #load.Rdata2(filename=f_name,objname=f_name, path='../Results/')
  VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"trip"]+m2$VCV[,"units"]+m2$VCV[,"mat_id"]+m2$VCV[,"soc_brood"]+m2$VCV[,"age"])	
  R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
  h2<-(m2$VCV[,"animal"])/VP2	
  en<-(m2$VCV[,"BirdID"])/VP2	
  tr<-(m2$VCV[,"trip"])/VP2	
  mat<-(m2$VCV[,"mat_id"])/VP2	
  soc<-(m2$VCV[,"soc_brood"])/VP2
  age<-(m2$VCV[,"age"])/VP2	
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
  
  age_v <- posterior.mode(age)
  results[i,"Age_Pos"] <- age_v
  age_in <- HPDinterval(age)
  results[i,"Age_HDP_L"] <- age_in[1]
  results[i,"Age_HDP_H"] <- age_in[2]
  return(results)
  
}

results <- rbind(run5('mt_c5a',4))
results <- rbind(run5('mt_b5a',5))
results <- rbind(run5('mt_d5a',6))

########################## Save Data

write.csv(results, file = "../Results/hpc_table.csv")
