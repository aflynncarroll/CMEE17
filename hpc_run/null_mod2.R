#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "25 Jul 2018"



require(dplyr)
require(plyr)
require(MCMCglmm)

rm(list=ls())
graphics.off()

#old <- Sys.time()
######################################################################
# load and split data
######################################################################


iter = as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

set.seed(iter)

data_t <- read.csv('../Results/total_soc.csv')

trips <- unique(data_t$trip)
len_t <- length(trips)


reps <- 250
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

split_data <- split(data_t, f = data_t$trip)
#stats.fp.pruned <- pedigreeStats(fp2, dat=fp3$Measured, retain="informative")
# stats on pedigree


######################################################################
# Functions
######################################################################

shuffle <- function(x){
  split_data[[x]]$BirdID <- sample(split_data[[x]]$BirdID)
  print(x)
  return(split_data[[x]])
}
# shuffles the individual column of one of the split segments

swap <- function(){
  for (i in c(1:len_t)){
    split_data[[i]] <- shuffle(i)
    #return(split_data[[i]])
  }
  split_data <- split_data
}
# calls shuffle: reorders all split individual columns within splits

#split_data <- swap()

null_t <- data.frame('degree_repeat'=rep(0,reps),'degree_low'=rep(0,reps),'degree_high'=rep(0,reps),'betweenness_repeat'=rep(0,reps),'betweenness_low'=rep(0,reps),'betweenness_high'=rep(0,reps),'closeness_repeat'=rep(0,reps),'closeness_low'=rep(0,reps),'closeness_high'=rep(0,reps))
# empty df to populate

run_reps <- function(r){
  for (t in c(1:r)){
    print(paste0("********** running rep: ",t,' **********'))
    split_data <- swap()
    print(head(split_data[[1]]))
    # runs the function: the output of function becomes new split_data
    split_data <- unsplit(split_data,f = data_t$trip, drop = FALSE)
    
    #r_d <- MCMCglmm(degree_norm~1, random=~BirdID, data=split_data, nitt=1000000, burnin=100000, verbose=FALSE)
    r_d2 <- MCMCglmm(degree_norm~1, random=~BirdID+trip+mat_id, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)
    
    R1 <- r_d2$VCV[,"BirdID"]/(r_d2$VCV[,"BirdID"]+r_d2$VCV[,"units"]+r_d2$VCV[,"trip"]+r_d2$VCV[,"mat_id"])
    
    null_t[t,"degree_repeat"] <- posterior.mode(R1)
    rep_r1 <-HPDinterval(R1)
    null_t[t,"degree_low"] <- rep_r1[1]
    null_t[t,"degree_high"] <- rep_r1[2]
    
    
    r_b2 <- MCMCglmm(betweenness_norm~1, random=~BirdID+trip+mat_id, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)
    
    R2 <- r_b2$VCV[,"BirdID"]/(r_b2$VCV[,"BirdID"]+r_b2$VCV[,"units"]+r_d2$VCV[,"trip"]+r_d2$VCV[,"mat_id"])
    
    null_t[t,"betweenness_repeat"] <- posterior.mode(R2)
    rep_r2 <-HPDinterval(R2)
    null_t[t,"betweenness_low"] <- rep_r2[1]
    null_t[t,"betweenness_high"] <- rep_r2[2]
    
    r_c2 <- MCMCglmm(closeness_norm~1, random=~BirdID+trip+mat_id, data=split_data, nitt=100000, burnin=1000, verbose=FALSE)
    
    R3 <- r_c2$VCV[,"BirdID"]/(r_c2$VCV[,"BirdID"]+r_c2$VCV[,"units"]+r_d2$VCV[,"trip"]+r_d2$VCV[,"mat_id"])
   
    null_t[t,"closeness_repeat"] <- posterior.mode(R3)
    rep_r3 <-HPDinterval(R3)
    null_t[t,"closeness_low"] <- rep_r3[1]
    null_t[t,"closeness_high"] <- rep_r3[2]
    
    
    split_data <- split(split_data, f = data_t$trip)
    #return(null_t)
  }
  return(null_t)
}

null_t <- run_reps(2)

write.csv(null_t, file=paste0("null_res_2_",iter,".csv"))
          
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
