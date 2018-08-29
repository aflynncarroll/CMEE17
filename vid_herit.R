#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "27 Apr 2018"

require(MCMCglmm)
require(pedantics)
require(dplyr)
require(plyr)

# Creating a workflow to analyse repeatability from video data

rm(list=ls())
graphics.off()




PhenData<-read.csv("../Results/video_soc.csv", header=T)
# reads in the phenotypic data - cohort is a column


dat_t <- subset(PhenData, PhenData$closeness != "NA")
dat_t <- subset(PhenData, PhenData$degree != "NA")
dat_t <- subset(PhenData, PhenData$betweenness != "NA")
# subset where social measures do not = NA
# No NA's in the data set to begin with


table(table(PhenData$individual))
# number of times individuals have been measured - ie 302 just once


############################## Normalize Data
# see Alpin for Normalizing procedures

dat_t$closeness_norm <- sqrt(dat_t$closeness)
# take sqrt closeness data

dat_t$degree_norm <- sqrt(dat_t$degree)
# sqrt degree data

dat_t$betweenness_norm <- log(dat_t$betweenness)
# log betweenness data

dat_t$betweenness_norm[dat_t$betweenness_norm == "-Inf"] <- 0
# log produved non intergers - made to 0 - double check

############################################################################################################
############################################################################################################
# Data Subsets
############################################################################################################
############################################################################################################

dat_male <- subset(dat_t, dat_t$sex == 1)
# just the males

dat_female <- subset(dat_t, dat_t$sex == 0)
# just the females

############################################################################################################
############################################################################################################
# Repeatability 
############################################################################################################
############################################################################################################

# Function to calculate repeatability
# Plug in different DF's later ie M/F

rep_vid <- function(dat){ 
  
  
  ############################################################################################################
  # Individuals 
  ############################################################################################################
  
  print('Measures')
  print(length(dat$individual))
  
  print('Individuals')
  print(length(unique(dat$individual)))
  
  ############################################################################################################
  # Closeness 
  ############################################################################################################
  
  
  m1 <- MCMCglmm(closeness_norm~1, random = ~individual, data=dat, nitt=100000, burnin=10000, verbose=FALSE)
  # runs for closeness
  
  plot(m1$VCV)
  
  summary(m1)
  # summary stats on the model
  
  Vp1 = (m1$VCV[,"individual"] + m1$VCV[,"units"])
  # the total phenotypic variance of closeness
  
  posterior.mode(Vp1)
  # total variance
  
  HPDinterval(Vp1)
  # 95% confidence interval
  
  var(dat$closeness)
  # estimate still within confidence interval
  
  ############################################################################
  
  R1 <- m1$VCV[,"individual"]/(m1$VCV[,"individual"]+m1$VCV[,"units"])
  # calculates repeatability
  
  print("Closenness")
  
  print(posterior.mode(R1))
  # repeatability
  
  print(HPDinterval(R1))
  # confidence interval
  
  ############################################################################################################
  # Degree 
  ############################################################################################################
  
  m2 <- MCMCglmm(degree_norm~1, random = ~individual, data=dat, nitt=100000, burnin=10000, verbose=FALSE)
  # runs for degree
  
  plot(m2$VCV)
  
  summary(m2)
  # summary stats on the model
  
  Vp2 = (m2$VCV[,"individual"] + m2$VCV[,"units"])
  # the total phenotypic variance of Winglength
  
  posterior.mode(Vp2)
  # total variance
  
  HPDinterval(Vp2)
  # 95% confidence interval
  
  var(dat$degree)
  # estimate still within confidence interval
  
  ############################################################################
  
  R2 <- m2$VCV[,"individual"]/(m2$VCV[,"individual"]+m2$VCV[,"units"])
  # calculates repeatability
  
  
  print("Degree")
  print(posterior.mode(R2))
  # repeatability
  
  print(HPDinterval(R2))
  # confidence interval
  
  ############################################################################################################
  # Betweenness 
  ############################################################################################################
  
  m3 <- MCMCglmm(betweenness_norm~1, random = ~individual, data=dat, nitt=100000, burnin=10000, verbose=FALSE)
  # runs for betweenness
  
  plot(m3$VCV)
  
  summary(m3)
  # summary stats on the model
  
  Vp3 = (m3$VCV[,"individual"] + m3$VCV[,"units"])
  # the total phenotypic variance of Winglength
  
  posterior.mode(Vp3)
  # total variance
  
  HPDinterval(Vp3)
  # 95% confidence interval
  
  var(dat$betweenness)
  # estimate still within confidence interval
  
  ############################################################################
  
  R3 <- m3$VCV[,"individual"]/(m3$VCV[,"individual"]+m3$VCV[,"units"])
  # calculates repeatability
  
  
  print("Betweenness")
  
  print(posterior.mode(R3))
  # repeatability
  
  print(HPDinterval(R3))
  # confidence interval
}



rep_vid(dat_t)
rep_vid(dat_male)
rep_vid(dat_female)
# runs for all 3 df's


############################################################################
############################################################################
# The Animal Model 
############################################################################
############################################################################


############################################################################
# Read in data and plot pedigree
############################################################################

fp <- read.table("../Data/PedigreeUpToIncl2016.txt", header =TRUE)
# read in pedigree data

str(fp)
# prints info on pedigree




########### works fine

#graphics.off()

#drawPedigree(fp,dots="y")
# draws pedigree map

#measuredPed <- fp
# made from old pedigree- new map to favor measured individuals

#measuredPed$Measured <- ifelse(measuredPed$id %in% dat_t$individual =="TRUE",1,0)
#measuredPed
# new column to indicate if it was measured


############


dat_t<- rename(dat_t, c("individual"="animal"))
# rename column to match 

dat_t$BirdID <- dat_t$animal
# makes a new column as a clone of BirdID as to use pedigree info in the animal model

fp2 <- data.frame(fp$id, fp$dam, fp$sire)
fp2 <- rename(fp2, c("fp.id"="animal", "fp.dam"="dam", "fp.sire"="sire"))
# choose & rename columns to match


###################################### test redraw

fp2 <- fixPedigree(fp2)
#orderPed(fp2)

fp2 <- rename(fp2, c("id"="animal"))

fp3<-fp2
#creates fp3 as to add measured column

fp3$Measured <- ifelse(fp2$animal %in% dat_t$animal =="TRUE",1,0)
#adds column to indicate that individual was measured

graphics.off()

drawPedigree(fp3, dots="Y", dat=fp3$Measured, retain="informative")
# draw pedigree, but grey out uninformative individuals

#stats.fp <- pedigreeStats(fp3, graphicalReport = 'n')

stats.fp.pruned <- pedigreeStats(fp2, dat=fp3$Measured, retain="informative")
# prodeces statistics on the relevent individuals - those who relate to measured individuals
# had to use fp2 measured as it would not accept pedigree longer than 3 columns
# produced same results when used the same format in draw pedigree

pedStatSummary(stats.fp.pruned)
# produces a condenced set of statistics on the pedigree information

#names(stats.fp.pruned)
# produces a list of stats you can retrieve on the pedigree

############################################################################
# calculate heritability
############################################################################


#################################### This does not work

m2 <- MCMCglmm(closeness_norm~1, random=~animal+BirdID, data=dat_t, ped=fp2, nitt=100000, burnin=10000, verbose=FALSE)
# runs the animal model

#Error in MCMCglmm(closeness_norm ~ 1, random = ~animal + BirdID, data = dat_t,  : 
#   some levels of animal do not have a row entry in ginverse


###################################
# Random Tests Below###############

run_data <- subset(dat_t[c("animal", "closeness_norm", "BirdID")])



