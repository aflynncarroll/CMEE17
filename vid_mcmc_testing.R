#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "27 Apr 2018"

require(MCMCglmm)
require(pedantics)
require(dplyr)
require(plyr)

# Playing around with julia's practical to create workflow for project analysis

rm(list=ls())
graphics.off()

PhenData<-read.csv("../Results/video_soc.csv", header=T)
# reads in the phenotypic data - cohort is a column

head(PhenData)

dat_t <- subset(PhenData, PhenData$closeness != "NA")
# subset where wing length does not = NA


table(table(PhenData$individual))
# number of times individuals have been measured ie 302 just once


############################## Normalize Data
dat_t$closeness_norm <- sqrt(dat_t$closeness)

dat_t$degree_norm <- sqrt(dat_t$degree)

dat_t$betweenness_norm <- log(dat_t$betweenness)

dat_t$betweenness_norm[dat_t$betweenness_norm == "-Inf"] <- 0

############################################################################################################
############################################################################################################
# Data Subsets
############################################################################################################
############################################################################################################

dat_male <- subset(dat_t, dat_t$sex == 1)

dat_female <- subset(dat_t, dat_t$sex == 0)


############################################################################################################
############################################################################################################
# Tests (ignore) 
############################################################################################################
############################################################################################################
m <- MCMCglmm(closeness~1, random = ~individual, data=dat, nitt=1000, burnin=0)
# estimate variance in wing length due to bird ID 
# variance within birds (residual variance - units?)
# variance between birds
# ~1 dont want fixed effects
# nitt = number of MCMC iterations
# VCV = Posteriors for the variance components
# Sol = Posteriors for fixed parameters (resid)


plot(m$Sol)
# plots fixed effects for m

plot(m$VCV)
# plots for random efects ie the variance components 

m <- MCMCglmm(closeness~1, random = ~individual, data=dat)
# default nitt is 13,000
# same as above but run longer

plot(m$Sol)
# plots fixed effects for m

plot(m$VCV)
# plots for random efects ie the variance components 

autocorr(m$Sol)
# Marcov chain samples indipendantly between two subsequent iterations
# testing this

autocorr(m$VCV)
# high values for autocorr suggest run model for longer
# autocorr is ok if close to 0 (0.01, -0.01)

############################################################################################################
############################################################################################################
# Repeatability 
############################################################################################################
############################################################################################################
# Funtion

rep_vid <- function(dat){ 
  

############################################################################################################
# Individuals 
############################################################################################################

print('Measures')
length(dat$individual)

print('Individuals')
length(unique(dat$individual))

############################################################################################################
# Closeness 
############################################################################################################

#m1 <- MCMCglmm(closeness~1, random = ~individual, data=dat, nitt=10000)



m1 <- MCMCglmm(closeness_norm~1, random = ~individual, data=dat, nitt=100000, burnin=10000, verbose=FALSE)

plot(m1$VCV)

summary(m1)
# summary stats on the model

Vp1 = (m1$VCV[,"individual"] + m1$VCV[,"units"])
# the total phenotypic variance of Winglength

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
# verb=F silences run status updates

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
############################################################################
############################################################################
# The Animal Model - only need to do this
############################################################################
############################################################################

fp <- read.table("../Data/PedigreeUpToIncl2016.txt", header =TRUE)
# read in pedigree

str(fp)
# prints info on pedigree

graphics.off()

drawPedigree(fp,dots="y")
# draws pedigree map

measuredPed <- fp
# made from old pedigree- new map to favor measured individuals

measuredPed$Measured <- ifelse(measuredPed$id %in% dat$individual =="TRUE",1,0)
#measuredPed
# new column to indicate if it was measured

graphics.off()
################################no work
drawPedigree(measuredPed, dots="Y", dat=measuredPed$Measured, retain="informative")
# draws new map with grey lines equaling relationships between uninformative individuals
# gives total number in pedigree and number of informative

dat<- rename(dat, c("individual"="animal"))
dat$animal <- dat$BirdID
# makes a new column as a clone of BirdID as to use pedigree info in the animal model

fp2 <- data.frame(fp$id, fp$dam, fp$sire)
fp2 <- rename(fp2, c("fp.id"="animal", "fp.dam"="dam", "fp.sire"="sire"))

m2 <- MCMCglmm(closeness~1, random=~animal+BirdID, data=dat, ped=fp2, nitt=100000, burnin=10000, verbose=FALSE)
# runs the animal model

plot(m2$Sol)
plot(m2$VCV)

VP2=(m2$VCV[,"BirdID"]+m2$VCV[,"animal"]+m2$VCV[,"units"])	
R2<-(m2$VCV[,"BirdID"]+m2$VCV[,"animal"])/VP2	
h2<-(m2$VCV[,"animal"])/VP2	
PE<-(m2$VCV[,"BirdID"])/VP2	
# calculating heritability permanent environment and repeatability

posterior.mode(R2)
HPDinterval(R2)
# for repeatability (same as before)

posterior.mode(h2)
HPDinterval(h2)
# for heritability - mean and confidence interval

posterior.mode(PE)
HPDinterval(PE)
# the last variance component:
#       the permanent environment