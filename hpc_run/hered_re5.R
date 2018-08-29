#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#!/usr/bin/env Rscript
#__version__ = "0.0.1"
#__date__ = "26 Jun 2018"

#install.packages("MCMCglmm", repos = "https://cran.ma.imperial.ac.uk/")
#install.packages("pedantics", repos = "https://cran.ma.imperial.ac.uk/")
#install.packages("dplyr", repos = "https://cran.ma.imperial.ac.uk/")
#install.packages("plyr", repos = "https://cran.ma.imperial.ac.uk/")
#install.packages(c("MCMCglmm", "dplyr","plyr"))
 
require(MCMCglmm)
#require(pedantics)
require(dplyr)
require(plyr)



# Creating a workflow to analyse heritability from video data

rm(list=ls())
graphics.off()



set.seed(1)

iter= as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
            

PhenData <- read.csv("/work/af2017/total_soc.csv", header=T)
#PhenData <- read.csv("total_soc.csv", header=T)
# reads in the phenotypic data - cohort is a column

fp <- read.csv("/work/af2017/pedigree.csv", header=T)
#fp <- read.csv("pedigree.csv", header=T)
# reads in previously fixed pedigree

fp <- rename(fp, c("animal"="id"))

fp <- subset(fp, select=c('id', 'dam', 'sire'))

dat_t <- subset(PhenData, PhenData$closeness != "NA")
dat_t <- subset(PhenData, PhenData$degree != "NA")
dat_t <- subset(PhenData, PhenData$betweenness != "NA")
# subset where social measures do not = NA
# No NA's in the data set to begin with



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


#stats.fp.pruned <- pedigreeStats(fp2, dat=fp3$Measured, retain="informative")
# stats on pedigree


ped_indv <- unique(fp$id)
# individuals present in pedigree

dat_t<- rename(dat_t, c("individual"="animal"))
# rename column to match 



dat_f <- subset(dat_t, dat_t$animal %in% fp$id)
# subset of data included in pedigree

dat_f$BirdID <- dat_f$animal

############################################################################
# MCMCglmm parallel run iterations
############################################################################



if(iter==1){
  mt <- MCMCglmm(closeness_norm~1, random=~animal+BirdID+trip+mat_id+soc_brood, data=dat_f, ped=fp, nitt=1000000, burnin=10000, verbose=FALSE)
  # account individual variation :closeness
  save(mt, file= 'mt_c5.rda')
}

if(iter==2){
  mt <- MCMCglmm(betweenness_norm~1, random=~animal+BirdID+trip+mat_id+soc_brood, data=dat_f, ped=fp, nitt=1000000, burnin=10000, verbose=FALSE)
  # betweenness
  save(mt, file= 'mt_b5.rda')
}

if(iter==3){
  mt <- MCMCglmm(degree_norm~1, random=~animal+BirdID+trip+mat_id+soc_brood, data=dat_f, ped=fp, nitt=1000000, burnin=10000, verbose=FALSE)
  # degree
  save(mt, file= 'mt_d5.rda')
}
