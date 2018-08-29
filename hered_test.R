#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "26 Mar 2018"

require(igraph)
require(dplyr)

rm(list=ls())
graphics.off()

#args <- commandArgs(trailingOnly = TRUE)
# use command line arguments

#random <- args[1]


# Testing formating of lundy data from python code



###############################################################################
# Read in Data
###############################################################################
f_name <- 'fake_overlap_test'

sparrows <- read.csv('../Data/Lundy_sparrows.csv', header=T)

data <- read.csv(paste('../Data/',f_name,'.csv', sep=''), header=T)
# fake data file 

id <- read.csv('../Data/birdsex.1_validcolourcodes_AST.csv', header=T)
# colour ring to ID file

ped <- read.table("../Data/PedigreeUpToIncl2016.txt", header =TRUE)
# pedigree data

###############################################################################
# Format Data
###############################################################################


#################### Change Ring code to ID code

data$id1 <- id$BirdID[match(data$id1, id$Code)]
# change bird id1 to ID code from ring data

data$id2 <- id$BirdID[match(data$id2, id$Code)]
# change bird id2 to ID code from ring data


#################### extract data for sociability

indv <- data.frame(data$id1, data$id2)
# make individual data into df

net <- graph.data.frame(indv, directed=F)
# network plot format for data

E(net)$weight <- 1
net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
# collapses multiple edges to weighted edges


###############################################################################
# find sociability
###############################################################################

#deg <- degree(net2, mode="all")

#close <- closeness(net2, mode="all", weights=NA)

#betw <- betweenness(net2, directed=T, weights=NA)

################### tests 

betw <- as.data.frame(betweenness(net2, directed=F, weights=NA))
names(betw)[1] <- "betweenness"
betw$individual <- rownames(betw)
# creates and formats betweenness data


close <- as.data.frame(closeness(net2, mode="all", weights=NA))
names(close)[1] <- "closeness"
close$individual <- rownames(close)
# creates and formats closeness data


deg <- as.data.frame(degree(net2, mode="all"))
names(deg)[1] <- "degree"
deg$individual <- rownames(deg)
# creates and formats degree data


soc <- merge(close, deg, by="individual", all=TRUE)
soc <- merge(soc, betw, by="individual", all=TRUE)
# merges the data created above


#betw <- as.table(betweenness(net2, directed=F, weights=NA))

soc$cohort <- ped$Cohort[match(soc$individual, ped$id)]
# adds cohort to the measured individuals

soc$sex <- sparrows$Sex[match(soc$individual, sparrows$BirdID)]
# adds gender to sparrows 
# 0 = F
# 1 = M

soc$year <- 2018
# add sample year
# change depending on when data collected

soc$age <- soc$year-soc$cohort
# calculate age of sparrow
# year sampled-cohort year


write.csv(soc, paste('../Results/',f_name,'_soc.csv', sep=''))

          