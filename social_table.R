#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "20 Apr 2018"

require(igraph)
require(dplyr)

rm(list=ls())
graphics.off()


# finds social values from 'date'_overlap.csv file and creates a table of values


data <- read.csv('../Data/fake_overlap_test.csv', header=T)
# fake data file 

###############################################################################
# Format Data
###############################################################################

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