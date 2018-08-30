#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "26 Mar 2018"

require(igraph)
require(dplyr)
require(plyr)

rm(list=ls())
graphics.off()

#args <- commandArgs(trailingOnly = TRUE)
# use command line arguments

#random <- args[1]


# Makes social networks and extracts social values



###############################################################################
# Read in Data
###############################################################################
f_name <- 'vid_adj_data'

sparrows <- read.csv('../Data/Lundy_sparrows.csv', header=T)

data <- read.csv(paste('../Data/',f_name,'.csv', sep=''), header=T)
# video data file 

coh_sex <- read.csv('../Data/coh_sex.csv', header=T)
brood <- read.csv('../Data/brood.csv', header=T)

id <- read.csv('../Data/birdsex.1_validcolourcodes_AST.csv', header=T)
# colour ring to ID file

ped <- read.table("../Data/PedigreeUpToIncl2016.txt", header =TRUE)
# pedigree data

###############################################################################
# Format Data
###############################################################################


#################### Change Ring code to ID code

data$individual1 <- id$BirdID[match(data$individual1, id$Code)]
# change bird id1 to ID code from ring data

data$individual2 <- id$BirdID[match(data$individual2, id$Code)]
# change bird id2 to ID code from ring data


#################### Change sex1 to 0 or 1
data$sex1 <- as.character(data$sex1)
data$sex2 <- as.character(data$sex2)
data$sex1[data$sex1=="m"] <- 1
data$sex1[data$sex1=="f"] <- 0
data$sex2[data$sex2=="m"] <- 1
data$sex2[data$sex2=="f"] <- 0
# turns column of factors into characters 
# then turns them into 0 or 1 to fit with other data



###############################################################################
# function to make networks and make social data
###############################################################################
#data$date <- format(as.Date(data$date, format="%Y/%m/%d"), "%Y")
data$date<-as.numeric(format(as.Date(data$date, '%Y-%m-%d'), '%Y'))


soc_trip <- function(ev){ 
  
  df <- subset(data, trip == trips[ev,])
  
  indv <- data.frame(df$individual1, df$individual2)
  # make individual data into df
  
  net <- graph.data.frame(indv, directed=F)
  # network plot format for data
  
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  # collapses multiple edges to weighted edges
  
  pdf(file=paste('../Results/Plots/network_',trips[ev,],'.pdf', sep=''))
  plot(net2, vertex.size=2, vertex.label=NA, vertex.color="grey50", layout=layout_nicely(net2)) 
  dev.off()
  
  den <- edge_density(net2, loops=FALSE)
  
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
  
  
  
  soc$cohort <- coh_sex$Cohort[match(soc$individual, coh_sex$BirdID)]
  # adds cohort to the measured individuals
  
  soc$year <- df$date[1]
  
  
  soc$age <- soc$year - soc$cohort
  
  soc$sex <- coh_sex$SexEstimate[match(soc$individual, coh_sex$BirdID)]
  # soc$sex <- data$sex1[match(soc$individual, data$id1)]
  # soc$sex <- data$sex2[match(soc$individual, data$id2)]
  # adds gender to sparrows 
  # 0 = F
  
  soc$soc_brood <- brood$RearingBrood[match(soc$individual, brood$BirdID)]
  
  soc$trip <- ev
  soc$type <- 1
  # adds trip column 
  
  soc$density <- den
  
  #paste("soc_t", trip, sep="") <- soc
  #nam <- paste("soc_t", trip, sep="")
  #assign(nam, soc)
  # assigns new name to soc based on day
  #paste("soc_t", trip, sep="") <<- paste("soc_t", trip, sep="")
  return(soc)
}


###############################################################################
# loop to run functions for days
###############################################################################



trips <- data.matrix(unique(data$trip, incomparables = FALSE, MARGIN = 1, fromLast = FALSE))
# makes a df of list of unique recording days

dlen_d <- nrow(trips)
# number of days


#soc_t1 <- setNames(data.frame(matrix(ncol = 9, nrow = 9)), c("individual", "closeness", "degree", "betweenness", "cohort", "year", "age", "sex", "trip"))
# sets up df to populate later for average modularity


for (i in 1:dlen_d){
  #var <- paste("soc_t",i, sep="")
  #eval(parse(text=var)) <- soc_trip(i)
  #paste("soc_t",i, sep="") <- soc_trip(i)
  assign(paste("soc_t",i, sep=""), soc_trip(i))
}

soc_total <- rbind(soc_t1,soc_t2,soc_t3,soc_t4,soc_t5,soc_t6,soc_t7)

write.csv(soc_total, '../Results/video_soc.csv')
