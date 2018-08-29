#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "11 Feb 2018"

rm(list=ls())
graphics.off()

require(igraph)
require(dplyr)
require(plyr)





# Makes network pdf used on the front cover


f_name <- 'vid_adj_data'
data <- read.csv(paste('../Data/',f_name,'.csv', sep=''), header=T)
# video data file 


#################### Change Ring code to ID code



data <- rename(data, c("date.ELO2"="date", "individual1"="id1", "individual2"="id2"))
###############################################################################


trips <- data.matrix(unique(data$trip, incomparables = FALSE, MARGIN = 1, fromLast = FALSE))
# makes a df of list of unique recording days

dlen_d <- nrow(trips)
# number of days


########################## make network

ev=1
df <- subset(data, trip == trips[ev,])

indv <- data.frame(df$id1, df$id2)
# make individual data into df

net <- graph.data.frame(indv, directed=F)
# network plot format for data

E(net)$weight <- 1
net2 <- simplify(net, edge.attr.comb = list(weight="sum"))


################################ save plot

pdf("../writting/ex_net.pdf")
plot(net2,vertex.color="white", vertex.size=3, vertex.label=NA)
dev.off()
