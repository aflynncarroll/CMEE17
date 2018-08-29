#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "10 Jul 2018"


require(dplyr)
require(plyr)


rm(list=ls())
graphics.off()

# Combine Iqbal/Alfredo's data so they can be uploaded to the hpc


######################### Define Data Files
vid_data <- read.csv("../Results/video_soc.csv", header=T)
iqbal_data <- read.csv("../Results/iqbal_soc.csv", header=T)

fp <- read.csv("../Data/pedigree.csv", header=T)


######################## Combine Data
total_data <- rbind(vid_data, iqbal_data)

total_data$mat_id <- fp$dam[match(total_data$individual, fp$animal)]
# add maternal id's


####################### Save file
write.csv(total_data, '../Results/total_soc.csv')
