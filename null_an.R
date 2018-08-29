#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "1 Aug 2018"

require(lattice)


rm(list=ls())
graphics.off()


# Analyse null repeatability run

######################################### Read in data

data_files <- list.files(path="../Results/HPC_results/null/", pattern= '\\.csv$')

######################################### Make df to put data

len_dat <- length(data_files)


null_t <- data.frame(matrix(vector(),ncol=3))
colnames(null_t) <- c('closeness_repeat', 'betweenness_repeat', 'degree_repeat')
#run <- data_files[1]


################################ read in null data

null_m <- function(run){
  # function to read in all null data files and put them in table
  dat<-read.csv(paste('../Results/HPC_results/null/',run, sep=''))
  null_t <- rbind(null_t, dat)
  #return(null_t)
}



############################### run function for all null data files

for (z in data_files){
  print(z)
  null_t <-null_m(z)
}
hist(null_t$closeness_repeat)
hist(null_t$betweenness_repeat)
hist(null_t$degree_repeat)

####### closeness
m_c <- mean(null_t$closeness_repeat)
s_c <- sd(null_t$closeness_repeat)
cih_c <- m_c + (s_c*2)
cil_c <- m_c - (s_c*2)


####### betweenness
m_b <- mean(null_t$betweenness_repeat)
s_b <- sd(null_t$betweenness_repeat)
cih_b <- m_b + (s_b*2)
cil_b <- m_b - (s_b*2)


####### degree
m_d <- mean(null_t$degree_repeat)
s_d <- sd(null_t$degree_repeat)
cih_d <- m_d + (s_d*2)
cil_d <- m_d - (s_d*2)

##################################################### Calculate stats on to total data

var.interval = function(data, conf.level = 0.95) {
       df = length(data) - 1
       chilower = qchisq((1 - conf.level)/2, df)
       chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
       v = var(data)
       c(df * v/chiupper, df * v/chilower)
}
var.interval(null_t$betweenness_repeat)
hist(null_t$betweenness_repeat, breaks=30, xlab='Betweenness Repeatability', main= 'Null Distribution of Betweenness')


############################ Save data
write.csv(null_t, "../Results/null_rep.csv")