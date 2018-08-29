#!/bin/bash                                                                     
#PBS -l walltime=60:00:00                                                       
#PBS -l select=1:ncpus=1:mem=2gb                                                
#module load R
module load anaconda3/personal
module load intel-suite
echo "R is about to run"                                                        
R --vanilla <$WORK/hered_p_hpc.R                                               
mv *.rda $WORK                                                          
echo "R has finished running"
 
