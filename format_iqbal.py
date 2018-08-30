#!/usr/bin/env python3
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "03 July 2018"

import pandas as pd
from datetime import datetime
import sys
import os
import glob

"""formats Iqbal's data to add to Alfredo's"""


path = "../Data/iqbal_data/Interactions/"
all_files = glob.glob(os.path.join(path,"*.csv"))
# creates list of all .CSV files in a directory

combined_csv = pd.concat( [ pd.read_csv(f) for f in all_files ] )
# combine all files in list into one df



# combined_csv['Date'] = pd.to_datetime(combined_csv['Date'], format="%d-%m-%Y")
combined_csv['start'] = pd.to_datetime(combined_csv['start'], format="%Y-%m-%d %H:%M:%S")
combined_csv['end'] = pd.to_datetime(combined_csv['end'], format="%Y-%m-%d %H:%M:%S")
# # # make date date time

combined_csv['Date'] = combined_csv['start'].dt.date


combined_csv['trip'] = 8
# Adds trip number based on date to all of the rows - time period defined above

#combined_csv.to_csv("../Results/iqbal_test.csv")

combined_csv.to_csv("../Results/iqbal_total.csv")





