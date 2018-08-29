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

#combined_csv['Date'] = combined_csv['start'].dt.date

#days = combined_csv['Date'].unique()
#days.size




# def sample_period(row):
#     """Assigns trip numbers to all interactions based on a date range."""
    # if (row['start'] >= pd.to_datetime('2017-06-13', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-06-15', format="%Y-%m-%d")):
    #     return 8
    # if (row['start'] >= pd.to_datetime('2017-06-15', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-06-23', format="%Y-%m-%d")):
    #     return 9
    # if (row['start'] >= pd.to_datetime('2017-06-23', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-06-25', format="%Y-%m-%d")):
    #     return 10
    # if (row['start'] >= pd.to_datetime('2017-06-25', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-07-07', format="%Y-%m-%d")):
    #     return 11
    # if (row['start'] >= pd.to_datetime('2017-07-07', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-07-13', format="%Y-%m-%d")):
    #     return 12
    # if (row['start'] >= pd.to_datetime('2017-07-13', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-07-14', format="%Y-%m-%d")):
    #     return 13
    # if (row['start'] >= pd.to_datetime('2017-07-14', format="%Y-%m-%d")) & (row['start'] < pd.to_datetime('2017-07-20', format="%Y-%m-%d")):
    #     return 14

#combined_csv['trip'] = combined_csv.apply(lambda row:sample_period(row), axis=1)
combined_csv['trip'] = 8
# Adds trip number based on date to all of the rows - time period defined above

#combined_csv.to_csv("../Results/iqbal_test.csv")

combined_csv.to_csv("../Results/iqbal_total.csv")







# Explore results

combined_csv.sort_values(by=['start'])

pd.value_counts(combined_csv['Date'].values, sort=False)

pd.value_counts(combined_csv['trip'].values, sort=False)



# def sample_period(row):
#     """Assigns trip numbers to all interactions based on a date range."""
#     if (row['Date'] == pd.to_datetime('2017-06-13', format="%Y-%m-%d")):
#         return 8
#     if (row['Date'] == pd.to_datetime('2017-06-14', format="%Y-%m-%d")):
#         return 8
#     if (row['Date'] == pd.to_datetime('2017-06-15', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-16', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-17', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-18', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-19', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-21', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-22', format="%Y-%m-%d")):
#         return 9
#     if (row['Date'] == pd.to_datetime('2017-06-23', format="%Y-%m-%d")):
#         return 10
#     if (row['Date'] == pd.to_datetime('2017-06-24', format="%Y-%m-%d")):
#         return 10
#     if (row['Date'] == pd.to_datetime('2017-06-25', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-06-26', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-06-28', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-02', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-03', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-04', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-05', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-06', format="%Y-%m-%d")):
#         return 11
#     if (row['Date'] == pd.to_datetime('2017-07-07', format="%Y-%m-%d")):
#         return 12
#     if (row['Date'] == pd.to_datetime('2017-07-08', format="%Y-%m-%d")):
#         return 12
#     if (row['Date'] == pd.to_datetime('2017-07-09', format="%Y-%m-%d")):
#         return 12
#     if (row['Date'] == pd.to_datetime('2017-07-10', format="%Y-%m-%d")):
#         return 13
#     if (row['Date'] == pd.to_datetime('2017-07-11', format="%Y-%m-%d")):
#         return 13
#     if (row['Date'] == pd.to_datetime('2017-07-12', format="%Y-%m-%d")):
#         return 13
#     if (row['Date'] == pd.to_datetime('2017-07-13', format="%Y-%m-%d")):
#         return 14
#     if (row['Date'] == pd.to_datetime('2017-07-14', format="%Y-%m-%d")):
#         return 14
#     if (row['Date'] == pd.to_datetime('2017-07-15', format="%Y-%m-%d")):
#         return 15
#     if (row['Date'] == pd.to_datetime('2017-07-16', format="%Y-%m-%d")):
#         return 15
#     if (row['Date'] == pd.to_datetime('2017-07-17', format="%Y-%m-%d")):
#         return 15
#     if (row['Date'] == pd.to_datetime('2017-07-18', format="%Y-%m-%d")):
#         return 15
#     if (row['Date'] == pd.to_datetime('2017-07-19', format="%Y-%m-%d")):
#         return 15
