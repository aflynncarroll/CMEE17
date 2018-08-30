#!/usr/bin/env python3
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "20 Apr 2018"


import numpy as np
import pandas as pd
from datetime import datetime
import sys
import itertools
from collections import namedtuple
import os
import glob


# Part I
# Finds how long individuals were in the spaarrow cage
# Folder structure:
# Have 'Code' 'Data' & 'Results' folders within one folder
# Runs out of Code folder, opens file from Data folder,
# and saves to Results folder


#extension = 'CSV'
#path = "../Data/iqbal_data/"
#os.chdir(path)
#iqbal_files = [i for i in glob.glob('*.{}'.format(extension))]
#all_files = glob.glob(os.path.join(path,"*.CSV"))
glo = glob.glob("../Data/iqbal_data/*.CSV")
#all_files = glob.glob(os("*.CSV"))
# creates list of all .CSV files in a directory

#cd = [os.path.splitext(x)[0] for x in iqbal_files]
iq_d = [os.path.splitext(x)[0] for x in glo]
iq_d = [s.replace("../Data/iqbal_data/","") for s in iq_d]


############################################################################################
# Read in all of Iqbal's data
############################################################################################


def parse_iq(cage_date):
    """Runs analysis on all dates"""
    #cage_date = sys.argv[-1]
    filename = "../Data/iqbal_data/"+cage_date+".CSV"
    out_name = cage_date
    # Command line input to run and save file
    # To run: example below
    # run sparrow_cage2.py '20171130'

    print("********************* starting: " +cage_date+ " *********************")

    cage_data = pd.read_csv(filename, sep = ',', header = 'infer',  usecols = [0,1,2,3,6])
    # reads in raw cage data

    ############################################################################################
    # use only if data is in only one column
    # ~data will not run properly through code~
    ############################################################################################

    #cage_data = pd.read_csv(filename, sep = ';', header = 'infer')
    #cage_data = pd.read_csv(filename, sep = ',', header = 'infer')
    # loads in population statistics csv with only the required columns
    #	return cage_data

    #cage_data = (cage_data['Identifier'].str.split(';', expand = True))
    #cage_data.columns= ['Identifier','Date', 'Time','Unit number', 'Antena number', 'Transponder type', 'Transponder code', 'Weight','Input status','Output status', 'Event', 'GPS coordinates']
    #cage_data.drop(cage_data.columns[[0]], axis=1)
    #columns = ['Transponder type','Antena number','Weight','Input status','Output status', 'Event', 'GPS coordinates']
    #cage_data.drop(columns, inplace=True, axis=1)
    #use above four lines if data is in only one column

    ############################################################################################
    # Format Date and time into usable datetime format for Pandas
    ############################################################################################


    cage_data["date_time"] = cage_data["Date"].map(str) + ' ' + cage_data["Time"].map(str)
    # combines seperate date and time into a single column

    cage_data['date_time2'] = [datetime.strptime(x, '%d-%m-%Y %H:%M:%S') for x in cage_data['date_time'] ] 
    # forms date_time into correct format

    cage_data['Identifier']=range(len(cage_data))
    cage_data = cage_data.set_index(pd.Index(cage_data['Identifier']))
    # sets time as pandas' DatetimeIndex


    ############################################################################################
    #order data and set up final df of in-out times
    ############################################################################################


    unique = pd.unique(cage_data['Transponder code']) 
    # prints array of unique RFID's

    d = {}
    for i in unique:
        d[i] = pd.DataFrame(cage_data.loc[(cage_data['Transponder code']== i) ])
    #creates a dictionary of different data frames for each sparrow

    in_out = pd.DataFrame()
    tempDF = pd.DataFrame(columns=["time_in","time_out","id"])
    in_out= pd.concat([in_out, tempDF])
    #sets up df to append in out times to in loop

    ############################################################################################
    #For loop to find in-out times for all sparrows in file
    ############################################################################################

    for n in d.keys():
        ind_data = d[n]
        # loops through all of the sparrow df's

        workinglist = []
        # temp working list to tie in out times together 

        insidelist = pd.DataFrame()
        tempDF = pd.DataFrame(columns=["time_in","time_out","id"])
        insidelist= pd.concat([insidelist, tempDF])
        # formats df in loop to add in out times to for each df
        # will append each sparrow set to in_out df
        #print(n) # for troubleshooting

        insideflag = False
        # sets initial flag state to false
        # used as a marker of sensor type to determine in out times
        for x in ind_data['Identifier']:
            #print(x, ind_data.loc[x, 'Unit number'] ) # to test if fails
            if insideflag:
                if ind_data.loc[x, 'Unit number'] not in (20,30):
                    insideflag = False # closes in out pair
                    workinglist.append((ind_data.loc[x, 'date_time2']))
                    workinglist.append((ind_data.loc[x, 'Transponder code']))
                    #adds out time and transponder code to in time created in the else conditional

                    workinglist = pd.DataFrame(workinglist)
                    workinglist = workinglist.transpose()
                    workinglist.columns = ["time_in","time_out","id"]
                    # formats workinglist to a df so it can be appended to in_out[]

                    insidelist=pd.concat([insidelist, workinglist])
                    # adds the data from this df to insidelist (total for outter for-loop)
                    #print(insidelist) # for troubleshooting

                    workinglist = [] # resets workinglist
            else:
                if ind_data.loc[x, 'Unit number'] in (20,30):
                    insideflag = True # looking for the next false value now
                    workinglist.append(ind_data.loc[x, 'date_time2'])
                    # puts the in data point in workinglist
        if insideflag:
            print("**** Bird never left! Dead??", 'Time:', ind_data.loc[x, 'date_time2', ],'ID:', ind_data.loc[x,'Transponder code'], 'Reader:', ind_data.loc[x, 'Unit number'],'****')
            # if there is a sparrow that enters but never leaves this flags it and prints a statement to the console

        in_out = pd.concat([in_out, insidelist])
        # adds the data from all the loops to in_out[]

    ############################################################################################
    # Format df and save file
    ############################################################################################

    in_out['visit_time'] = pd.Series(in_out.time_out - in_out.time_in, index=in_out.index)
    # calculates the time spent in cage per entrance and adds it to a new column

    in_out_s = in_out.sort_values(by=['time_in','time_out'])
    #sorts df by entrance time

    in_out_s.to_csv("../Data/iqbal_data/Entrances/"+out_name+"_sorted.csv")
    # uses command line input to save in_out to a .csv file


    ##############################################################################################################################
    ##############################################################################################################################





    # Part II
    # Finds overlapping sparrows
    # Be sure to set overlap time window below

    ############################################################################################
    # Runs from file created above
    ############################################################################################

    filename2 = "../Data/iqbal_data/Entrances/"+out_name+"_sorted.csv"
    #out_name = sys.argv[-1]



    cage_data2 = pd.read_csv(filename2, sep = ',', header = 'infer', parse_dates = ['time_in','time_out'])
    # loads in population statistics csv with only the required columns

    ############################################################################################
    # Format df and set df to fill from for-loop
    ############################################################################################
    cage_data2 = cage_data2.reset_index(drop=True)
    cage_data2['index1'] = cage_data2.index
    # index cage_data2 by number 0:length 
    # this allows for the row to be called by its row number within the loop

    newdf = pd.DataFrame(list(itertools.combinations(cage_data2['index1'],2)))
    # creates df of unique time in pairs to compare 

    overlap = pd.DataFrame()
    tempDF = pd.DataFrame(columns=["id1","id2",'duration',"start","end"])
    overlap= pd.concat([overlap, tempDF])
    # creates and structures final df to fill in loop

    length = (len(newdf)-1)
    # number of lines to run through in loop

    range_s = namedtuple('range', ['start', 'end'])
    # set tuple 

    ############################################################################################
    # For loop to find overlapping time windows
    ############################################################################################

    for i in range(0,length):

        workinglist =[]
        #sets a working list for each loop to populate


        r1 = range_s(start=(cage_data2.loc[newdf[0][i],'time_in']), end=(cage_data2.loc[newdf[0][i],'time_out']))
        r2 = range_s(start=(cage_data2.loc[newdf[1][i], 'time_in']), end=(cage_data2.loc[newdf[1][i],'time_out']))
        # defines the two time ranges to be compared

        latest_start = max(r1.start, r2.start)
        # chooses the later start time out of the pair
        
        earliest_end = min(r1.end, r2.end)
        #chooses the earlier end time for the day
        # in combination these fins the overlapping time window

        delta = (earliest_end - latest_start).days + 1
        # work around to calculate if time overlaps
        # if yes the value will be 1


        if delta > 0:
            # if there is overlap do the following
            workinglist.append((cage_data2.loc[newdf[0][i], 'id']))
            workinglist.append((cage_data2.loc[newdf[1][i], 'id']))
            workinglist.append((earliest_end-latest_start)) # difference in time
            workinglist.append((latest_start))
            workinglist.append((earliest_end))
            #adds out time and transponder code to in time created in the else conditional

            workinglist = pd.DataFrame(workinglist)
            workinglist = workinglist.transpose()
            workinglist.columns = ["id1","id2",'duration',"start","end"]
            # reformats working list for easy appending to overlap
            overlap = pd.concat([overlap, workinglist])
            # add columns from current loop to overlap
            workinglist = []
            # reset workinglist (to blank)

    ############################################################################################
    # Remove interactions under a certain time
    ############################################################################################

    #overlap = pd.DataFrame(overlap[overlap.duration > pd.Timedelta(seconds=3)])
    # can put seconds, hours, days... in the minutes position
    # without this, all interactions (even 0s) are saved to a file

    ############################################################################################
    # Add index and save output data
    ############################################################################################

    overlap = overlap.reset_index()
    del overlap['index']
    # add index and delete non-functional index

    overlap.to_csv("../Data/iqbal_data/Interactions/"+out_name+"_overlap.csv")
    # save data




#parse_iq(cd)
#list(map(lambda x:parse_iq(),cd))
#map(parse_iq, iq_d)

#parse_iq('20170710')


