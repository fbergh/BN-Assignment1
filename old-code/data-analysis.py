'''
The "#%%" lines are used for Visual Studio Code (and perhaps other editors) to execute a .py file as a notebook.
Every "#%%" represents a cell.

Note: functions that print something (like head or describe) need to be in separate cells, otherwise they won't show.
'''

#%%
# Import packages
import pandas as pd 
import numpy as np 

# Create dictionaries for converting the strings in the "day" column to booleans and the strings in the "month" column
# to integers
di_day = {"mon":0,"tue":0,"wed":0,"thu":0,"fri":0,"sat":1,"sun":1}
di_mon = {"jan":1, "feb":2, "mar":3, "apr":4, "may":5, "jun":6, "jul":7, "aug":8, "sep":9, "oct":10, "nov":11, "dec":12}

#%%
# Read data
df = pd.read_csv("forestfires.csv")
df.head(10)

#%%
# Pre-process data
df = df.drop(['X','Y'],axis=1)
df = df.replace({"month":di_mon})
df = df.replace({"day":di_day})
df = df.sort_values(by=["month"])
df.head(10)

#%%
df.describe()

#%%
# Show correlation matrix between all variables to see potentially related variables
df.corr()

#%%
# Get descriptions of all variables for every month and for every weekend 
# and export to Excel because the output is too large
month_desc = df.drop("day",axis=1).groupby('month').describe()
month_desc.to_excel("temp/month_desc.xlsx")
#%%
day_desc = df.drop('month',axis=1).groupby('day').describe()
day_desc.to_excel("temp/day_desc.xlsx")

#%%
