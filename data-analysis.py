#%%
import pandas as pd 
import numpy as np 

di_day = {"mon":0,"tue":0,"wed":0,"thu":0,"fri":0,"sat":1,"sun":1}
di_mon = {"jan":1, "feb":2, "mar":3, "apr":4, "may":5, "jun":6, "jul":7, "aug":8, "sep":9, "oct":10, "nov":11, "dec":12}

df = pd.read_csv("forestfires.csv")
df = df.drop(['X','Y'],axis=1)
df = df.replace({"month":di_mon})
df = df.replace({"day":di_day})
df = df.sort_values(by=["month"])
df.head(20)

#%%
df.corr()

#%%
df.describe()

#%%
df.groupby('month').describe()
df.groupby('day').describe()

#%%
