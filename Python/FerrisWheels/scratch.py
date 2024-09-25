#%%
# loading in libraries
import pandas as pd
import numpy as np
#%%
# loading in data
wheels = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv', usecols=lambda c: not c.startswith('Unnamed:'))

#%%
### EDA ###
# looking at example data
wheels.head()
#%%
# descriptive stats -- numerical
wheels.describe()

# could make a total seats capacity?
# number_of_cabins * passengers_per_cabin
#%%
# descriptive stats -- categorical
wheels.describe(include=object)

#%%
my_list = [2, 'apple']
my