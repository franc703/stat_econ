import pandas as pd
import numpy as np
#%%

# set a random seed to obtain same results every run
np.random.seed(123)

# create data frame
df = pd.DataFrame({
    # Have W go from 0 to .1
    'W': np.random.uniform(0, .1, size = 200)})

# Higher W makes X = 1 more likely
df['X'] = np.random.uniform(size = 200) < .2 + df['W']

# True effect of X on Y is 3
df['Y'] = 3*df['X'] + df['W'] + np.random.normal(size = 200)

