import pandas as pd
import numpy as np
from itertools import product
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

# create a function with the previous
def create_data(N = 200):
    df = pd.DataFrame({
        'W': np.random.uniform(0, .1, size = N)})
    df['X'] = np.random.uniform(size = N) < .2 + df['W']
    df['Y'] = 3*df['X'] + df['W'] + np.random.normal(size = N)
    return df

create_data(500)

#%%
np.random.seed(123)

# N for number of individuals and T for number of periods
def create_panel_data(N = 200, T = 10):
    # Use product to get all combinations of individual and time
    p = pd.DataFrame(
        product(range(0,N), range(0, T))
    )
    p.columns = ['ID', 't']

    # Individual and time-varying variable
    p['W1'] = np.random.normal(size = N*T)

    # Individual Data
    indiv_data = pd.DataFrame({
        'ID' : range(0, N),
        'W2' : np.random.normal(size = N)
    })

    # Bring them together
    p = p.merge(indiv_data, on = 'ID')

    # Create X, caused by W1 and W2
    p['X'] = 2*p['W1'] + 1.5*p['W2'] + np.random.normal(size = N*T)

    # Create Y the true effect of X on Y is 3
    # W1 and W2 has causal effect too

    p['Y'] = 3*p['X'] + p['W1'] - 2*p['W2'] + np.random.normal(size = N*T)
    return p

# Create heterocedasticity
def create_het_data(N = 200):
    d = pd.DataFrame({
        'X': np.random.uniform(size = N)
    })
    # Let the standard deviation of the error be related to X.
    d['Y'] = 3*d['X'] + np.random.normal(scale = 5*d['X'])

    return d

create_het_data(500)
