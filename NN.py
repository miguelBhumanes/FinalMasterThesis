'''This script implements all the neural network models included in the benchmark'''

### 1 IMPORTING ALL THE PACKAGES AND DATA

# Importing required packages
import pandas as pd
import numpy as np
import random
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error as mse
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import GRU, Dense, LSTM, Input, Layer, Flatten, Dropout, MultiHeadAttention
from tensorflow.keras import backend as K
from fredapi import Fred

# Importing the created factors and blocks
# 'Monthly' corresponds to the 5 economic factors and the 4 blocks
# 'Daily' corresponds to the 5 financial factors and the 5 text factors
monthly_vars = pd.read_csv('monthly_vars.csv') # 276 rows, 10 variables (date, 5 dims and 4 blocks)
monthly_vars = monthly_vars.drop(['block.survey','block.labor', 'block.real', 'block.price'],axis=1) # Removing factors
daily_vars = pd.read_csv('daily_vars.csv') # 6000 rows, 11 vars (date, 5 fin, 5 text)
daily_vars.set_index('date', inplace=True) # Setting date as index of daily variable. For monthly data is done in a later line

### 2 CREATING TARGET VARIABLES 
# Inflation and GDP year over year growth
# to forecast 1 and 4 quarters ahead, taking 1 full year of data

# Importing GDP and Inflation data from January 1999 to December 2022
fred = Fred(api_key="your_key_here")
real_gdp_data = fred.get_series('GDPC1')
real_gdp_data = real_gdp_data['1999':'2022']
cpi_data = fred.get_series('CPIAUCSL')
cpi_data = cpi_data['1999':'2022']

# Computing the yearly rates of growth, which are the target variables
gdp_growth_y = ((real_gdp_data - real_gdp_data.shift(4)) / real_gdp_data.shift(4)).dropna()
inflation_data = ((cpi_data - cpi_data.shift(12)) / cpi_data.shift(12)).dropna()

# Moving to last day of the month
gdp_growth_y.index = gdp_growth_y.index + pd.DateOffset(months=3, days=-1)
inflation_data.index = inflation_data.index + pd.DateOffset(months=1, days=-1)

# Using this same index for the montly regressor variables
monthly_vars = monthly_vars.drop('date',axis=1)
monthly_vars.index = inflation_data.index

# Scaling (to use sigmoid as output layer)
gdp_g_scaled = (gdp_growth_y-min(gdp_growth_y))/(max(gdp_growth_y) - min(gdp_growth_y))
inflation_scaled = (inflation_data-min(inflation_data))/(max(inflation_data) - min(inflation_data))

# Getting all the targets: GDP Growth
y_1q = gdp_g_scaled[gdp_growth_y.index>="2001-03-31"] # For 1q ahead forecasting
y_4q = gdp_g_scaled[gdp_growth_y.index>="2001-12-31"] # For 4q ahead forecasting

# Getting all the targets: CPI Inflation
p_1q = inflation_scaled[inflation_data.index>="2001-03-31"] # For 1q ahead forecasting
p_4q = inflation_scaled[inflation_data.index>="2001-12-31"] # For 4q ahead forecasting

# Note: 
# y_1q has 88 datapoints, and y_4q has 85 datapoints
# (it was 23 years -> 92 quarters. But first year lost to predict. So 88 quarters.
# And when 4 quarters ahead, there are 3 quarters more lost. So 85)
# p_1q has 262 datapoints, and p_4q has 253
# (it was 23 years -> 276 monhts. But first year lost to predict. So 264 monhts
# and 2 more months because we are focusing on quarters so moving until March. So 262 monhts)
# And also losing 9 quarters more when moving 3q ahead for 4q ahead prediciton. So 253 months. 

### 3 CREATING FEATURE MATRIX

# Scaling data, and converting it back to dataframe
# (The data should be scaled in neural networks. Faster convergence)
scaler_xd = scaler_xm = MinMaxScaler()
daily_vars_scaled = scaler_xd.fit_transform(daily_vars)
monthly_vars_scaled = scaler_xm.fit_transform(monthly_vars)
daily_vars_scaled = pd.DataFrame(daily_vars)
daily_vars_scaled.index = daily_vars.index
monthly_vars_scaled = pd.DataFrame(monthly_vars)
monthly_vars_scaled.index = monthly_vars.index

# monthly vars has 276 entries and daily vars has 6000 entries (23 years of data)
# Exactly 12 months per year and like 65.21 days per quarter

# Creating the windows of observations (1 year. 12 observations for monthly, 259 for daily)
X_samples_monthly_forgdp = []
X_samples_daily_forgdp = []
X_samples_monthly_forinf = []
X_samples_daily_forinf = []

# Now for every date available on the dataset, take the last 259 days and 12 months of data 
# (The reason for 259 is that the first year there is only 259 days, instead of 260)
# And with them create a dataset of features associated to that data
# We are saying: we are going to predict the GDP/Inflation next quarter with all 
# the data from the past year. 
# This dataset for each datapoint is stacked together into an array with the datasets for 
# the rest of datapoints. 
for index_date in (y_1q.index + pd.DateOffset(months=-3, days=0)):
    regressors_daily = daily_vars[daily_vars.index <= str(index_date)]
    regressors_daily = regressors_daily.tail(259)
    regressors_monthly = monthly_vars[monthly_vars.index <= str(index_date)]
    regressors_monthly = regressors_monthly.tail(12)
    X_samples_daily_forgdp.append(regressors_daily)
    X_samples_monthly_forgdp.append(regressors_monthly)

X_samples_daily_np_gdp = np.stack([df.values for df in X_samples_daily_forgdp], axis=0)
X_samples_monthly_np_gdp = np.stack([df.values for df in X_samples_monthly_forgdp], axis=0)

for index_date in (p_1q.index + pd.DateOffset(months=-3, days=0)):
    regressors_daily = daily_vars[daily_vars.index <= str(index_date)]
    regressors_daily = regressors_daily.tail(259)
    regressors_monthly = monthly_vars[monthly_vars.index <= str(index_date)]
    regressors_monthly = regressors_monthly.tail(12)
    X_samples_daily_forinf.append(regressors_daily)
    X_samples_monthly_forinf.append(regressors_monthly)

X_samples_daily_np_inf = np.stack([df.values for df in X_samples_daily_forinf], axis=0)
X_samples_monthly_np_inf = np.stack([df.values for df in X_samples_monthly_forinf], axis=0)

# Create the different windows for the different models 
len_gdp = X_samples_daily_np_gdp.shape[0]
len_inflation = X_samples_daily_np_inf.shape[0]

X_samples_daily_np_y1q = X_samples_daily_np_gdp
X_samples_monthly_np_y1q = X_samples_monthly_np_gdp
X_samples_daily_np_y4q = X_samples_daily_np_gdp[:(len_gdp-3)]
X_samples_monthly_np_y4q = X_samples_monthly_np_gdp[:(len_gdp)-3]

X_samples_daily_np_p1q = X_samples_daily_np_inf
X_samples_monthly_np_p1q = X_samples_monthly_np_inf
X_samples_daily_np_p4q = X_samples_daily_np_inf[:(len_inflation-9)]
X_samples_monthly_np_p4q = X_samples_monthly_np_inf[:(len_inflation)-9]

# Do the train test split for each of the prediction targets
# 80% train+validation (68% train 12% validation split) - 20% test

X_gdp_train_d_q1 = X_samples_daily_np_y1q[0:70] # 70 datasets of 259 days of 10 variables
X_gdp_train_m_q1 = X_samples_monthly_np_y1q[0:70] # 70 datasets of 12 months of 9 variables 
X_gdp_test_d_q1 = X_samples_daily_np_y1q[70:89] # 18 datasets of 259 days of 10 variables
X_gdp_test_m_q1 = X_samples_monthly_np_y1q[70:89] # 18 datasets of 12 months of 9 variables
y_train_q1 = y_1q[0:70] # 70 observations of GPD growth
y_test_q1 = y_1q[70:89] # 18 test observations of GDP growth

X_gdp_train_d_q4 = X_samples_daily_np_y4q[0:68] 
X_gdp_train_m_q4 = X_samples_monthly_np_y4q[0:68] 
X_gdp_test_d_q4 = X_samples_daily_np_y4q[68:86] 
X_gdp_test_m_q4 = X_samples_monthly_np_y4q[68:86]
y_train_q4 = y_4q[0:68] 
y_test_q4 = y_4q[68:86] 

X_inf_train_d_q1 = X_samples_daily_np_p1q[0:210] # 210 datasets of 259 days of 10 variables
X_inf_train_m_q1 = X_samples_monthly_np_p1q[0:210] # same. 210 x 12 x 9
X_inf_test_d_q1 = X_samples_daily_np_p1q[210:263] # Same, with 52 observations
X_inf_test_m_q1 = X_samples_monthly_np_p1q[210:263] # Same idea
p_train_q1 = p_1q[0:210] # same
p_test_q1 = p_1q[210:263] # same 

X_inf_train_d_q4 = X_samples_daily_np_p4q[0:202] 
X_inf_train_m_q4 = X_samples_monthly_np_p4q[0:202] 
X_inf_test_d_q4 = X_samples_daily_np_p4q[202:253] 
X_inf_test_m_q4 = X_samples_monthly_np_p4q[202:253] 
p_train_q4 = p_4q[0:202] 
p_test_q4 = p_4q[202:253] 

### 4 DEFINING CONVOLUTION LAYERS

# Define the nealmon functions for computing the weights
def nealmon_m(param1, param2):
    i_values = tf.range(1, 4, dtype=tf.float32) # Will condense 3 months into a quarter
    ll = (param1 * i_values + param2 * i_values**2) - tf.math.reduce_logsumexp((param1 * i_values + param2 * i_values**2))
    nealmon_weights = tf.exp(ll)
    return nealmon_weights

def nealmon_q(param5, param6):
    i_values = tf.range(1, 65, dtype=tf.float32) # Will condense 64 days into a quarter
    ll = (param5 * i_values + param6 * i_values**2) - tf.math.reduce_logsumexp((param5 * i_values + param6 * i_values**2))
    nealmon_weights = tf.exp(ll)
    return nealmon_weights

# Define a custom layer for the monthly data (converts monthly to quarterly)
class CustomConv1D_m(Layer):
    def __init__(self, **kwargs):
        super(CustomConv1D_m, self).__init__(**kwargs)

    def build(self, input_shape):
        self.param1 = self.add_weight(name='param1', shape=(), initializer=tf.constant_initializer(0), trainable=True)
        self.param2 = self.add_weight(name='param2', shape=(), initializer=tf.constant_initializer(0), trainable=True)

    def call(self, inputs):
        nealmon_weights = nealmon_m(self.param1, self.param2)
        w = tf.reshape(nealmon_weights, (3, 1, 1)) # 3 because 3 months per quarter
        kernel = w * tf.ones((3, 5, 5), dtype=tf.float32) # 9 monthly variables
        return K.conv1d(x=inputs, kernel=kernel, strides=3) # Strides = 3 months per quarter
    
# Define a custom layer for the daily data (Converts daily to quarterly)
class CustomConv1D_dtoq(Layer):
    def __init__(self, **kwargs):
        super(CustomConv1D_dtoq, self).__init__(**kwargs)

    def build(self, input_shape):
        self.param5 = self.add_weight(name='param5', shape=(), initializer=tf.constant_initializer(0), trainable=True)
        self.param6 = self.add_weight(name='param6', shape=(), initializer=tf.constant_initializer(0), trainable=True)
        
    def call(self, inputs):
        nealmon_weights = nealmon_q(self.param5, self.param6)
        w = tf.reshape(nealmon_weights, (64, 1, 1)) # 64 days in a quarter
        kernel = w * tf.ones((64, 10, 10), dtype=tf.float32) # 10 daily variables
        return K.conv1d(x=inputs, kernel=kernel, strides=64)

# Custom layer to aggregate the daily data to the quarter, using the average
class CustomConv1D_dtoq_simple(Layer):
    def __init__(self, **kwargs):
        super(CustomConv1D_dtoq_simple, self).__init__(**kwargs)

    def call(self, inputs):
        nealmon_weights = nealmon_q(0,0) # Force equal weighting
        w = tf.reshape(nealmon_weights, (64, 1, 1))
        kernel = w * tf.ones((64, 10, 10), dtype=tf.float32)
        return K.conv1d(x=inputs, kernel=kernel, strides=64)
    
# Custom layer to aggregate the monthly data to the quarter, using the average
class CustomConv1D_mtoq_simple(Layer):
    def __init__(self, **kwargs):
        super(CustomConv1D_mtoq_simple, self).__init__(**kwargs)

    def call(self, inputs):
        nealmon_weights = nealmon_m(0,0) # Force equal weighting
        w = tf.reshape(nealmon_weights, (3, 1, 1))
        kernel = w * tf.ones((3, 5, 5), dtype=tf.float32)
        return K.conv1d(x=inputs, kernel=kernel, strides=3)    
    

### 5 IMPLEMENT AND TEST THE MODELS

'''To test the different models, change the inputs and dependent variables of the models (available in section 4).
Also, remember to change the line where the predictions are converted back to their original units.
All the possible options are commented out, so the user can use the required line for every model
'''

### Defining early stopping callback and the optimizer
early_stopping = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=100,restore_best_weights=True)
custom_optimizer = tf.keras.optimizers.legacy.Adam(learning_rate=0.0001)

#### 10.1 ANN 

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_mtoq_simple()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq_simple()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])
            Flat = Flatten()(Concat)

            # Creating the feed forward layers
            x = Flat
            for _ in range(hlayers):
                x = Dense(units=nodes, activation='relu')(x)
                x = Dropout(rate=dropout_rate)(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_inf_train_d_q4,X_inf_train_m_q4], p_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_inf_test_d_q4,X_inf_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            # rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
ann_naive = min(list_of_results, key=lambda x: x[3])

#### 10.2 ANN MIDAS

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_m()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])
            Flat = Flatten()(Concat)

            # Creating the feed forward layers
            x = Flat
            for _ in range(hlayers):
                x = Dense(units=nodes, activation='relu')(x)
                x = Dropout(rate=dropout_rate)(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_inf_train_d_q4,X_inf_train_m_q4], p_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_inf_test_d_q4,X_inf_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            # rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
ann_midas = min(list_of_results, key=lambda x: x[3])

#### 10.3 LSTM

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_mtoq_simple()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq_simple()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers-1):
                x = LSTM(units=nodes, activation='relu', return_sequences=True)(x)
                x = Dropout(rate=dropout_rate)(x)
            x = LSTM(units=nodes, activation='relu', return_sequences=False)(x)
            x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_gdp_train_d_q4,X_gdp_train_m_q4], y_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_gdp_test_d_q4,X_gdp_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
lstm_naive = min(list_of_results, key=lambda x: x[3])

#### 10.4 LSTM MIDAS

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_m()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers-1):
                x = LSTM(units=nodes, activation='relu', return_sequences=True)(x)
                x = Dropout(rate=dropout_rate)(x)
            x = LSTM(units=nodes, activation='relu', return_sequences=False)(x)
            x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_inf_train_d_q4,X_inf_train_m_q4], p_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_inf_test_d_q4,X_inf_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            # rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
lstm_midas = min(list_of_results, key=lambda x: x[3])

#### 10.5 GRU

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_mtoq_simple()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq_simple()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers-1):
                x = GRU(units=nodes, activation='relu', return_sequences=True)(x)
                x = Dropout(rate=dropout_rate)(x)
            x = GRU(units=nodes, activation='relu', return_sequences=False)(x)
            x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_inf_train_d_q4,X_inf_train_m_q4], p_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_inf_test_d_q4,X_inf_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            # rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
gru_naive = min(list_of_results, key=lambda x: x[3])

#### 10.6 GRU MIDAS

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_m()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers-1):
                x = GRU(units=nodes, activation='relu', return_sequences=True)(x)
                x = Dropout(rate=dropout_rate)(x)
            x = GRU(units=nodes, activation='relu', return_sequences=False)(x)
            x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_gdp_train_d_q4,X_gdp_train_m_q4], y_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_gdp_test_d_q4,X_gdp_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
gru_midas = min(list_of_results, key=lambda x: x[3])

#### 10.7 SA

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_mtoq_simple()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq_simple()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers):
                x = MultiHeadAttention(num_heads=8, key_dim=10)(x, x)
                x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_gdp_train_d_q4,X_gdp_train_m_q4], y_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_gdp_test_d_q4,X_gdp_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
sa_naive = min(list_of_results, key=lambda x: x[3])

#### 10.8 SA MIDAS

# List of results
list_of_results = []

# Hyperparameters
for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            Input_monthly = Input(shape=(12,5))
            Input_daily = Input(shape=(259,10))

            # Aligning inputs to all of them quarterly
            aligned_m = CustomConv1D_m()(Input_monthly) # Has converted 5 vars of 12 months to 5 vars of 4 quarters
            aligned_d = CustomConv1D_dtoq()(Input_daily) # Has converted 10 vars of 259 days, to 10 vars of 4 quarters

            # Flatenning the dataset before passing through feed forward layers
            Concat = tf.keras.layers.Concatenate(axis=-1)([aligned_m, aligned_d])

            # Creating the LSTM Layers
            x = Concat
            for _ in range(hlayers):
                x = MultiHeadAttention(num_heads=8, key_dim=10)(x, x)
                x = Dropout(rate=dropout_rate)(x)

            # Flatenning results
            x = Flatten()(x)
            x = Dense(units=nodes, activation='relu')(x)

            # Sigmoid output layer (sigmoid because target is standardized)
            Output = Dense(1, activation='sigmoid')(x)

            # Model
            model = Model([Input_daily,Input_monthly], Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit([X_gdp_train_d_q4,X_gdp_train_m_q4], y_train_q4, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict([X_gdp_test_d_q4,X_gdp_test_m_q4]))

            # Rescale depending on whether you are using GDP or inflation data
            # Also, make sure select the appropriate test variable to rescale
            rescaled_preds = preds * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_preds = preds * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = y_test_q1 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            rescaled_ytest = y_test_q4 * (max(gdp_growth_y) - min(gdp_growth_y)) + min(gdp_growth_y)
            # rescaled_ytest = p_test_q1 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)
            # rescaled_ytest = p_test_q4 * (max(inflation_data) - min(inflation_data)) + min(inflation_data)

            # RMSE
            rmse = np.sqrt(np.mean((rescaled_ytest - rescaled_preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE
sa_midas = min(list_of_results, key=lambda x: x[3])

