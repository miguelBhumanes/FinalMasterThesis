'''This script checks the stationarity of the series and implements the autoregressive models'''

### 1 IMPORTING ALL PACKAGES AND DATA

# Importing packages
import pandas as pd
import numpy as np
import random
import matplotlib.pyplot as plt
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, LSTM, Input, Flatten, Dropout
from fredapi import Fred
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import adfuller, kpss
import pmdarima as pm
from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error
import warnings
warnings.filterwarnings('ignore')

# Importing GDP and Inflation data from January 1999 to December 2022
fred = Fred(api_key="your_key_here")
real_gdp_data = fred.get_series('GDPC1')
real_gdp_data = real_gdp_data['1999':'2022']
cpi_data = fred.get_series('CPIAUCSL')
cpi_data = cpi_data['1999':'2022']

# Computing the yearly rates of growth, which are the target variables
gdp_growth_y = ((real_gdp_data - real_gdp_data.shift(4)) / real_gdp_data.shift(4)).dropna()
inflation_data = ((cpi_data - cpi_data.shift(12)) / cpi_data.shift(12)).dropna()

### 2 CHECKING STATIONARITY OF THE SERIES

# Plotting the series to check stationarity visually: 

# GDP growth
plt.figure(figsize=(10, 5))
plt.plot(gdp_growth_y)
plt.title('GDP annual growth')
plt.xlabel('Year')
plt.grid(True)
#plt.show()
plt.savefig('gdp_growth.png')

# Inflation
plt.figure(figsize=(10, 5))
plt.plot(inflation_data)
plt.title('CPI annual inflation')
plt.xlabel('Year')
plt.grid(True)
#plt.show()
plt.savefig('inflation.png')

# Plotting the ACF and PACF of the series

# GDP growth

# Plotting the Autocorrelation Function (ACF)
plt.figure(figsize=(12, 6))
plt.subplot(2, 1, 1)
plot_acf(gdp_growth_y, ax=plt.gca(), lags=20)
plt.title('Autocorrelation Function')

# Plotting the Partial Autocorrelation Function (PACF)
plt.subplot(2, 1, 2)
plot_pacf(gdp_growth_y, ax=plt.gca(), lags=20)
plt.title('Partial Autocorrelation Function')

plt.tight_layout()
plt.show()

# Inflation

# Plotting the Autocorrelation Function (ACF)
plt.figure(figsize=(12, 6))
plt.subplot(2, 1, 1)
plot_acf(inflation_data, ax=plt.gca(), lags=20)
plt.title('Autocorrelation Function')

# Plotting the Partial Autocorrelation Function (PACF)
plt.subplot(2, 1, 2)
plot_pacf(inflation_data, ax=plt.gca(), lags=20)
plt.title('Partial Autocorrelation Function')

plt.tight_layout()
plt.show()

# Checking stationarity with ADF and KPSS tests

'''Please, select the series of interest and then run the code to perform the test:

- gdp_growht_y // for GDP growth
- gdp_growth_y[1:80] // for GDP growth, excluding Covid period
- inflation_data // for inflation
- inflation_data[1:240] // for inflation, excluding Covid period
'''

series = gdp_growth_y[1:80]

# ADF test

adf_test = adfuller(series)

adf_results = pd.DataFrame({'ADF Test Statistic': adf_test[0],
                            'p-value': adf_test[1],
                            'Used Lag': adf_test[2],
                            'Number of Observations': adf_test[3],
                            'Critical Values': adf_test[4]},
                           index=['Value'])

adf_results.T # Print results of the test

# KPSS test

kpss_test = kpss(series, regression='c')

kpss_results = pd.DataFrame({'KPSS Test Statistic': kpss_test[0],
                             'p-value': kpss_test[1],
                             'Used Lag': kpss_test[2],
                             'Critical Values': kpss_test[3]},
                            index=['Value'])

kpss_results.T # Print results of the test

### 3 IDENTIFYING BEST ARIMA MODEL

# Identify the best model

'''Please, select the series of interest and then run the code to perform the test:

- gdp_growht_y // for GDP growth
- gdp_growth_y[1:80] // for GDP growth, excluding Covid period
- inflation_data // for inflation
- inflation_data[1:240] // for inflation, excluding Covid period

Remember as well to select max_p = max_q = 5 for GDP, and 13 for Inflation (1 year worth of lags)
'''

series = gdp_growth_y[1:80]

model = pm.auto_arima(series, start_p=0, start_q=0,
                      max_p=5, max_q=5,
                      d=0,
                      seasonal=False,
                      suppress_warnings=True, 
                      stepwise=True)

print(model) # Get best model fitted

### 4 TEST THE ARIMA MODELS

# Create the functions to compute the rmses of the models for each target of prediction

def rmse_arima_1qahead(data,order=(0,0,0)):
    train_size = int(len(data) * 0.8) # Training sample are first 80% of observations
    train, test = data[:train_size], data[train_size:] # Separating train and test samples
    predictions = [] # Predictions
    for t in range(len(test)):
        model = ARIMA(train, order=order) # Fit model
        model_fit = model.fit() 
        output = model_fit.forecast()
        yhat = output[0] # Get prediction for next period (1 quarter ahead)
        predictions.append(yhat) # Append prediction to list of predictors
        train = np.append(train[1:], test[t]) # Update training sample in rolling fashion. Add the newest, remove the oldest
    rmse = np.sqrt(mean_squared_error(test, predictions)) # Compute RMSE
    return rmse * 100 # Return result in percentage points

def rmse_arima_4qahead(data,order=(0,0,0)):
    train_size = int(len(data) * 0.8) # Training sample are first 80% of observations
    train, test = data[:train_size], data[train_size:] # Separating train and test samples
    q4test = test.copy()[3:] # Creating test sample for 4q ahead prediction eliminating the first 3 periods of the test sample
    predictions = [] # Predictions
    for t in range(len(q4test)):
        model = ARIMA(train, order=order) # Fit model
        model_fit = model.fit() 
        output = model_fit.forecast(steps=4) # Forecast 4q ahead
        yhat = output[3] # Get the fourth element of the next 4 predictions
        predictions.append(yhat) # Append prediction to list of predictors
        train = np.append(train[1:], test[t]) # Update training sample in rolling fashion. Add the newest, remove the oldest
    rmse = np.sqrt(mean_squared_error(q4test, predictions)) # Compute RMSE
    return rmse * 100 # Return result in percentage points

def rmse_arima_3mahead(data,order=(0,0,0)):
    train_size = int(len(data) * 0.8) # Training sample are first 80% of observations
    train, test = data[:train_size], data[train_size:] # Separating train and test samples
    m3test = test.copy()[2:] # Creating test sample for 3m ahead prediction eliminating the first 2 periods of the test sample
    predictions = [] # Predictions
    for t in range(len(m3test)):
        model = ARIMA(train, order=order) # Fit model
        model_fit = model.fit() 
        output = model_fit.forecast(steps=3) # Forecast 3m ahead
        yhat = output[2] # Get the third element of the next 3 steps predicted (1q ahead)
        predictions.append(yhat) # Append prediction to list of predictors
        train = np.append(train[1:], test[t]) # Update training sample in rolling fashion. Add the newest, remove the oldest
    rmse = np.sqrt(mean_squared_error(m3test, predictions)) # Compute RMSE
    return rmse * 100 # Return result in percentage points

def rmse_arima_12mahead(data,order=(0,0,0)):
    train_size = int(len(data) * 0.8) # Training sample are first 80% of observations
    train, test = data[:train_size], data[train_size:] # Separating train and test samples
    m12test = test.copy()[11:] # Creating test sample for 12m ahead prediction eliminating the first 11 periods of the test sample
    predictions = [] # Predictions
    for t in range(len(m12test)):
        model = ARIMA(train, order=order) # Fit model
        model_fit = model.fit() 
        output = model_fit.forecast(steps=12) # Forecast 3m ahead
        yhat = output[11] # Get the 12th element of the next 12 steps predicted (4q ahead)
        predictions.append(yhat) # Append prediction to list of predictors
        train = np.append(train[1:], test[t]) # Update training sample in rolling fashion. Add the newest, remove the oldest
    rmse = np.sqrt(mean_squared_error(m12test, predictions)) # Compute RMSE
    return rmse * 100 # Return result in percentage points

# Get results

# GDP Growth

rmse_arima_1qahead(gdp_growth_y,order=(1,0,0)) # 3.55%
rmse_arima_1qahead(gdp_growth_y,order=(5,0,0)) # 4.00%
rmse_arima_4qahead(gdp_growth_y,order=(1,0,0)) # 5.09%
rmse_arima_4qahead(gdp_growth_y,order=(5,0,0)) # 5.24%

# GDP Growth ex covid

rmse_arima_1qahead(gdp_growth_y[1:80],order=(5,0,0)) # 0.36%
rmse_arima_4qahead(gdp_growth_y[1:80],order=(5,0,0)) # 0.85%

# Inflation 

rmse_arima_3mahead(inflation_data,order=(13,0,0)) # 1.14%
rmse_arima_3mahead(inflation_data,order=(1,0,3)) # 1.21%
rmse_arima_12mahead(inflation_data,order=(13,0,0)) # 3.81%
rmse_arima_12mahead(inflation_data,order=(3,0,0)) #     

# Inflation ex covid

rmse_arima_3mahead(inflation_data[1:240],order=(13,0,0)) # 0.50%
rmse_arima_3mahead(inflation_data[1:240],order=(3,0,0)) # 0.41%
rmse_arima_12mahead(inflation_data[1:240],order=(13,0,0)) # 0.73%
rmse_arima_12mahead(inflation_data[1:240],order=(3,0,0)) # 0.42%

### 5 CREATE AND TEST THE AUTOREGRESSIVE LSTM MODEL

'''Please, follow the instructions in the model to adapt it to the different targets of prediction'''

# Settings of the model
periods_ahead = 12 # 1 for GDP 1q ahead // 4 for GDP 4q ahead // 3 for Inf 1Q ahead // 12 for Inf 4Q ahead
freq = 12 # 4 for GDP // 12 for inflation
oseries = inflation_data[1:240].copy() # gdp_growth_y ([1:80] to exclude covid) or inflation_data ([1:240] to exclude covid)
series = (oseries - min(oseries))/(max(oseries)-min(oseries)) # normalized data

# Preparing the data for the model
y = series[(freq + periods_ahead - 1):]
x = np.array([series[i-freq:i] for i in range(freq, len(series))])
x = x[:(x.shape[0] - periods_ahead + 1)]
train_size = round(len(y)*0.8)
y_train = y[:train_size]
y_test = y[train_size:]
x_train = x[:train_size]
x_train = x_train.reshape((x_train.shape[0], x_train.shape[1], 1))
x_test = x[train_size:]
x_test = x_test.reshape((x_test.shape[0], x_test.shape[1], 1))

# Callback for the model (to avoid overfitting)
early_stopping = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=100,restore_best_weights=True)
custom_optimizer = tf.keras.optimizers.legacy.Adam(learning_rate=0.0001)

# Loop that performs a grid search to optimize hyperparameters
list_of_results = []

for hlayers in [1,2,3]:
    for nodes in [10,50,100]:
        for dropout_rate in [0,0.1]:

            # Inputs
            input_layer = Input(shape=(freq,1))

            # Creating the LSTM Layers
            x = input_layer
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
            model = Model(input_layer, Output)

            # Compiling model
            model.compile(optimizer=custom_optimizer, loss='mean_squared_error') 

            # Seed
            seed = 957
            tf.random.set_seed(seed)
            np.random.seed(seed)
            random.seed(seed)

            # Model fitting
            model.fit(x_train, y_train, 
                        epochs=500, batch_size=16, validation_split=0.15,
                        callbacks=[early_stopping])

            preds = np.squeeze(model.predict(x_test))

            # Rescale 
            preds = preds * (max(oseries)-min(oseries)) + min(oseries)
            rescaledytest = y_test * (max(oseries)-min(oseries)) + min(oseries)

            # RMSE
            rmse = np.sqrt(np.mean((rescaledytest - preds)**2)) * 100
            rmse

            # Collect the parameters and the rmse achieved
            result = [hlayers, nodes, dropout_rate, rmse]
            list_of_results.append(result)
            tf.keras.backend.clear_session()

# Hyperparameters that achieved lowest MSE, and lowest MSE
best_result = min(list_of_results, key=lambda x: x[3])

# GDP 3.53 (Q1) 3.73 (Q4)
# GDP Ex Covid 0.46 (Q1) 0.64 (Q4)
# Inflation 2.64 (Q1) 3.30 (Q4)
# Inflation Ex Covid 0.47 (Q1) (Q4)


