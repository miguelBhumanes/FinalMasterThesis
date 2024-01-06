#################################################################
# MIDAS MODELS FOR US GDP PREDICTIONG
##################################################################

### README:
# This is a very extense script, testing all the DFM models for which the RMSEs have been reported
# The reason to write such an extense script is to make each of the models directly available to test
# This is preferred to using a loop to test all the models in case future researchers want to focus only on one of the DFMs

#####################################################################
### IMPORTING REQUIRED PACKAGES

library(dplyr)
library(corrplot)
library(lubridate)
library(midasr)
library(fredr)

#####################################################################
### DOWNLOADING DATA FROM FRED TO CREATE OUTCOME VARIABLE FOR GDP

# FRED key
fredr_set_key("your_key_here")

# Setting the dates to download
data <- data.frame(
  date = seq(as.Date("1998-01-01"), 
             as.Date("2023-02-28"), by="1 day"))

# List of outcome variables to download (GDP, Production side, US + CPI Inflation, urban)
names = c("GDPC1","CPIAUCSL")

# Loop that creates a dataframe with the downloaded data
for (col in names) {
  tmp <- fredr(col)
  tmp <- tmp %>%
    rename(!!sym(tolower(col)) := value) %>%
    select(date, tolower(col)) %>%
    mutate(date = as.Date(date))
  data <- data %>%
    left_join(tmp, by="date") %>%
    mutate(!!sym(tolower(col)) := ifelse(!!sym(tolower(col)) == ".", 
                                         NA, !!sym(tolower(col)))) %>%
    tibble()
}

# The previous dataframe, after selecting only GDP values, when publication available
length(na.omit(data$gdpc1)) # 101 observations, 25 years plus 1 quarter in 2023
length(na.omit(data$cpiaucsl)) # 302 observations, 25 years of 12 months, plus 2 months in 2023

# Creating the variable 
gdp1 = na.omit(data$gdpc1)
gdp1 = gdp1[1:(length(gdp1)-1)] 
cpiaucsl = na.omit(data$cpiaucsl)
cpiaucsl = cpiaucsl[1:(length(cpiaucsl)-2)] 
#Eliminating the values for 2023. Only doing until 2022 for simplicity

# Creating necessary lags: defining function that computes lag
lagp_return <- function(x,p) {
  lagged_x <- lag(x, p)
  returns <- (x - lagged_x) / lagged_x
  return(returns)
}

# Creating lags for GPD (present, 1Q lag, 2Q lag, 3Q lag, 1Y lag)
for (i in 0:4) {
  # create variable name
  var_name <- paste0("gdp1.lag", i)
  # compute variable value
  var_value <- head(tail(na.omit(lagp_return(gdp1,4)),92+i),92)
  # assign variable to environment
  assign(var_name, var_value)
} # Using 92 values, corresponding to 23 years of data we want to predict

# Creating lags for Inflation (present, 1Q lag, 2Q lag, 3Q lag, 1Y lag)
for (i in c(0,3,6,9,12)) {
  # create variable name
  var_name <- paste0("inf.lag", i)
  # compute variable value
  var_value <- head(tail(na.omit(lagp_return(cpiaucsl,12)),276+i),276)
  # assign variable to environment
  assign(var_name, var_value)
} # Using 276 values, corresponding to 23 years of data we want to predict

#####################################################################
# IMPORTING THE CREATED REGRESSORS

# List with the names of the datasets of regressors
# Financial factors includes the first 5 PCA factors for the daily financial data
# Text factors includes the first 5 PCA factors for the daily text data
# Economic factors includes the first 5 PCA factors for the monthyl economic data
# And the 4 blocks created also using PCA
names = c("financial_factors","text_factors","economic_factors")

# Importing the regressors
for (name in names) {
  # Read in the CSV file and remove the X column
  df <- select(read.csv(paste0("MIDAS/Final Factors/", name, ".csv"), header = TRUE), -X)
  
  # Convert the date column to a Date format
  df$date <- as.Date(df$date)
  
  # Assign the modified data frame to the variable with the current name
  assign(name, df)
  rm(df)
}

# Creating a consolidated dataset of regressors: dates
df = data.frame(date=rev(seq(as.Date("2000-01-01"), as.Date("2023-03-31"), by = "day")))

# Creating a consolidated dataset of regressors: column names
colnames(financial_factors) = c("date",paste0("Fin", 1:5))
colnames(text_factors) = c("date",paste0("Text", 1:5))
colnames(economic_factors)[1:6] = c("date",paste0("Econ", 1:5))

# Creating a consolidated dataset of regressors: merging factors into a single dataset
df = left_join(df,financial_factors,by="date") %>%
  left_join(text_factors,by="date") %>%
  left_join(economic_factors,by="date")

# Moving  publication date to next Monday, if published during the weekend: function
weekend.moves = function(date,price){ #lubridate::wday -> 1 corresponds to Sunday
  for (i in 1:length(date)){
    if (wday(date[i])==7 & !is.na(price[i])){
      price[i-2]=price[i]
      price[i]=NA
    } else if (wday(date[i])==1 & !is.na(price[i])) {
      price[i-1]=price[i]
      price[i]=NA
    }
  }
  return(price)
}

# Moving Sunday publication dates to next Monday
for (i in 12:dim(df)[2]){
  df[,i] = weekend.moves(df$date,df[,i])
}

# Eliminating weekend days 
df = df[as.numeric(format(df$date, "%u"))<=5,]

# Using normal sorting. Now, first entry is the oldest date
df <- df[rev(seq_len(nrow(df))), ]
df = df[-1,] #First date did not have data for text indicators

# Correlation plot: checking correlation between regressors
#corrplot(cor(select(df,-date),use="p",method = "spearman"), method = "number", type = "upper",
#tl.cex = 0.3, number.cex = 0.4)
#corrplot(cor(select(df,-date),use="p",method = "kendall"), method = "number", type = "upper",
#tl.cex = 0.3, number.cex = 0.4)
# It seems that there is some information overlap between
# some blocks and economic factors. Normal since they use very similar variables
# Model selection will determine if it is better to use manually selected factors
# (blocks) or selected by correlation automatically factors (economic factors)
# Note: used Kendall correlation. Better for time series data. With other correlation
# measure, correlations are lower.

#####################################################################
# ADAPTING THE CREATED REGRESSORS TO THE MIDAS REGRESSION FORMAT

# Observations of the data before the last used GDP (Q4 December) are selected 
df.gdp = filter(df,date<="2023-1-2") 

# Selecting the economic, financial and text variables
x_econ = na.omit(select(df.gdp, c(paste0("Econ", 1:5), # 5 economic factors and 4 blocks
                                  "block.survey","block.labor","block.real","block.price")))
x_fin = (select(df.gdp,c(paste0("Fin", 1:5)))) # 5 financial factors
x_text = (select(df.gdp,c(paste0("Text", 1:5)))) # 5 text factors

# Checking lengths and adjusting regressors (MIDAS requires exact product lengths)

length(gdp1.lag0) 
#92 quarterly values of GDP. Corresponding to the 23 years of data in 2000-2022

dim(x_econ)
#276 = 92 * 3 monthly values corresponding to 23 years of data

dim(x_fin) 
#6000 = 65.21 * 92 (65 days per quarter is 5 days per week)
# Since month length is variable (28-31) it is not exact
# Need to eliminate 20 observations to have exact correspondence between months and days
# I do it at random

dim(x_text) 
#6000 = 65.21 * 92 (65 days per quarter is 5 days per week)
#Same treatment as daily financial factors

# Saving the dataframes as csvs
monthly_vars = na.omit(select(df.gdp, c(date, paste0("Econ", 1:5), # 5 economic factors and 4 blocks
                                  "block.survey","block.labor","block.real","block.price")))
daily_vars = (select(df.gdp,c(date, paste0("Fin", 1:5),paste0("Text", 1:5)))) # 5 financial factors and 5 text
write.csv(monthly_vars, file = "monthly_vars.csv", row.names = FALSE)
write.csv(daily_vars, file = "daily_vars.csv", row.names = FALSE)


# Creating the 5 Economic Factors
var_names <- paste0("Econ", 1:5)
for (i in 1:length(var_names)) {
  assign(var_names[i], eval(parse(text = paste0("x_econ$", var_names[i]))))
}

# Creating the 5 Financial Factors (eliminating 20 observations at random as explained)
var_names <- paste0("Fin", 1:5)
for (i in 1:length(var_names)) {
  assign(var_names[i], 
         eval(parse(text = paste0("x_fin$",
                                  var_names[i],
                                  "[-sample(1:6000,20,replace=FALSE)]"))))
} 

# Creating the 5 Text Factors (eliminating 20 observations at random as explained)
var_names <- paste0("Text", 1:5)
for (i in 1:length(var_names)) {
  assign(var_names[i], 
         eval(parse(text = paste0("x_text$",
                                  var_names[i],
                                  "[-sample(1:6000,20,replace=FALSE)]"))))
} 

# Creating the 5 Economic Blocks 
block.survey = x_econ$block.survey
block.labor = x_econ$block.labor 
block.real = x_econ$block.real
block.price = x_econ$block.price

############

#### PREVIOUS COMMON ADJUSTMENTS TO THE MODELS

# Series for ahead predictions (dependent variables)

# Normal
gdp1.lag0m1 = gdp1.lag0[2:length(gdp1.lag0)]
gdp1.lag0m4 = gdp1.lag0[5:length(gdp1.lag0)]
inf.lag0m1 = inf.lag0[4:length(inf.lag0)]
inf.lag0m4 = inf.lag0[13:length(inf.lag0)]

# Excluding COVID
gdp1.lag0ex = gdp1.lag0[1:80]
inf.lag0ex = inf.lag0[1:240]
gdp1.lag0m1ex = gdp1.lag0ex[2:length(gdp1.lag0ex)]
gdp1.lag0m4ex = gdp1.lag0ex[5:length(gdp1.lag0ex)]
inf.lag0m1ex = inf.lag0ex[4:length(inf.lag0ex)]
inf.lag0m4ex = inf.lag0ex[13:length(inf.lag0ex)]

# Series for ahead predictions 

Econ1m1 = Econ1[1:(length(Econ1)-3)]
Econ1m4 = Econ1[1:(length(Econ1)-12)]
Econ2m1 = Econ2[1:(length(Econ2)-3)]
Econ2m4 = Econ2[1:(length(Econ2)-12)]
Econ3m1 = Econ3[1:(length(Econ3)-3)]
Econ3m4 = Econ3[1:(length(Econ3)-12)]
Econ4m1 = Econ4[1:(length(Econ4)-3)]
Econ4m4 = Econ4[1:(length(Econ4)-12)]
Econ5m1 = Econ5[1:(length(Econ5)-3)]
Econ5m4 = Econ5[1:(length(Econ5)-12)]
block.surveym1 = block.survey[1:(length(block.survey) - 3)]
block.surveym4 = block.survey[1:(length(block.survey) - 12)]
block.laborm1 = block.labor[1:(length(block.labor) - 3)]
block.laborm4 = block.labor[1:(length(block.labor) - 12)]
block.realm1 = block.real[1:(length(block.real) - 3)]
block.realm4 = block.real[1:(length(block.real) - 12)]
block.pricem1 = block.price[1:(length(block.price) - 3)]
block.pricem4 = block.price[1:(length(block.price) - 12)]
Fin1m1 = Fin1[1:(length(Fin1)-65)]
Fin1m4 = Fin1[1:(length(Fin1)-260)]
Fin2m1 = Fin2[1:(length(Fin2)-65)]
Fin2m4 = Fin2[1:(length(Fin2)-260)]
Fin3m1 = Fin3[1:(length(Fin3)-65)]
Fin3m4 = Fin3[1:(length(Fin3)-260)]
Fin4m1 = Fin4[1:(length(Fin4)-65)]
Fin4m4 = Fin4[1:(length(Fin4)-260)]
Fin5m1 = Fin5[1:(length(Fin5)-65)]
Fin5m4 = Fin5[1:(length(Fin5)-260)]
Txt1m1 = Text1[1:(length(Text1)-65)]
Txt1m4 = Text1[1:(length(Text1)-260)]
Txt2m1 = Text2[1:(length(Text2)-65)]
Txt2m4 = Text2[1:(length(Text2)-260)]
Txt3m1 = Text3[1:(length(Text3)-65)]
Txt3m4 = Text3[1:(length(Text3)-260)]
Txt4m1 = Text4[1:(length(Text4)-65)]
Txt4m4 = Text4[1:(length(Text4)-260)]
Txt5m1 = Text5[1:(length(Text5)-65)]
Txt5m4 = Text5[1:(length(Text5)-260)]

# Excluding Covid
Econ1ex = Econ1[1:240]
Econ2ex = Econ2[1:240]
Econ3ex = Econ3[1:240]
Econ4ex = Econ4[1:240]
Econ5ex = Econ5[1:240]
block.survey.ex = block.survey[1:240]
block.labor.ex = block.labor[1:240]
block.real.ex = block.real[1:240]
block.price.ex = block.price[1:240]

for (i in 1:5) {
  assign(paste0("Fin", i, "ex"), get(paste0("Fin", i))[1:5200])
  assign(paste0("Text", i, "ex"), get(paste0("Text", i))[1:5200])
}

for (i in 1:5) {
  assign(paste0("Econ", i, "m1ex"), get(paste0("Econ", i, "ex"))[1:(length(get(paste0("Econ", i, "ex")))-3)])
  assign(paste0("Econ", i, "m4ex"), get(paste0("Econ", i, "ex"))[1:(length(get(paste0("Econ", i, "ex")))-12)])

  assign(paste0("Fin", i, "m1ex"), get(paste0("Fin", i, "ex"))[1:(length(get(paste0("Fin", i, "ex")))-65)])
  assign(paste0("Fin", i, "m4ex"), get(paste0("Fin", i, "ex"))[1:(length(get(paste0("Fin", i, "ex")))-260)])

  assign(paste0("Text", i, "m1ex"), get(paste0("Text", i, "ex"))[1:(length(get(paste0("Text", i, "ex")))-65)])
  assign(paste0("Text", i, "m4ex"), get(paste0("Text", i, "ex"))[1:(length(get(paste0("Text", i, "ex")))-260)])
}

block.survey.exm1 = block.survey.ex[1:(length(block.survey.ex) - 3)]
block.survey.exm4 = block.survey.ex[1:(length(block.survey.ex) - 12)]
block.labor.exm1 = block.labor.ex[1:(length(block.labor.ex) - 3)]
block.labor.exm4 = block.labor.ex[1:(length(block.labor.ex) - 12)]
block.real.exm1 = block.real.ex[1:(length(block.real.ex) - 3)]
block.real.exm4 = block.real.ex[1:(length(block.real.ex) - 12)]
block.price.exm1 = block.price.ex[1:(length(block.price.ex) - 3)]
block.price.exm4 = block.price.ex[1:(length(block.price.ex) - 12)]

# Creating functions to compute the naive averages to align frequency

d_to_q = function(data){
  return(colMeans(matrix(data, nrow = 65)))
}

m_to_q = function(data){
  return(colMeans(matrix(data, nrow = 3)))
}

d_to_m_m1 = function(data){
  excess = length(data) %% 273 # Eliminating observations at random to make the frequencies ratios match
  if (excess > 0) {
    remove_indices = sample(length(data), excess) 
    data = data[-remove_indices] 
  }
  return(colMeans(matrix(data, nrow = 21)))
}

d_to_m_m1ex = function(data){
  excess = length(data) %% 237 # Eliminating observations at random to make the frequencies ratios match
  if (excess > 0) {
    remove_indices = sample(length(data), excess) 
    data = data[-remove_indices] 
  }
  return(colMeans(matrix(data, nrow = 21)))
}

d_to_m_m4ex = function(data){
  excess = length(data) %% 228 # Eliminating observations at random to make the frequencies ratios match
  if (excess > 0) {
    remove_indices = sample(length(data), excess) 
    data = data[-remove_indices] 
  }
  return(colMeans(matrix(data, nrow = 21)))
}

d_to_m_m4 = function(data){
  excess = length(data) %% 264 # Eliminating observations at random to make the frequencies ratios match
  if (excess > 0) {
    remove_indices = sample(length(data), excess) 
    data = data[-remove_indices] 
  }
  return(colMeans(matrix(data, nrow = 21)))
}

multiple273 = function(data){
  excess = length(data) %% 273
  remove_indices = sample(length(data), excess) 
  return(data[-remove_indices])
}

multiple264 = function(data){
  excess = length(data) %% 264
  remove_indices = sample(length(data), excess) 
  return(data[-remove_indices])
}

multiple237 = function(data){
  excess = length(data) %% 237
  remove_indices = sample(length(data), excess) 
  return(data[-remove_indices])
}

multiple228 = function(data){
  excess = length(data) %% 228
  remove_indices = sample(length(data), excess) 
  return(data[-remove_indices])
}

################

### Trying each of the models 

################

################
### GDP Q1 COVID E1
################

# GDP Q1 COVID E1 NB EW

set.seed(957)

X = data.frame(gdp1.lag0m1,m_to_q(Econ1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon),
  start = list(Econ1m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID E1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 COVID E1F1
################

# GDP Q1 COVID E1F1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Fin1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1F1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Fin1m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Fin1m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Fin1m1 = Fin1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID E1F1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Fin1m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Fin1m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Fin1m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Fin1m1 = Fin1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q1 COVID E1T1
################

# GDP Q1 COVID E1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Txt1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Txt1m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Txt1m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Txt1m1 = Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID E1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Txt1m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Txt1m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Txt1m1 = Txt1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 COVID E1F1T1
################

# GDP Q1 COVID E1F1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Fin1m1),
  d_to_q(Txt1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1F1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Fin1m1,65,65,nealmon)
  + fmls(Txt1m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0.0074199,-0.5350413),
    Fin1m1 = c(0,0),
    Txt1m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Fin1m1 = Fin1m1,
    Txt1m1 = Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID E1F1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
  d_to_q(Fin1m1),
  d_to_q(Txt1m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
  + fmls(Fin1m1,65,65,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Fin1m1 = c(0,0),
    Txt1m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
    Fin1m1 = Fin1m1,
    Txt1m1 = Txt1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

######################################################################
## EXCLUDING COVID
######################################################################

################
### GDP Q1 EX E1
################

# GDP Q1 EX E1 NB EW

set.seed(957)

X = data.frame(gdp1.lag0m1ex,m_to_q(Econ1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon),
  start = list(Econ1m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX E1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 EX E1F1
################

# GDP Q1 EX E1F1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Fin1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 COVID E1F1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Fin1m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Fin1m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Fin1m1ex = Fin1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX E1F1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Fin1m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Fin1m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Fin1m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Fin1m1ex = Fin1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 EX E1T1
################

# GDP Q1 EX E1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Text1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Text1m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Text1m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Text1m1ex = Text1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX E1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Text1m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Text1m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Text1m1ex = Text1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 EX E1F1T1
################

# GDP Q1 EX E1F1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Fin1m1ex),
  d_to_q(Text1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1F1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Fin1m1ex,65,65,nealmon)
  + fmls(Text1m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Fin1m1ex = c(0,0),
    Text1m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Fin1m1ex = Fin1m1ex,
    Text1m1ex = Text1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX E1F1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
  d_to_q(Fin1m1ex),
  d_to_q(Text1m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
  + fmls(Fin1m1ex,65,65,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Fin1m1ex = c(0,0),
    Text1m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Fin1m1ex = Fin1m1ex,
    Text1m1ex = Text1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

#################################################################

################################################################

### Q4

#####################################################################

################

### Trying each of the models 

################

################
### GDP Q4 COVID E1
################

# GDP Q4 COVID E1 NB EW

set.seed(957)

X = data.frame(gdp1.lag0m4,m_to_q(Econ1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon),
  start = list(Econ1m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID E1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 COVID E1F1
################

# GDP Q4 COVID E1F1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Fin1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1F1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Fin1m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Fin1m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Fin1m4 = Fin1m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID E1F1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Fin1m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Fin1m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Fin1m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Fin1m4 = Fin1m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q4 COVID E1T1
################

# GDP Q4 COVID E1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Txt1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Txt1m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Txt1m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Txt1m4 = Txt1m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID E1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Txt1m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Txt1m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Txt1m4 = Txt1m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 COVID E1F1T1
################

# GDP Q4 COVID E1F1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Fin1m4),
  d_to_q(Txt1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1F1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Fin1m4,65,65,nealmon)
  + fmls(Txt1m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0.0074199,-0.5350413),
    Fin1m4 = c(0,0),
    Txt1m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Fin1m4 = Fin1m4,
    Txt1m4 = Txt1m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID E1F1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
  d_to_q(Fin1m4),
  d_to_q(Txt1m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
  + fmls(Fin1m4,65,65,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Fin1m4 = c(0,0),
    Txt1m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
    Fin1m4 = Fin1m4,
    Txt1m4 = Txt1m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

######################################################################
## EXCLUDING COVID
######################################################################

################
### GDP Q4 EX E1
################

# GDP Q4 EX E1 NB EW

set.seed(957)

X = data.frame(gdp1.lag0m4ex,m_to_q(Econ1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon),
  start = list(Econ1m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX E1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 EX E1F1
################

# GDP Q4 EX E1F1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Fin1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 COVID E1F1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Fin1m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Fin1m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Fin1m4ex = Fin1m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX E1F1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Fin1m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Fin1m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Fin1m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Fin1m4ex = Fin1m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 EX E1T1
################

# GDP Q4 EX E1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Text1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Text1m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Text1m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Text1m4ex = Text1m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX E1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Text1m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Text1m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Text1m4ex = Text1m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 EX E1F1T1
################

# GDP Q4 EX E1F1T1 NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Fin1m4ex),
  d_to_q(Text1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1F1T1 NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Fin1m4ex,65,65,nealmon)
  + fmls(Text1m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Fin1m4ex = c(0,0),
    Text1m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Fin1m4ex = Fin1m4ex,
    Text1m4ex = Text1m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX E1F1T1 B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
  d_to_q(Fin1m4ex),
  d_to_q(Text1m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
  + fmls(Fin1m4ex,65,65,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Fin1m4ex = c(0,0),
    Text1m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Fin1m4ex = Fin1m4ex,
    Text1m4ex = Text1m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################

### Trying each of the models 

################

################
### GDP Q1 COVID E
################

# GDP Q1 COVID E NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon),
  start = list(Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID E B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID E B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 COVID EF
################

# GDP Q1 COVID EF NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Fin1m1),
d_to_q(Fin2m1),
d_to_q(Fin3m1),
d_to_q(Fin4m1),
d_to_q(Fin5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID EF NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
  + fmls(Fin1m1,65,65,nealmon)
+ fmls(Fin2m1,65,65,nealmon)
+ fmls(Fin3m1,65,65,nealmon)
+ fmls(Fin4m1,65,65,nealmon)
+ fmls(Fin5m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    Fin1m1 = c(0,0),
Fin2m1 = c(0,0),
Fin3m1 = c(0,0),
Fin4m1 = c(0,0),
Fin5m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Fin1m1 = Fin1m1,
Fin2m1 = Fin2m1,
Fin3m1 = Fin3m1,
Fin4m1 = Fin4m1,
Fin5m1 = Fin5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID EF B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Fin1m1),
d_to_q(Fin2m1),
d_to_q(Fin3m1),
d_to_q(Fin4m1),
d_to_q(Fin5m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID EF B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
+ fmls(Fin1m1,65,65,nealmon)
+ fmls(Fin2m1,65,65,nealmon)
+ fmls(Fin3m1,65,65,nealmon)
+ fmls(Fin4m1,65,65,nealmon)
+ fmls(Fin5m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    Fin1m1 = c(0,0),
Fin2m1 = c(0,0),
Fin3m1 = c(0,0),
Fin4m1 = c(0,0),
Fin5m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Fin1m1 = Fin1m1,
Fin2m1 = Fin2m1,
Fin3m1 = Fin3m1,
Fin4m1 = Fin4m1,
Fin5m1 = Fin5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q1 COVID ET
################

# GDP Q1 COVID ET NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Txt1m1),
d_to_q(Txt2m1),
d_to_q(Txt3m1),
d_to_q(Txt4m1),
d_to_q(Txt5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID ET NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
+ fmls(Txt2m1,65,65,nealmon)
+ fmls(Txt3m1,65,65,nealmon)
+ fmls(Txt4m1,65,65,nealmon)
+ fmls(Txt5m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    Txt1m1 = c(0,0),
Txt2m1 = c(0,0),
Txt3m1 = c(0,0),
Txt4m1 = c(0,0),
Txt5m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Txt1m1 = Txt1m1,
Txt2m1 = Txt2m1,
Txt3m1 = Txt3m1,
Txt4m1 = Txt4m1,
Txt5m1 = Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID ET B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Txt1m1),
d_to_q(Txt2m1),
d_to_q(Txt3m1),
d_to_q(Txt4m1),
d_to_q(Txt5m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID ET B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
+ fmls(Txt2m1,65,65,nealmon)
+ fmls(Txt3m1,65,65,nealmon)
+ fmls(Txt4m1,65,65,nealmon)
+ fmls(Txt5m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    Txt1m1 = c(0,0),
Txt2m1 = c(0,0),
Txt3m1 = c(0,0),
Txt4m1 = c(0,0),
Txt5m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Txt1m1 = Txt1m1,
Txt2m1 = Txt2m1,
Txt3m1 = Txt3m1,
Txt4m1 = Txt4m1,
Txt5m1 = Txt5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 COVID EFT
################

# GDP Q1 COVID EFT NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Fin1m1),
d_to_q(Fin2m1),
d_to_q(Fin3m1),
d_to_q(Fin4m1),
d_to_q(Fin5m1),
  d_to_q(Txt1m1),
d_to_q(Txt2m1),
d_to_q(Txt3m1),
d_to_q(Txt4m1),
d_to_q(Txt5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID EFT NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
+ fmls(Fin1m1,65,65,nealmon)
+ fmls(Fin2m1,65,65,nealmon)
+ fmls(Fin3m1,65,65,nealmon)
+ fmls(Fin4m1,65,65,nealmon)
+ fmls(Fin5m1,65,65,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
+ fmls(Txt2m1,65,65,nealmon)
+ fmls(Txt3m1,65,65,nealmon)
+ fmls(Txt4m1,65,65,nealmon)
+ fmls(Txt5m1,65,65,nealmon),
  start = list(
    Econ1m1 = c(0,0),
    Econ2m1 = c(0,0),
    Econ3m1 = c(0,0),
    Econ4m1 = c(0,0),
    Econ5m1 = c(0,0),
    Fin1m1 = c(0,0),
Fin2m1 = c(0,0),
Fin3m1 = c(0,0),
Fin4m1 = c(0,0),
Fin5m1 = c(0,0),
    Txt1m1 = c(0,0),
Txt2m1 = c(0,0),
Txt3m1 = c(0,0),
Txt4m1 = c(0,0),
Txt5m1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Fin1m1 = Fin1m1,
Fin2m1 = Fin2m1,
Fin3m1 = Fin3m1,
Fin4m1 = Fin4m1,
Fin5m1 = Fin5m1,
    Txt1m1 = Txt1m1,
Txt2m1 = Txt2m1,
Txt3m1 = Txt3m1,
Txt4m1 = Txt4m1,
Txt5m1 = Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 COVID EFT B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1,
  m_to_q(Econ1m1),
m_to_q(Econ2m1),
m_to_q(Econ3m1),
m_to_q(Econ4m1),
m_to_q(Econ5m1),
  d_to_q(Fin1m1),
d_to_q(Fin2m1),
d_to_q(Fin3m1),
d_to_q(Fin4m1),
d_to_q(Fin5m1),
  d_to_q(Txt1m1),
d_to_q(Txt2m1),
d_to_q(Txt3m1),
d_to_q(Txt4m1),
d_to_q(Txt5m1),
  m_to_q(block.surveym1),
  m_to_q(block.laborm1),
  m_to_q(block.realm1),
  m_to_q(block.pricem1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1)^2)) * 100
rmse

# GDP Q1 COVID EFT B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1 ~ 1 
  + fmls(Econ1m1,3,3,nealmon)
+ fmls(Econ2m1,3,3,nealmon)
+ fmls(Econ3m1,3,3,nealmon)
+ fmls(Econ4m1,3,3,nealmon)
+ fmls(Econ5m1,3,3,nealmon)
+ fmls(Fin1m1,65,65,nealmon)
+ fmls(Fin2m1,65,65,nealmon)
+ fmls(Fin3m1,65,65,nealmon)
+ fmls(Fin4m1,65,65,nealmon)
+ fmls(Fin5m1,65,65,nealmon)
  + fmls(Txt1m1,65,65,nealmon)
+ fmls(Txt2m1,65,65,nealmon)
+ fmls(Txt3m1,65,65,nealmon)
+ fmls(Txt4m1,65,65,nealmon)
+ fmls(Txt5m1,65,65,nealmon)
  + fmls(block.surveym1,3,3,nealmon)
  + fmls(block.laborm1,3,3,nealmon)
  + fmls(block.realm1,3,3,nealmon)
  + fmls(block.pricem1,3,3,nealmon),
  start = list(
    Econ1m1 = c(0,0),
Econ2m1 = c(0,0),
Econ3m1 = c(0,0),
Econ4m1 = c(0,0),
Econ5m1 = c(0,0),
    Fin1m1 = c(0,0),
Fin2m1 = c(0,0),
Fin3m1 = c(0,0),
Fin4m1 = c(0,0),
Fin5m1 = c(0,0),
    Txt1m1 = c(0,0),
Txt2m1 = c(0,0),
Txt3m1 = c(0,0),
Txt4m1 = c(0,0),
Txt5m1 = c(0,0),
    block.surveym1 = c(0,0),
    block.laborm1 = c(0,0),
    block.realm1 = c(0,0),
    block.pricem1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1))
ylength = length(gdp1.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1 = gdp1.lag0m1, 
    Econ1m1 = Econ1m1,
Econ2m1 = Econ2m1,
Econ3m1 = Econ3m1,
Econ4m1 = Econ4m1,
Econ5m1 = Econ5m1,
    Fin1m1 = Fin1m1,
Fin2m1 = Fin2m1,
Fin3m1 = Fin3m1,
Fin4m1 = Fin4m1,
Fin5m1 = Fin5m1,
    Txt1m1 = Txt1m1,
Txt2m1 = Txt2m1,
Txt3m1 = Txt3m1,
Txt4m1 = Txt4m1,
Txt5m1 = Txt5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

######################################################################
## EXCLUDING COVID
######################################################################

################

### Trying each of the models 

################

################
### GDP Q1 EX E
################

# GDP Q1 EX E NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon),
  start = list(Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX E B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX E B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 EX EF
################

# GDP Q1 EX EF NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Fin1m1ex),
d_to_q(Fin2m1ex),
d_to_q(Fin3m1ex),
d_to_q(Fin4m1ex),
d_to_q(Fin5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX EF NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
  + fmls(Fin1m1ex,65,65,nealmon)
+ fmls(Fin2m1ex,65,65,nealmon)
+ fmls(Fin3m1ex,65,65,nealmon)
+ fmls(Fin4m1ex,65,65,nealmon)
+ fmls(Fin5m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    Fin1m1ex = c(0,0),
Fin2m1ex = c(0,0),
Fin3m1ex = c(0,0),
Fin4m1ex = c(0,0),
Fin5m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Fin1m1ex = Fin1m1ex,
Fin2m1ex = Fin2m1ex,
Fin3m1ex = Fin3m1ex,
Fin4m1ex = Fin4m1ex,
Fin5m1ex = Fin5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX EF B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Fin1m1ex),
d_to_q(Fin2m1ex),
d_to_q(Fin3m1ex),
d_to_q(Fin4m1ex),
d_to_q(Fin5m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX EF B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
+ fmls(Fin1m1ex,65,65,nealmon)
+ fmls(Fin2m1ex,65,65,nealmon)
+ fmls(Fin3m1ex,65,65,nealmon)
+ fmls(Fin4m1ex,65,65,nealmon)
+ fmls(Fin5m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    Fin1m1ex = c(0,0),
Fin2m1ex = c(0,0),
Fin3m1ex = c(0,0),
Fin4m1ex = c(0,0),
Fin5m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Fin1m1ex = Fin1m1ex,
Fin2m1ex = Fin2m1ex,
Fin3m1ex = Fin3m1ex,
Fin4m1ex = Fin4m1ex,
Fin5m1ex = Fin5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q1 EX ET
################

# GDP Q1 EX ET NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Text1m1ex),
d_to_q(Text2m1ex),
d_to_q(Text3m1ex),
d_to_q(Text4m1ex),
d_to_q(Text5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX ET NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
+ fmls(Text2m1ex,65,65,nealmon)
+ fmls(Text3m1ex,65,65,nealmon)
+ fmls(Text4m1ex,65,65,nealmon)
+ fmls(Text5m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    Text1m1ex = c(0,0),
Text2m1ex = c(0,0),
Text3m1ex = c(0,0),
Text4m1ex = c(0,0),
Text5m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Text1m1ex = Text1m1ex,
Text2m1ex = Text2m1ex,
Text3m1ex = Text3m1ex,
Text4m1ex = Text4m1ex,
Text5m1ex = Text5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX ET B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Text1m1ex),
d_to_q(Text2m1ex),
d_to_q(Text3m1ex),
d_to_q(Text4m1ex),
d_to_q(Text5m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX ET B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
+ fmls(Text2m1ex,65,65,nealmon)
+ fmls(Text3m1ex,65,65,nealmon)
+ fmls(Text4m1ex,65,65,nealmon)
+ fmls(Text5m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    Text1m1ex = c(0,0),
Text2m1ex = c(0,0),
Text3m1ex = c(0,0),
Text4m1ex = c(0,0),
Text5m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Text1m1ex = Text1m1ex,
Text2m1ex = Text2m1ex,
Text3m1ex = Text3m1ex,
Text4m1ex = Text4m1ex,
Text5m1ex = Text5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q1 EX EFT
################

# GDP Q1 EX EFT NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Fin1m1ex),
d_to_q(Fin2m1ex),
d_to_q(Fin3m1ex),
d_to_q(Fin4m1ex),
d_to_q(Fin5m1ex),
  d_to_q(Text1m1ex),
d_to_q(Text2m1ex),
d_to_q(Text3m1ex),
d_to_q(Text4m1ex),
d_to_q(Text5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX EFT NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
+ fmls(Fin1m1ex,65,65,nealmon)
+ fmls(Fin2m1ex,65,65,nealmon)
+ fmls(Fin3m1ex,65,65,nealmon)
+ fmls(Fin4m1ex,65,65,nealmon)
+ fmls(Fin5m1ex,65,65,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
+ fmls(Text2m1ex,65,65,nealmon)
+ fmls(Text3m1ex,65,65,nealmon)
+ fmls(Text4m1ex,65,65,nealmon)
+ fmls(Text5m1ex,65,65,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
    Econ2m1ex = c(0,0),
    Econ3m1ex = c(0,0),
    Econ4m1ex = c(0,0),
    Econ5m1ex = c(0,0),
    Fin1m1ex = c(0,0),
Fin2m1ex = c(0,0),
Fin3m1ex = c(0,0),
Fin4m1ex = c(0,0),
Fin5m1ex = c(0,0),
    Text1m1ex = c(0,0),
Text2m1ex = c(0,0),
Text3m1ex = c(0,0),
Text4m1ex = c(0,0),
Text5m1ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Fin1m1ex = Fin1m1ex,
Fin2m1ex = Fin2m1ex,
Fin3m1ex = Fin3m1ex,
Fin4m1ex = Fin4m1ex,
Fin5m1ex = Fin5m1ex,
    Text1m1ex = Text1m1ex,
Text2m1ex = Text2m1ex,
Text3m1ex = Text3m1ex,
Text4m1ex = Text4m1ex,
Text5m1ex = Text5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q1 EX EFT B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m1ex,
  m_to_q(Econ1m1ex),
m_to_q(Econ2m1ex),
m_to_q(Econ3m1ex),
m_to_q(Econ4m1ex),
m_to_q(Econ5m1ex),
  d_to_q(Fin1m1ex),
d_to_q(Fin2m1ex),
d_to_q(Fin3m1ex),
d_to_q(Fin4m1ex),
d_to_q(Fin5m1ex),
  d_to_q(Text1m1ex),
d_to_q(Text2m1ex),
d_to_q(Text3m1ex),
d_to_q(Text4m1ex),
d_to_q(Text5m1ex),
  m_to_q(block.survey.exm1),
  m_to_q(block.labor.exm1),
  m_to_q(block.real.exm1),
  m_to_q(block.price.exm1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m1ex)^2)) * 100
rmse

# GDP Q1 EX EFT B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m1ex ~ 1 
  + fmls(Econ1m1ex,3,3,nealmon)
+ fmls(Econ2m1ex,3,3,nealmon)
+ fmls(Econ3m1ex,3,3,nealmon)
+ fmls(Econ4m1ex,3,3,nealmon)
+ fmls(Econ5m1ex,3,3,nealmon)
+ fmls(Fin1m1ex,65,65,nealmon)
+ fmls(Fin2m1ex,65,65,nealmon)
+ fmls(Fin3m1ex,65,65,nealmon)
+ fmls(Fin4m1ex,65,65,nealmon)
+ fmls(Fin5m1ex,65,65,nealmon)
  + fmls(Text1m1ex,65,65,nealmon)
+ fmls(Text2m1ex,65,65,nealmon)
+ fmls(Text3m1ex,65,65,nealmon)
+ fmls(Text4m1ex,65,65,nealmon)
+ fmls(Text5m1ex,65,65,nealmon)
  + fmls(block.survey.exm1,3,3,nealmon)
  + fmls(block.labor.exm1,3,3,nealmon)
  + fmls(block.real.exm1,3,3,nealmon)
  + fmls(block.price.exm1,3,3,nealmon),
  start = list(
    Econ1m1ex = c(0,0),
Econ2m1ex = c(0,0),
Econ3m1ex = c(0,0),
Econ4m1ex = c(0,0),
Econ5m1ex = c(0,0),
    Fin1m1ex = c(0,0),
Fin2m1ex = c(0,0),
Fin3m1ex = c(0,0),
Fin4m1ex = c(0,0),
Fin5m1ex = c(0,0),
    Text1m1ex = c(0,0),
Text2m1ex = c(0,0),
Text3m1ex = c(0,0),
Text4m1ex = c(0,0),
Text5m1ex = c(0,0),
    block.survey.exm1 = c(0,0),
    block.labor.exm1 = c(0,0),
    block.real.exm1 = c(0,0),
    block.price.exm1 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m1ex))
ylength = length(gdp1.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m1ex = gdp1.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
Econ2m1ex = Econ2m1ex,
Econ3m1ex = Econ3m1ex,
Econ4m1ex = Econ4m1ex,
Econ5m1ex = Econ5m1ex,
    Fin1m1ex = Fin1m1ex,
Fin2m1ex = Fin2m1ex,
Fin3m1ex = Fin3m1ex,
Fin4m1ex = Fin4m1ex,
Fin5m1ex = Fin5m1ex,
    Text1m1ex = Text1m1ex,
Text2m1ex = Text2m1ex,
Text3m1ex = Text3m1ex,
Text4m1ex = Text4m1ex,
Text5m1ex = Text5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################

### Trying each of the models 

################

################
### GDP Q4 COVID E
################

# GDP Q4 COVID E NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon),
  start = list(Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID E B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID E B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 COVID EF
################

# GDP Q4 COVID EF NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Fin1m4),
d_to_q(Fin2m4),
d_to_q(Fin3m4),
d_to_q(Fin4m4),
d_to_q(Fin5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID EF NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
  + fmls(Fin1m4,65,65,nealmon)
+ fmls(Fin2m4,65,65,nealmon)
+ fmls(Fin3m4,65,65,nealmon)
+ fmls(Fin4m4,65,65,nealmon)
+ fmls(Fin5m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    Fin1m4 = c(0,0),
Fin2m4 = c(0,0),
Fin3m4 = c(0,0),
Fin4m4 = c(0,0),
Fin5m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Fin1m4 = Fin1m4,
Fin2m4 = Fin2m4,
Fin3m4 = Fin3m4,
Fin4m4 = Fin4m4,
Fin5m4 = Fin5m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID EF B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Fin1m4),
d_to_q(Fin2m4),
d_to_q(Fin3m4),
d_to_q(Fin4m4),
d_to_q(Fin5m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID EF B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
+ fmls(Fin1m4,65,65,nealmon)
+ fmls(Fin2m4,65,65,nealmon)
+ fmls(Fin3m4,65,65,nealmon)
+ fmls(Fin4m4,65,65,nealmon)
+ fmls(Fin5m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    Fin1m4 = c(0,0),
Fin2m4 = c(0,0),
Fin3m4 = c(0,0),
Fin4m4 = c(0,0),
Fin5m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Fin1m4 = Fin1m4,
Fin2m4 = Fin2m4,
Fin3m4 = Fin3m4,
Fin4m4 = Fin4m4,
Fin5m4 = Fin5m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q4 COVID ET
################

# GDP Q4 COVID ET NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Txt1m4),
d_to_q(Txt2m4),
d_to_q(Txt3m4),
d_to_q(Txt4m4),
d_to_q(Txt5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID ET NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
+ fmls(Txt2m4,65,65,nealmon)
+ fmls(Txt3m4,65,65,nealmon)
+ fmls(Txt4m4,65,65,nealmon)
+ fmls(Txt5m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    Txt1m4 = c(0,0),
Txt2m4 = c(0,0),
Txt3m4 = c(0,0),
Txt4m4 = c(0,0),
Txt5m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Txt1m4 = Txt1m4,
Txt2m4 = Txt2m4,
Txt3m4 = Txt3m4,
Txt4m4 = Txt4m4,
Txt5m4 = Txt5m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID ET B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Txt1m4),
d_to_q(Txt2m4),
d_to_q(Txt3m4),
d_to_q(Txt4m4),
d_to_q(Txt5m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID ET B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
+ fmls(Txt2m4,65,65,nealmon)
+ fmls(Txt3m4,65,65,nealmon)
+ fmls(Txt4m4,65,65,nealmon)
+ fmls(Txt5m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    Txt1m4 = c(0,0),
Txt2m4 = c(0,0),
Txt3m4 = c(0,0),
Txt4m4 = c(0,0),
Txt5m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Txt1m4 = Txt1m4,
Txt2m4 = Txt2m4,
Txt3m4 = Txt3m4,
Txt4m4 = Txt4m4,
Txt5m4 = Txt5m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 COVID EFT
################

# GDP Q4 COVID EFT NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Fin1m4),
d_to_q(Fin2m4),
d_to_q(Fin3m4),
d_to_q(Fin4m4),
d_to_q(Fin5m4),
  d_to_q(Txt1m4),
d_to_q(Txt2m4),
d_to_q(Txt3m4),
d_to_q(Txt4m4),
d_to_q(Txt5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID EFT NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
+ fmls(Fin1m4,65,65,nealmon)
+ fmls(Fin2m4,65,65,nealmon)
+ fmls(Fin3m4,65,65,nealmon)
+ fmls(Fin4m4,65,65,nealmon)
+ fmls(Fin5m4,65,65,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
+ fmls(Txt2m4,65,65,nealmon)
+ fmls(Txt3m4,65,65,nealmon)
+ fmls(Txt4m4,65,65,nealmon)
+ fmls(Txt5m4,65,65,nealmon),
  start = list(
    Econ1m4 = c(0,0),
    Econ2m4 = c(0,0),
    Econ3m4 = c(0,0),
    Econ4m4 = c(0,0),
    Econ5m4 = c(0,0),
    Fin1m4 = c(0,0),
Fin2m4 = c(0,0),
Fin3m4 = c(0,0),
Fin4m4 = c(0,0),
Fin5m4 = c(0,0),
    Txt1m4 = c(0,0),
Txt2m4 = c(0,0),
Txt3m4 = c(0,0),
Txt4m4 = c(0,0),
Txt5m4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Fin1m4 = Fin1m4,
Fin2m4 = Fin2m4,
Fin3m4 = Fin3m4,
Fin4m4 = Fin4m4,
Fin5m4 = Fin5m4,
    Txt1m4 = Txt1m4,
Txt2m4 = Txt2m4,
Txt3m4 = Txt3m4,
Txt4m4 = Txt4m4,
Txt5m4 = Txt5m4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 COVID EFT B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4,
  m_to_q(Econ1m4),
m_to_q(Econ2m4),
m_to_q(Econ3m4),
m_to_q(Econ4m4),
m_to_q(Econ5m4),
  d_to_q(Fin1m4),
d_to_q(Fin2m4),
d_to_q(Fin3m4),
d_to_q(Fin4m4),
d_to_q(Fin5m4),
  d_to_q(Txt1m4),
d_to_q(Txt2m4),
d_to_q(Txt3m4),
d_to_q(Txt4m4),
d_to_q(Txt5m4),
  m_to_q(block.surveym4),
  m_to_q(block.laborm4),
  m_to_q(block.realm4),
  m_to_q(block.pricem4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4)^2)) * 100
rmse

# GDP Q4 COVID EFT B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4 ~ 1 
  + fmls(Econ1m4,3,3,nealmon)
+ fmls(Econ2m4,3,3,nealmon)
+ fmls(Econ3m4,3,3,nealmon)
+ fmls(Econ4m4,3,3,nealmon)
+ fmls(Econ5m4,3,3,nealmon)
+ fmls(Fin1m4,65,65,nealmon)
+ fmls(Fin2m4,65,65,nealmon)
+ fmls(Fin3m4,65,65,nealmon)
+ fmls(Fin4m4,65,65,nealmon)
+ fmls(Fin5m4,65,65,nealmon)
  + fmls(Txt1m4,65,65,nealmon)
+ fmls(Txt2m4,65,65,nealmon)
+ fmls(Txt3m4,65,65,nealmon)
+ fmls(Txt4m4,65,65,nealmon)
+ fmls(Txt5m4,65,65,nealmon)
  + fmls(block.surveym4,3,3,nealmon)
  + fmls(block.laborm4,3,3,nealmon)
  + fmls(block.realm4,3,3,nealmon)
  + fmls(block.pricem4,3,3,nealmon),
  start = list(
    Econ1m4 = c(0,0),
Econ2m4 = c(0,0),
Econ3m4 = c(0,0),
Econ4m4 = c(0,0),
Econ5m4 = c(0,0),
    Fin1m4 = c(0,0),
Fin2m4 = c(0,0),
Fin3m4 = c(0,0),
Fin4m4 = c(0,0),
Fin5m4 = c(0,0),
    Txt1m4 = c(0,0),
Txt2m4 = c(0,0),
Txt3m4 = c(0,0),
Txt4m4 = c(0,0),
Txt5m4 = c(0,0),
    block.surveym4 = c(0,0),
    block.laborm4 = c(0,0),
    block.realm4 = c(0,0),
    block.pricem4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4))
ylength = length(gdp1.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4 = gdp1.lag0m4, 
    Econ1m4 = Econ1m4,
Econ2m4 = Econ2m4,
Econ3m4 = Econ3m4,
Econ4m4 = Econ4m4,
Econ5m4 = Econ5m4,
    Fin1m4 = Fin1m4,
Fin2m4 = Fin2m4,
Fin3m4 = Fin3m4,
Fin4m4 = Fin4m4,
Fin5m4 = Fin5m4,
    Txt1m4 = Txt1m4,
Txt2m4 = Txt2m4,
Txt3m4 = Txt3m4,
Txt4m4 = Txt4m4,
Txt5m4 = Txt5m4,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

######################################################################
## EXCLUDING COVID
######################################################################

################

### Trying each of the models 

################

################
### GDP Q4 EX E
################

# GDP Q4 EX E NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon),
  start = list(Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX E B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX E B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 EX EF
################

# GDP Q4 EX EF NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Fin1m4ex),
d_to_q(Fin2m4ex),
d_to_q(Fin3m4ex),
d_to_q(Fin4m4ex),
d_to_q(Fin5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX EF NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
  + fmls(Fin1m4ex,65,65,nealmon)
+ fmls(Fin2m4ex,65,65,nealmon)
+ fmls(Fin3m4ex,65,65,nealmon)
+ fmls(Fin4m4ex,65,65,nealmon)
+ fmls(Fin5m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    Fin1m4ex = c(0,0),
Fin2m4ex = c(0,0),
Fin3m4ex = c(0,0),
Fin4m4ex = c(0,0),
Fin5m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Fin1m4ex = Fin1m4ex,
Fin2m4ex = Fin2m4ex,
Fin3m4ex = Fin3m4ex,
Fin4m4ex = Fin4m4ex,
Fin5m4ex = Fin5m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX EF B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Fin1m4ex),
d_to_q(Fin2m4ex),
d_to_q(Fin3m4ex),
d_to_q(Fin4m4ex),
d_to_q(Fin5m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX EF B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
+ fmls(Fin1m4ex,65,65,nealmon)
+ fmls(Fin2m4ex,65,65,nealmon)
+ fmls(Fin3m4ex,65,65,nealmon)
+ fmls(Fin4m4ex,65,65,nealmon)
+ fmls(Fin5m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    Fin1m4ex = c(0,0),
Fin2m4ex = c(0,0),
Fin3m4ex = c(0,0),
Fin4m4ex = c(0,0),
Fin5m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Fin1m4ex = Fin1m4ex,
Fin2m4ex = Fin2m4ex,
Fin3m4ex = Fin3m4ex,
Fin4m4ex = Fin4m4ex,
Fin5m4ex = Fin5m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


################
### GDP Q4 EX ET
################

# GDP Q4 EX ET NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Text1m4ex),
d_to_q(Text2m4ex),
d_to_q(Text3m4ex),
d_to_q(Text4m4ex),
d_to_q(Text5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX ET NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
+ fmls(Text2m4ex,65,65,nealmon)
+ fmls(Text3m4ex,65,65,nealmon)
+ fmls(Text4m4ex,65,65,nealmon)
+ fmls(Text5m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    Text1m4ex = c(0,0),
Text2m4ex = c(0,0),
Text3m4ex = c(0,0),
Text4m4ex = c(0,0),
Text5m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Text1m4ex = Text1m4ex,
Text2m4ex = Text2m4ex,
Text3m4ex = Text3m4ex,
Text4m4ex = Text4m4ex,
Text5m4ex = Text5m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX ET B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Text1m4ex),
d_to_q(Text2m4ex),
d_to_q(Text3m4ex),
d_to_q(Text4m4ex),
d_to_q(Text5m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX ET B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
+ fmls(Text2m4ex,65,65,nealmon)
+ fmls(Text3m4ex,65,65,nealmon)
+ fmls(Text4m4ex,65,65,nealmon)
+ fmls(Text5m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    Text1m4ex = c(0,0),
Text2m4ex = c(0,0),
Text3m4ex = c(0,0),
Text4m4ex = c(0,0),
Text5m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Text1m4ex = Text1m4ex,
Text2m4ex = Text2m4ex,
Text3m4ex = Text3m4ex,
Text4m4ex = Text4m4ex,
Text5m4ex = Text5m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### GDP Q4 EX EFT
################

# GDP Q4 EX EFT NB EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Fin1m4ex),
d_to_q(Fin2m4ex),
d_to_q(Fin3m4ex),
d_to_q(Fin4m4ex),
d_to_q(Fin5m4ex),
  d_to_q(Text1m4ex),
d_to_q(Text2m4ex),
d_to_q(Text3m4ex),
d_to_q(Text4m4ex),
d_to_q(Text5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX EFT NB MIDAS

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
+ fmls(Fin1m4ex,65,65,nealmon)
+ fmls(Fin2m4ex,65,65,nealmon)
+ fmls(Fin3m4ex,65,65,nealmon)
+ fmls(Fin4m4ex,65,65,nealmon)
+ fmls(Fin5m4ex,65,65,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
+ fmls(Text2m4ex,65,65,nealmon)
+ fmls(Text3m4ex,65,65,nealmon)
+ fmls(Text4m4ex,65,65,nealmon)
+ fmls(Text5m4ex,65,65,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
    Econ2m4ex = c(0,0),
    Econ3m4ex = c(0,0),
    Econ4m4ex = c(0,0),
    Econ5m4ex = c(0,0),
    Fin1m4ex = c(0,0),
Fin2m4ex = c(0,0),
Fin3m4ex = c(0,0),
Fin4m4ex = c(0,0),
Fin5m4ex = c(0,0),
    Text1m4ex = c(0,0),
Text2m4ex = c(0,0),
Text3m4ex = c(0,0),
Text4m4ex = c(0,0),
Text5m4ex = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Fin1m4ex = Fin1m4ex,
Fin2m4ex = Fin2m4ex,
Fin3m4ex = Fin3m4ex,
Fin4m4ex = Fin4m4ex,
Fin5m4ex = Fin5m4ex,
    Text1m4ex = Text1m4ex,
Text2m4ex = Text2m4ex,
Text3m4ex = Text3m4ex,
Text4m4ex = Text4m4ex,
Text5m4ex = Text5m4ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# GDP Q4 EX EFT B EW

set.seed(957)

X = data.frame(
  gdp1.lag0m4ex,
  m_to_q(Econ1m4ex),
m_to_q(Econ2m4ex),
m_to_q(Econ3m4ex),
m_to_q(Econ4m4ex),
m_to_q(Econ5m4ex),
  d_to_q(Fin1m4ex),
d_to_q(Fin2m4ex),
d_to_q(Fin3m4ex),
d_to_q(Fin4m4ex),
d_to_q(Fin5m4ex),
  d_to_q(Text1m4ex),
d_to_q(Text2m4ex),
d_to_q(Text3m4ex),
d_to_q(Text4m4ex),
d_to_q(Text5m4ex),
  m_to_q(block.survey.exm4),
  m_to_q(block.labor.exm4),
  m_to_q(block.real.exm4),
  m_to_q(block.price.exm4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(gdp1.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$gdp1.lag0m4ex)^2)) * 100
rmse

# GDP Q4 EX EFT B MIDAS 

set.seed(957)
model = midas_r(
  gdp1.lag0m4ex ~ 1 
  + fmls(Econ1m4ex,3,3,nealmon)
+ fmls(Econ2m4ex,3,3,nealmon)
+ fmls(Econ3m4ex,3,3,nealmon)
+ fmls(Econ4m4ex,3,3,nealmon)
+ fmls(Econ5m4ex,3,3,nealmon)
+ fmls(Fin1m4ex,65,65,nealmon)
+ fmls(Fin2m4ex,65,65,nealmon)
+ fmls(Fin3m4ex,65,65,nealmon)
+ fmls(Fin4m4ex,65,65,nealmon)
+ fmls(Fin5m4ex,65,65,nealmon)
  + fmls(Text1m4ex,65,65,nealmon)
+ fmls(Text2m4ex,65,65,nealmon)
+ fmls(Text3m4ex,65,65,nealmon)
+ fmls(Text4m4ex,65,65,nealmon)
+ fmls(Text5m4ex,65,65,nealmon)
  + fmls(block.survey.exm4,3,3,nealmon)
  + fmls(block.labor.exm4,3,3,nealmon)
  + fmls(block.real.exm4,3,3,nealmon)
  + fmls(block.price.exm4,3,3,nealmon),
  start = list(
    Econ1m4ex = c(0,0),
Econ2m4ex = c(0,0),
Econ3m4ex = c(0,0),
Econ4m4ex = c(0,0),
Econ5m4ex = c(0,0),
    Fin1m4ex = c(0,0),
Fin2m4ex = c(0,0),
Fin3m4ex = c(0,0),
Fin4m4ex = c(0,0),
Fin5m4ex = c(0,0),
    Text1m4ex = c(0,0),
Text2m4ex = c(0,0),
Text3m4ex = c(0,0),
Text4m4ex = c(0,0),
Text5m4ex = c(0,0),
    block.survey.exm4 = c(0,0),
    block.labor.exm4 = c(0,0),
    block.real.exm4 = c(0,0),
    block.price.exm4 = c(0,0))
)

index_cutoff = floor(0.8 * length(gdp1.lag0m4ex))
ylength = length(gdp1.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    gdp1.lag0m4ex = gdp1.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
Econ2m4ex = Econ2m4ex,
Econ3m4ex = Econ3m4ex,
Econ4m4ex = Econ4m4ex,
Econ5m4ex = Econ5m4ex,
    Fin1m4ex = Fin1m4ex,
Fin2m4ex = Fin2m4ex,
Fin3m4ex = Fin3m4ex,
Fin4m4ex = Fin4m4ex,
Fin5m4ex = Fin5m4ex,
    Text1m4ex = Text1m4ex,
Text2m4ex = Text2m4ex,
Text3m4ex = Text3m4ex,
Text4m4ex = Text4m4ex,
Text5m4ex = Text5m4ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################

### Trying each of the models 

################

################

### Adjusting some variables

adj1Fin1m1 = multiple273(Fin1m1)
adj1Fin2m1 = multiple273(Fin2m1)
adj1Fin3m1 = multiple273(Fin3m1)
adj1Fin4m1 = multiple273(Fin4m1)
adj1Fin5m1 = multiple273(Fin5m1)

adj4Fin1m1 = multiple264(Fin1m4)
adj4Fin2m1 = multiple264(Fin2m4)
adj4Fin3m1 = multiple264(Fin3m4)
adj4Fin4m1 = multiple264(Fin4m4)
adj4Fin5m1 = multiple264(Fin5m4)

adj1Txt1m1 = multiple273(Txt1m1)
adj1Txt2m1 = multiple273(Txt2m1)
adj1Txt3m1 = multiple273(Txt3m1)
adj1Txt4m1 = multiple273(Txt4m1)
adj1Txt5m1 = multiple273(Txt5m1)

adj4Txt1m1 = multiple264(Txt1m4)
adj4Txt2m1 = multiple264(Txt2m4)
adj4Txt3m1 = multiple264(Txt3m4)
adj4Txt4m1 = multiple264(Txt4m4)
adj4Txt5m1 = multiple264(Txt5m4)

adj1Fin1m1ex = multiple237(Fin1m1ex)
adj1Fin2m1ex = multiple237(Fin2m1ex)
adj1Fin3m1ex = multiple237(Fin3m1ex)
adj1Fin4m1ex = multiple237(Fin4m1ex)
adj1Fin5m1ex = multiple237(Fin5m1ex)

adj4Fin1m1ex = multiple228(Fin1m4ex)
adj4Fin2m1ex = multiple228(Fin2m4ex)
adj4Fin3m1ex = multiple228(Fin3m4ex)
adj4Fin4m1ex = multiple228(Fin4m4ex)
adj4Fin5m1ex = multiple228(Fin5m4ex)

adj1Txt1m1ex = multiple237(Text1m1ex)
adj1Txt2m1ex = multiple237(Text2m1ex)
adj1Txt3m1ex = multiple237(Text3m1ex)
adj1Txt4m1ex = multiple237(Text4m1ex)
adj1Txt5m1ex = multiple237(Text5m1ex)

adj4Txt1m1ex = multiple228(Text1m4ex)
adj4Txt2m1ex = multiple228(Text2m4ex)
adj4Txt3m1ex = multiple228(Text3m4ex)
adj4Txt4m1ex = multiple228(Text4m4ex)
adj4Txt5m1ex = multiple228(Text5m4ex)

################

################
### INF Q1 COVID E1
################

# INF Q1 COVID E1 NB EW

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1 B EW

set.seed(957)

X = data.frame(
  inf.lag0m1,
  Econ1m1,
  block.surveym1,
  block.laborm1,
  block.realm1,
  block.pricem1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1 B MIDAS (same results)

# INF Q1 COVID E1F1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,d_to_m_m1(Fin1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1F1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Fin1m1,21,21,nealmon),
  start = list(
    adj1Fin1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Fin1m1 = adj1Fin1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID E1F1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
    d_to_m_m1(Fin1m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Fin1m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Fin1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Fin1m1 = adj1Fin1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID E1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,d_to_m_m1(Txt1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Txt1m1,21,21,nealmon),
  start = list(
    adj1Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Txt1m1 = adj1Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID E1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
    d_to_m_m1(Txt1m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Txt1m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Txt1m1 = adj1Txt1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q1 COVID E1F1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,d_to_m_m1(Fin1m1),d_to_m_m1(Txt1m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1F1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Fin1m1,21,21,nealmon)
  + fmls(adj1Txt1m1,21,21,nealmon),
  start = list(
    adj1Fin1m1 = c(0,0),
    adj1Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Fin1m1 = adj1Fin1m1,
    adj1Txt1m1 = adj1Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID E1F1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
    d_to_m_m1(Fin1m1),
    d_to_m_m1(Txt1m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
  + fmls(adj1Fin1m1,21,21,nealmon)
  + fmls(adj1Txt1m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Fin1m1 = c(0,0),
    adj1Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    adj1Fin1m1 = adj1Fin1m1,
    adj1Txt1m1 = adj1Txt1m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q1 EX E1
################

# INF Q1 EX E1 NB EW

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1 B EW

set.seed(957)

X = data.frame(
  inf.lag0m1ex,
  Econ1m1ex,
  block.survey.exm1,
  block.labor.exm1,
  block.real.exm1,
  block.price.exm1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1 B MIDAS (same results)

# INF Q1 EX E1F1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,d_to_m_m1ex(Fin1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1F1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon),
  start = list(
    adj1Fin1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX E1F1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
    d_to_m_m1ex(Fin1m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Fin1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX E1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,d_to_m_m1ex(Text1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Txt1m1ex,21,21,nealmon),
  start = list(
    adj1Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX E1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
    d_to_m_m1ex(Text1m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Txt1m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q1 EX E1F1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,d_to_m_m1ex(Fin1m1ex),d_to_m_m1ex(Text1m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1F1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
  + fmls(adj1Txt1m1ex,21,21,nealmon),
  start = list(
    adj1Fin1m1ex = c(0,0),
    adj1Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX E1F1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
    d_to_m_m1ex(Fin1m1ex),
    d_to_m_m1ex(Text1m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
  + fmls(adj1Txt1m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Fin1m1ex = c(0,0),
    adj1Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q1 COVID E
################

# INF Q1 COVID E NB EW

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E B EW

set.seed(957)

X = data.frame(
  inf.lag0m1,
  Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,
  block.surveym1,
  block.laborm1,
  block.realm1,
  block.pricem1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID E B MIDAS (same results)

# INF Q1 COVID EF NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,d_to_m_m1(Fin1m1),
d_to_m_m1(Fin2m1),
d_to_m_m1(Fin3m1),
d_to_m_m1(Fin4m1),
d_to_m_m1(Fin5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID EF NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Fin1m1,21,21,nealmon)
+ fmls(adj1Fin2m1,21,21,nealmon)
+ fmls(adj1Fin3m1,21,21,nealmon)
+ fmls(adj1Fin4m1,21,21,nealmon)
+ fmls(adj1Fin5m1,21,21,nealmon),
  start = list(
    adj1Fin1m1 = c(0,0),
adj1Fin2m1 = c(0,0),
adj1Fin3m1 = c(0,0),
adj1Fin4m1 = c(0,0),
adj1Fin5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Fin1m1 = adj1Fin1m1,
adj1Fin2m1 = adj1Fin2m1,
adj1Fin3m1 = adj1Fin3m1,
adj1Fin4m1 = adj1Fin4m1,
adj1Fin5m1 = adj1Fin5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID EF B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,
    d_to_m_m1(Fin1m1),
d_to_m_m1(Fin2m1),
d_to_m_m1(Fin3m1),
d_to_m_m1(Fin4m1),
d_to_m_m1(Fin5m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID EF B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Fin1m1,21,21,nealmon)
+ fmls(adj1Fin2m1,21,21,nealmon)
+ fmls(adj1Fin3m1,21,21,nealmon)
+ fmls(adj1Fin4m1,21,21,nealmon)
+ fmls(adj1Fin5m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Fin1m1 = c(0,0),
adj1Fin2m1 = c(0,0),
adj1Fin3m1 = c(0,0),
adj1Fin4m1 = c(0,0),
adj1Fin5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Fin1m1 = adj1Fin1m1,
adj1Fin2m1 = adj1Fin2m1,
adj1Fin3m1 = adj1Fin3m1,
adj1Fin4m1 = adj1Fin4m1,
adj1Fin5m1 = adj1Fin5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID ET NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,d_to_m_m1(Txt1m1),
d_to_m_m1(Txt2m1),
d_to_m_m1(Txt3m1),
d_to_m_m1(Txt4m1),
d_to_m_m1(Txt5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID ET NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Txt1m1,21,21,nealmon)
+ fmls(adj1Txt2m1,21,21,nealmon)
+ fmls(adj1Txt3m1,21,21,nealmon)
+ fmls(adj1Txt4m1,21,21,nealmon)
+ fmls(adj1Txt5m1,21,21,nealmon),
  start = list(
    adj1Txt1m1 = c(0,0),
adj1Txt2m1 = c(0,0),
adj1Txt3m1 = c(0,0),
adj1Txt4m1 = c(0,0),
adj1Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Txt1m1 = adj1Txt1m1,
adj1Txt2m1 = adj1Txt2m1,
adj1Txt3m1 = adj1Txt3m1,
adj1Txt4m1 = adj1Txt4m1,
adj1Txt5m1 = adj1Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID ET B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,
    d_to_m_m1(Txt1m1),
d_to_m_m1(Txt2m1),
d_to_m_m1(Txt3m1),
d_to_m_m1(Txt4m1),
d_to_m_m1(Txt5m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID ET B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Txt1m1,21,21,nealmon)
+ fmls(adj1Txt2m1,21,21,nealmon)
+ fmls(adj1Txt3m1,21,21,nealmon)
+ fmls(adj1Txt4m1,21,21,nealmon)
+ fmls(adj1Txt5m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Txt1m1 = c(0,0),
adj1Txt2m1 = c(0,0),
adj1Txt3m1 = c(0,0),
adj1Txt4m1 = c(0,0),
adj1Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Txt1m1 = adj1Txt1m1,
adj1Txt2m1 = adj1Txt2m1,
adj1Txt3m1 = adj1Txt3m1,
adj1Txt4m1 = adj1Txt4m1,
adj1Txt5m1 = adj1Txt5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q1 COVID EFT NB EW 

set.seed(957)

X = data.frame(inf.lag0m1,Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,d_to_m_m1(Fin1m1),
d_to_m_m1(Fin2m1),
d_to_m_m1(Fin3m1),
d_to_m_m1(Fin4m1),
d_to_m_m1(Fin5m1),d_to_m_m1(Txt1m1),
d_to_m_m1(Txt2m1),
d_to_m_m1(Txt3m1),
d_to_m_m1(Txt4m1),
d_to_m_m1(Txt5m1))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID EFT NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Fin1m1,21,21,nealmon)
+ fmls(adj1Fin2m1,21,21,nealmon)
+ fmls(adj1Fin3m1,21,21,nealmon)
+ fmls(adj1Fin4m1,21,21,nealmon)
+ fmls(adj1Fin5m1,21,21,nealmon)
  + fmls(adj1Txt1m1,21,21,nealmon)
+ fmls(adj1Txt2m1,21,21,nealmon)
+ fmls(adj1Txt3m1,21,21,nealmon)
+ fmls(adj1Txt4m1,21,21,nealmon)
+ fmls(adj1Txt5m1,21,21,nealmon),
  start = list(
    adj1Fin1m1 = c(0,0),
adj1Fin2m1 = c(0,0),
adj1Fin3m1 = c(0,0),
adj1Fin4m1 = c(0,0),
adj1Fin5m1 = c(0,0),
    adj1Txt1m1 = c(0,0),
adj1Txt2m1 = c(0,0),
adj1Txt3m1 = c(0,0),
adj1Txt4m1 = c(0,0),
adj1Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Fin1m1 = adj1Fin1m1,
adj1Fin2m1 = adj1Fin2m1,
adj1Fin3m1 = adj1Fin3m1,
adj1Fin4m1 = adj1Fin4m1,
adj1Fin5m1 = adj1Fin5m1,
    adj1Txt1m1 = adj1Txt1m1,
adj1Txt2m1 = adj1Txt2m1,
adj1Txt3m1 = adj1Txt3m1,
adj1Txt4m1 = adj1Txt4m1,
adj1Txt5m1 = adj1Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 COVID EFT B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1,
    Econ1m1,
Econ2m1,
Econ3m1,
Econ4m1,
Econ5m1,
    d_to_m_m1(Fin1m1),
d_to_m_m1(Fin2m1),
d_to_m_m1(Fin3m1),
d_to_m_m1(Fin4m1),
d_to_m_m1(Fin5m1),
    d_to_m_m1(Txt1m1),
d_to_m_m1(Txt2m1),
d_to_m_m1(Txt3m1),
d_to_m_m1(Txt4m1),
d_to_m_m1(Txt5m1),
    block.surveym1, 
    block.laborm1,
    block.realm1,
    block.pricem1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1)^2)) * 100
rmse

# INF Q1 COVID EFT B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1 ~ 1 
  + Econ1m1
+ Econ2m1
+ Econ3m1
+ Econ4m1
+ Econ5m1
  + fmls(adj1Fin1m1,21,21,nealmon)
+ fmls(adj1Fin2m1,21,21,nealmon)
+ fmls(adj1Fin3m1,21,21,nealmon)
+ fmls(adj1Fin4m1,21,21,nealmon)
+ fmls(adj1Fin5m1,21,21,nealmon)
  + fmls(adj1Txt1m1,21,21,nealmon)
+ fmls(adj1Txt2m1,21,21,nealmon)
+ fmls(adj1Txt3m1,21,21,nealmon)
+ fmls(adj1Txt4m1,21,21,nealmon)
+ fmls(adj1Txt5m1,21,21,nealmon)
  + block.surveym1
  + block.laborm1
  + block.realm1
  + block.pricem1
  ,
  start = list(
    adj1Fin1m1 = c(0,0),
adj1Fin2m1 = c(0,0),
adj1Fin3m1 = c(0,0),
adj1Fin4m1 = c(0,0),
adj1Fin5m1 = c(0,0),
    adj1Txt1m1 = c(0,0),
adj1Txt2m1 = c(0,0),
adj1Txt3m1 = c(0,0),
adj1Txt4m1 = c(0,0),
adj1Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1))
ylength = length(inf.lag0m1)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1 = inf.lag0m1, 
    Econ1m1 = Econ1m1,
    Econ2m1 = Econ2m1,
    Econ3m1 = Econ3m1,
    Econ4m1 = Econ4m1,
    Econ5m1 = Econ5m1,
    adj1Fin1m1 = adj1Fin1m1,
adj1Fin2m1 = adj1Fin2m1,
adj1Fin3m1 = adj1Fin3m1,
adj1Fin4m1 = adj1Fin4m1,
adj1Fin5m1 = adj1Fin5m1,
    adj1Txt1m1 = adj1Txt1m1,
adj1Txt2m1 = adj1Txt2m1,
adj1Txt3m1 = adj1Txt3m1,
adj1Txt4m1 = adj1Txt4m1,
adj1Txt5m1 = adj1Txt5m1,
    block.surveym1 = block.surveym1,
    block.laborm1 = block.laborm1,
    block.realm1 = block.realm1,
    block.pricem1 = block.pricem1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q1 EX E
################

# INF Q1 EX E NB EW

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E B EW

set.seed(957)

X = data.frame(
  inf.lag0m1ex,
  Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,
  block.survey.exm1,
  block.labor.exm1,
  block.real.exm1,
  block.price.exm1)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX E B MIDAS (same results)

# INF Q1 EX EF NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,d_to_m_m1ex(Fin1m1ex),
d_to_m_m1ex(Fin2m1ex),
d_to_m_m1ex(Fin3m1ex),
d_to_m_m1ex(Fin4m1ex),
d_to_m_m1ex(Fin5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX EF NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
+ fmls(adj1Fin2m1ex,21,21,nealmon)
+ fmls(adj1Fin3m1ex,21,21,nealmon)
+ fmls(adj1Fin4m1ex,21,21,nealmon)
+ fmls(adj1Fin5m1ex,21,21,nealmon),
  start = list(
    adj1Fin1m1ex = c(0,0),
adj1Fin2m1ex = c(0,0),
adj1Fin3m1ex = c(0,0),
adj1Fin4m1ex = c(0,0),
adj1Fin5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
adj1Fin2m1ex = adj1Fin2m1ex,
adj1Fin3m1ex = adj1Fin3m1ex,
adj1Fin4m1ex = adj1Fin4m1ex,
adj1Fin5m1ex = adj1Fin5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX EF B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,
    d_to_m_m1ex(Fin1m1ex),
d_to_m_m1ex(Fin2m1ex),
d_to_m_m1ex(Fin3m1ex),
d_to_m_m1ex(Fin4m1ex),
d_to_m_m1ex(Fin5m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX EF B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
+ fmls(adj1Fin2m1ex,21,21,nealmon)
+ fmls(adj1Fin3m1ex,21,21,nealmon)
+ fmls(adj1Fin4m1ex,21,21,nealmon)
+ fmls(adj1Fin5m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Fin1m1ex = c(0,0),
adj1Fin2m1ex = c(0,0),
adj1Fin3m1ex = c(0,0),
adj1Fin4m1ex = c(0,0),
adj1Fin5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
adj1Fin2m1ex = adj1Fin2m1ex,
adj1Fin3m1ex = adj1Fin3m1ex,
adj1Fin4m1ex = adj1Fin4m1ex,
adj1Fin5m1ex = adj1Fin5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX ET NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,d_to_m_m1ex(Text1m1ex),
d_to_m_m1ex(Text2m1ex),
d_to_m_m1ex(Text3m1ex),
d_to_m_m1ex(Text4m1ex),
d_to_m_m1ex(Text5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX ET NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Txt1m1ex,21,21,nealmon)
+ fmls(adj1Txt2m1ex,21,21,nealmon)
+ fmls(adj1Txt3m1ex,21,21,nealmon)
+ fmls(adj1Txt4m1ex,21,21,nealmon)
+ fmls(adj1Txt5m1ex,21,21,nealmon),
  start = list(
    adj1Txt1m1ex = c(0,0),
adj1Txt2m1ex = c(0,0),
adj1Txt3m1ex = c(0,0),
adj1Txt4m1ex = c(0,0),
adj1Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
adj1Txt2m1ex = adj1Txt2m1ex,
adj1Txt3m1ex = adj1Txt3m1ex,
adj1Txt4m1ex = adj1Txt4m1ex,
adj1Txt5m1ex = adj1Txt5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX ET B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,
    d_to_m_m1ex(Text1m1ex),
d_to_m_m1ex(Text2m1ex),
d_to_m_m1ex(Text3m1ex),
d_to_m_m1ex(Text4m1ex),
d_to_m_m1ex(Text5m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX ET B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Txt1m1ex,21,21,nealmon)
+ fmls(adj1Txt2m1ex,21,21,nealmon)
+ fmls(adj1Txt3m1ex,21,21,nealmon)
+ fmls(adj1Txt4m1ex,21,21,nealmon)
+ fmls(adj1Txt5m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Txt1m1ex = c(0,0),
adj1Txt2m1ex = c(0,0),
adj1Txt3m1ex = c(0,0),
adj1Txt4m1ex = c(0,0),
adj1Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
adj1Txt2m1ex = adj1Txt2m1ex,
adj1Txt3m1ex = adj1Txt3m1ex,
adj1Txt4m1ex = adj1Txt4m1ex,
adj1Txt5m1ex = adj1Txt5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q1 EX EFT NB EW 

set.seed(957)

X = data.frame(inf.lag0m1ex,Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,d_to_m_m1ex(Fin1m1ex),
d_to_m_m1ex(Fin2m1ex),
d_to_m_m1ex(Fin3m1ex),
d_to_m_m1ex(Fin4m1ex),
d_to_m_m1ex(Fin5m1ex),d_to_m_m1ex(Text1m1ex),
d_to_m_m1ex(Text2m1ex),
d_to_m_m1ex(Text3m1ex),
d_to_m_m1ex(Text4m1ex),
d_to_m_m1ex(Text5m1ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX EFT NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
+ fmls(adj1Fin2m1ex,21,21,nealmon)
+ fmls(adj1Fin3m1ex,21,21,nealmon)
+ fmls(adj1Fin4m1ex,21,21,nealmon)
+ fmls(adj1Fin5m1ex,21,21,nealmon)
  + fmls(adj1Txt1m1ex,21,21,nealmon)
+ fmls(adj1Txt2m1ex,21,21,nealmon)
+ fmls(adj1Txt3m1ex,21,21,nealmon)
+ fmls(adj1Txt4m1ex,21,21,nealmon)
+ fmls(adj1Txt5m1ex,21,21,nealmon),
  start = list(
    adj1Fin1m1ex = c(0,0),
adj1Fin2m1ex = c(0,0),
adj1Fin3m1ex = c(0,0),
adj1Fin4m1ex = c(0,0),
adj1Fin5m1ex = c(0,0),
    adj1Txt1m1ex = c(0,0),
adj1Txt2m1ex = c(0,0),
adj1Txt3m1ex = c(0,0),
adj1Txt4m1ex = c(0,0),
adj1Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
adj1Fin2m1ex = adj1Fin2m1ex,
adj1Fin3m1ex = adj1Fin3m1ex,
adj1Fin4m1ex = adj1Fin4m1ex,
adj1Fin5m1ex = adj1Fin5m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
adj1Txt2m1ex = adj1Txt2m1ex,
adj1Txt3m1ex = adj1Txt3m1ex,
adj1Txt4m1ex = adj1Txt4m1ex,
adj1Txt5m1ex = adj1Txt5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q1 EX EFT B EW 

set.seed(957)

X = data.frame(
    inf.lag0m1ex,
    Econ1m1ex,
Econ2m1ex,
Econ3m1ex,
Econ4m1ex,
Econ5m1ex,
    d_to_m_m1ex(Fin1m1ex),
d_to_m_m1ex(Fin2m1ex),
d_to_m_m1ex(Fin3m1ex),
d_to_m_m1ex(Fin4m1ex),
d_to_m_m1ex(Fin5m1ex),
    d_to_m_m1ex(Text1m1ex),
d_to_m_m1ex(Text2m1ex),
d_to_m_m1ex(Text3m1ex),
d_to_m_m1ex(Text4m1ex),
d_to_m_m1ex(Text5m1ex),
    block.survey.exm1, 
    block.labor.exm1,
    block.real.exm1,
    block.price.exm1
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m1ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m1ex)^2)) * 100
rmse

# INF Q1 EX EFT B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m1ex ~ 1 
  + Econ1m1ex
+ Econ2m1ex
+ Econ3m1ex
+ Econ4m1ex
+ Econ5m1ex
  + fmls(adj1Fin1m1ex,21,21,nealmon)
+ fmls(adj1Fin2m1ex,21,21,nealmon)
+ fmls(adj1Fin3m1ex,21,21,nealmon)
+ fmls(adj1Fin4m1ex,21,21,nealmon)
+ fmls(adj1Fin5m1ex,21,21,nealmon)
  + fmls(adj1Txt1m1ex,21,21,nealmon)
+ fmls(adj1Txt2m1ex,21,21,nealmon)
+ fmls(adj1Txt3m1ex,21,21,nealmon)
+ fmls(adj1Txt4m1ex,21,21,nealmon)
+ fmls(adj1Txt5m1ex,21,21,nealmon)
  + block.survey.exm1
  + block.labor.exm1
  + block.real.exm1
  + block.price.exm1
  ,
  start = list(
    adj1Fin1m1ex = c(0,0),
adj1Fin2m1ex = c(0,0),
adj1Fin3m1ex = c(0,0),
adj1Fin4m1ex = c(0,0),
adj1Fin5m1ex = c(0,0),
    adj1Txt1m1ex = c(0,0),
adj1Txt2m1ex = c(0,0),
adj1Txt3m1ex = c(0,0),
adj1Txt4m1ex = c(0,0),
adj1Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m1ex))
ylength = length(inf.lag0m1ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m1ex = inf.lag0m1ex, 
    Econ1m1ex = Econ1m1ex,
    Econ2m1ex = Econ2m1ex,
    Econ3m1ex = Econ3m1ex,
    Econ4m1ex = Econ4m1ex,
    Econ5m1ex = Econ5m1ex,
    adj1Fin1m1ex = adj1Fin1m1ex,
adj1Fin2m1ex = adj1Fin2m1ex,
adj1Fin3m1ex = adj1Fin3m1ex,
adj1Fin4m1ex = adj1Fin4m1ex,
adj1Fin5m1ex = adj1Fin5m1ex,
    adj1Txt1m1ex = adj1Txt1m1ex,
adj1Txt2m1ex = adj1Txt2m1ex,
adj1Txt3m1ex = adj1Txt3m1ex,
adj1Txt4m1ex = adj1Txt4m1ex,
adj1Txt5m1ex = adj1Txt5m1ex,
    block.survey.exm1 = block.survey.exm1,
    block.labor.exm1 = block.labor.exm1,
    block.real.exm1 = block.real.exm1,
    block.price.exm1 = block.price.exm1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q4 COVID E1
################

# INF Q4 COVID E1 NB EW

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1 B EW

set.seed(957)

X = data.frame(
  inf.lag0m4,
  Econ1m4,
  block.surveym4,
  block.laborm4,
  block.realm4,
  block.pricem4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1 B MIDAS (same results)

# INF Q4 COVID E1F1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,d_to_m_m4(Fin1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1F1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Fin1m1,21,21,nealmon),
  start = list(
    adj4Fin1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Fin1m1 = adj4Fin1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID E1F1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
    d_to_m_m4(Fin1m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Fin1m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Fin1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Fin1m1 = adj4Fin1m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID E1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,d_to_m_m4(Txt1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Txt1m1,21,21,nealmon),
  start = list(
    adj4Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Txt1m1 = adj4Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID E1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
    d_to_m_m4(Txt1m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Txt1m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Txt1m1 = adj4Txt1m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q4 COVID E1F1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,d_to_m_m4(Fin1m4),d_to_m_m4(Txt1m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1F1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Fin1m1,21,21,nealmon)
  + fmls(adj4Txt1m1,21,21,nealmon),
  start = list(
    adj4Fin1m1 = c(0,0),
    adj4Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Fin1m1 = adj4Fin1m1,
    adj4Txt1m1 = adj4Txt1m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID E1F1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
    d_to_m_m4(Fin1m4),
    d_to_m_m4(Txt1m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
  + fmls(adj4Fin1m1,21,21,nealmon)
  + fmls(adj4Txt1m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Fin1m1 = c(0,0),
    adj4Txt1m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    adj4Fin1m1 = adj4Fin1m1,
    adj4Txt1m1 = adj4Txt1m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q4 EX E1
################

# INF Q4 EX E1 NB EW

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1 B EW

set.seed(957)

X = data.frame(
  inf.lag0m4ex,
  Econ1m4ex,
  block.survey.exm4,
  block.labor.exm4,
  block.real.exm4,
  block.price.exm4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1 B MIDAS (same results)

# INF Q4 EX E1F1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,d_to_m_m4ex(Fin1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1F1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon),
  start = list(
    adj4Fin1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX E1F1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
    d_to_m_m4ex(Fin1m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1F1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Fin1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX E1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,d_to_m_m4ex(Text1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Txt1m1ex,21,21,nealmon),
  start = list(
    adj4Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Txt1m1ex = adj4Txt1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX E1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
    d_to_m_m4ex(Text1m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Txt1m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q4 EX E1F1T1 NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,d_to_m_m4ex(Fin1m4ex),d_to_m_m4ex(Text1m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1F1T1 NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
  + fmls(adj4Txt1m1ex,21,21,nealmon),
  start = list(
    adj4Fin1m1ex = c(0,0),
    adj4Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
    adj4Txt1m1ex = adj4Txt1m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX E1F1T1 B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
    d_to_m_m4ex(Fin1m4ex),
    d_to_m_m4ex(Text1m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E1F1T1 B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
  + fmls(adj4Txt1m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Fin1m1ex = c(0,0),
    adj4Txt1m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q4 COVID E
################

# INF Q4 COVID E NB EW

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E B EW

set.seed(957)

X = data.frame(
  inf.lag0m4,
  Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,
  block.surveym4,
  block.laborm4,
  block.realm4,
  block.pricem4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID E B MIDAS (same results)

# INF Q4 COVID EF NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,d_to_m_m4(Fin1m4),
d_to_m_m4(Fin2m4),
d_to_m_m4(Fin3m4),
d_to_m_m4(Fin4m4),
d_to_m_m4(Fin5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID EF NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Fin1m1,21,21,nealmon)
+ fmls(adj4Fin2m1,21,21,nealmon)
+ fmls(adj4Fin3m1,21,21,nealmon)
+ fmls(adj4Fin4m1,21,21,nealmon)
+ fmls(adj4Fin5m1,21,21,nealmon),
  start = list(
    adj4Fin1m1 = c(0,0),
adj4Fin2m1 = c(0,0),
adj4Fin3m1 = c(0,0),
adj4Fin4m1 = c(0,0),
adj4Fin5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Fin1m1 = adj4Fin1m1,
adj4Fin2m1 = adj4Fin2m1,
adj4Fin3m1 = adj4Fin3m1,
adj4Fin4m1 = adj4Fin4m1,
adj4Fin5m1 = adj4Fin5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID EF B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,
    d_to_m_m4(Fin1m4),
d_to_m_m4(Fin2m4),
d_to_m_m4(Fin3m4),
d_to_m_m4(Fin4m4),
d_to_m_m4(Fin5m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID EF B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Fin1m1,21,21,nealmon)
+ fmls(adj4Fin2m1,21,21,nealmon)
+ fmls(adj4Fin3m1,21,21,nealmon)
+ fmls(adj4Fin4m1,21,21,nealmon)
+ fmls(adj4Fin5m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Fin1m1 = c(0,0),
adj4Fin2m1 = c(0,0),
adj4Fin3m1 = c(0,0),
adj4Fin4m1 = c(0,0),
adj4Fin5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Fin1m1 = adj4Fin1m1,
adj4Fin2m1 = adj4Fin2m1,
adj4Fin3m1 = adj4Fin3m1,
adj4Fin4m1 = adj4Fin4m1,
adj4Fin5m1 = adj4Fin5m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID ET NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,d_to_m_m4(Txt1m4),
d_to_m_m4(Txt2m4),
d_to_m_m4(Txt3m4),
d_to_m_m4(Txt4m4),
d_to_m_m4(Txt5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID ET NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Txt1m1,21,21,nealmon)
+ fmls(adj4Txt2m1,21,21,nealmon)
+ fmls(adj4Txt3m1,21,21,nealmon)
+ fmls(adj4Txt4m1,21,21,nealmon)
+ fmls(adj4Txt5m1,21,21,nealmon),
  start = list(
    adj4Txt1m1 = c(0,0),
adj4Txt2m1 = c(0,0),
adj4Txt3m1 = c(0,0),
adj4Txt4m1 = c(0,0),
adj4Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Txt1m1 = adj4Txt1m1,
adj4Txt2m1 = adj4Txt2m1,
adj4Txt3m1 = adj4Txt3m1,
adj4Txt4m1 = adj4Txt4m1,
adj4Txt5m1 = adj4Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID ET B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,
    d_to_m_m4(Txt1m4),
d_to_m_m4(Txt2m4),
d_to_m_m4(Txt3m4),
d_to_m_m4(Txt4m4),
d_to_m_m4(Txt5m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID ET B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Txt1m1,21,21,nealmon)
+ fmls(adj4Txt2m1,21,21,nealmon)
+ fmls(adj4Txt3m1,21,21,nealmon)
+ fmls(adj4Txt4m1,21,21,nealmon)
+ fmls(adj4Txt5m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Txt1m1 = c(0,0),
adj4Txt2m1 = c(0,0),
adj4Txt3m1 = c(0,0),
adj4Txt4m1 = c(0,0),
adj4Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Txt1m1 = adj4Txt1m1,
adj4Txt2m1 = adj4Txt2m1,
adj4Txt3m1 = adj4Txt3m1,
adj4Txt4m1 = adj4Txt4m1,
adj4Txt5m1 = adj4Txt5m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q4 COVID EFT NB EW 

set.seed(957)

X = data.frame(inf.lag0m4,Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,d_to_m_m4(Fin1m4),
d_to_m_m4(Fin2m4),
d_to_m_m4(Fin3m4),
d_to_m_m4(Fin4m4),
d_to_m_m4(Fin5m4),d_to_m_m4(Txt1m4),
d_to_m_m4(Txt2m4),
d_to_m_m4(Txt3m4),
d_to_m_m4(Txt4m4),
d_to_m_m4(Txt5m4))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID EFT NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Fin1m1,21,21,nealmon)
+ fmls(adj4Fin2m1,21,21,nealmon)
+ fmls(adj4Fin3m1,21,21,nealmon)
+ fmls(adj4Fin4m1,21,21,nealmon)
+ fmls(adj4Fin5m1,21,21,nealmon)
  + fmls(adj4Txt1m1,21,21,nealmon)
+ fmls(adj4Txt2m1,21,21,nealmon)
+ fmls(adj4Txt3m1,21,21,nealmon)
+ fmls(adj4Txt4m1,21,21,nealmon)
+ fmls(adj4Txt5m1,21,21,nealmon),
  start = list(
    adj4Fin1m1 = c(0,0),
adj4Fin2m1 = c(0,0),
adj4Fin3m1 = c(0,0),
adj4Fin4m1 = c(0,0),
adj4Fin5m1 = c(0,0),
    adj4Txt1m1 = c(0,0),
adj4Txt2m1 = c(0,0),
adj4Txt3m1 = c(0,0),
adj4Txt4m1 = c(0,0),
adj4Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Fin1m1 = adj4Fin1m1,
adj4Fin2m1 = adj4Fin2m1,
adj4Fin3m1 = adj4Fin3m1,
adj4Fin4m1 = adj4Fin4m1,
adj4Fin5m1 = adj4Fin5m1,
    adj4Txt1m1 = adj4Txt1m1,
adj4Txt2m1 = adj4Txt2m1,
adj4Txt3m1 = adj4Txt3m1,
adj4Txt4m1 = adj4Txt4m1,
adj4Txt5m1 = adj4Txt5m1
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 COVID EFT B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4,
    Econ1m4,
Econ2m4,
Econ3m4,
Econ4m4,
Econ5m4,
    d_to_m_m4(Fin1m4),
d_to_m_m4(Fin2m4),
d_to_m_m4(Fin3m4),
d_to_m_m4(Fin4m4),
d_to_m_m4(Fin5m4),
    d_to_m_m4(Txt1m4),
d_to_m_m4(Txt2m4),
d_to_m_m4(Txt3m4),
d_to_m_m4(Txt4m4),
d_to_m_m4(Txt5m4),
    block.surveym4, 
    block.laborm4,
    block.realm4,
    block.pricem4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4 ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4)^2)) * 100
rmse

# INF Q4 COVID EFT B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4 ~ 1 
  + Econ1m4
+ Econ2m4
+ Econ3m4
+ Econ4m4
+ Econ5m4
  + fmls(adj4Fin1m1,21,21,nealmon)
+ fmls(adj4Fin2m1,21,21,nealmon)
+ fmls(adj4Fin3m1,21,21,nealmon)
+ fmls(adj4Fin4m1,21,21,nealmon)
+ fmls(adj4Fin5m1,21,21,nealmon)
  + fmls(adj4Txt1m1,21,21,nealmon)
+ fmls(adj4Txt2m1,21,21,nealmon)
+ fmls(adj4Txt3m1,21,21,nealmon)
+ fmls(adj4Txt4m1,21,21,nealmon)
+ fmls(adj4Txt5m1,21,21,nealmon)
  + block.surveym4
  + block.laborm4
  + block.realm4
  + block.pricem4
  ,
  start = list(
    adj4Fin1m1 = c(0,0),
adj4Fin2m1 = c(0,0),
adj4Fin3m1 = c(0,0),
adj4Fin4m1 = c(0,0),
adj4Fin5m1 = c(0,0),
    adj4Txt1m1 = c(0,0),
adj4Txt2m1 = c(0,0),
adj4Txt3m1 = c(0,0),
adj4Txt4m1 = c(0,0),
adj4Txt5m1 = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4))
ylength = length(inf.lag0m4)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4 = inf.lag0m4, 
    Econ1m4 = Econ1m4,
    Econ2m4 = Econ2m4,
    Econ3m4 = Econ3m4,
    Econ4m4 = Econ4m4,
    Econ5m4 = Econ5m4,
    adj4Fin1m1 = adj4Fin1m1,
adj4Fin2m1 = adj4Fin2m1,
adj4Fin3m1 = adj4Fin3m1,
adj4Fin4m1 = adj4Fin4m1,
adj4Fin5m1 = adj4Fin5m1,
    adj4Txt1m1 = adj4Txt1m1,
adj4Txt2m1 = adj4Txt2m1,
adj4Txt3m1 = adj4Txt3m1,
adj4Txt4m1 = adj4Txt4m1,
adj4Txt5m1 = adj4Txt5m1,
    block.surveym4 = block.surveym4,
    block.laborm4 = block.laborm4,
    block.realm4 = block.realm4,
    block.pricem4 = block.pricem4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

################
### INF Q4 EX E
################

# INF Q4 EX E NB EW

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E B EW

set.seed(957)

X = data.frame(
  inf.lag0m4ex,
  Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,
  block.survey.exm4,
  block.labor.exm4,
  block.real.exm4,
  block.price.exm4)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX E B MIDAS (same results)

# INF Q4 EX EF NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,d_to_m_m4ex(Fin1m4ex),
d_to_m_m4ex(Fin2m4ex),
d_to_m_m4ex(Fin3m4ex),
d_to_m_m4ex(Fin4m4ex),
d_to_m_m4ex(Fin5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX EF NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
+ fmls(adj4Fin2m1ex,21,21,nealmon)
+ fmls(adj4Fin3m1ex,21,21,nealmon)
+ fmls(adj4Fin4m1ex,21,21,nealmon)
+ fmls(adj4Fin5m1ex,21,21,nealmon),
  start = list(
    adj4Fin1m1ex = c(0,0),
adj4Fin2m1ex = c(0,0),
adj4Fin3m1ex = c(0,0),
adj4Fin4m1ex = c(0,0),
adj4Fin5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
adj4Fin2m1ex = adj4Fin2m1ex,
adj4Fin3m1ex = adj4Fin3m1ex,
adj4Fin4m1ex = adj4Fin4m1ex,
adj4Fin5m1ex = adj4Fin5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX EF B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,
    d_to_m_m4ex(Fin1m4ex),
d_to_m_m4ex(Fin2m4ex),
d_to_m_m4ex(Fin3m4ex),
d_to_m_m4ex(Fin4m4ex),
d_to_m_m4ex(Fin5m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX EF B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
+ fmls(adj4Fin2m1ex,21,21,nealmon)
+ fmls(adj4Fin3m1ex,21,21,nealmon)
+ fmls(adj4Fin4m1ex,21,21,nealmon)
+ fmls(adj4Fin5m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Fin1m1ex = c(0,0),
adj4Fin2m1ex = c(0,0),
adj4Fin3m1ex = c(0,0),
adj4Fin4m1ex = c(0,0),
adj4Fin5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
adj4Fin2m1ex = adj4Fin2m1ex,
adj4Fin3m1ex = adj4Fin3m1ex,
adj4Fin4m1ex = adj4Fin4m1ex,
adj4Fin5m1ex = adj4Fin5m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX ET NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,d_to_m_m4ex(Text1m4ex),
d_to_m_m4ex(Text2m4ex),
d_to_m_m4ex(Text3m4ex),
d_to_m_m4ex(Text4m4ex),
d_to_m_m4ex(Text5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX ET NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Txt1m1ex,21,21,nealmon)
+ fmls(adj4Txt2m1ex,21,21,nealmon)
+ fmls(adj4Txt3m1ex,21,21,nealmon)
+ fmls(adj4Txt4m1ex,21,21,nealmon)
+ fmls(adj4Txt5m1ex,21,21,nealmon),
  start = list(
    adj4Txt1m1ex = c(0,0),
adj4Txt2m1ex = c(0,0),
adj4Txt3m1ex = c(0,0),
adj4Txt4m1ex = c(0,0),
adj4Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
adj4Txt2m1ex = adj4Txt2m1ex,
adj4Txt3m1ex = adj4Txt3m1ex,
adj4Txt4m1ex = adj4Txt4m1ex,
adj4Txt5m1ex = adj4Txt5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX ET B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,
    d_to_m_m4ex(Text1m4ex),
d_to_m_m4ex(Text2m4ex),
d_to_m_m4ex(Text3m4ex),
d_to_m_m4ex(Text4m4ex),
d_to_m_m4ex(Text5m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX ET B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Txt1m1ex,21,21,nealmon)
+ fmls(adj4Txt2m1ex,21,21,nealmon)
+ fmls(adj4Txt3m1ex,21,21,nealmon)
+ fmls(adj4Txt4m1ex,21,21,nealmon)
+ fmls(adj4Txt5m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Txt1m1ex = c(0,0),
adj4Txt2m1ex = c(0,0),
adj4Txt3m1ex = c(0,0),
adj4Txt4m1ex = c(0,0),
adj4Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
adj4Txt2m1ex = adj4Txt2m1ex,
adj4Txt3m1ex = adj4Txt3m1ex,
adj4Txt4m1ex = adj4Txt4m1ex,
adj4Txt5m1ex = adj4Txt5m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse





# INF Q4 EX EFT NB EW 

set.seed(957)

X = data.frame(inf.lag0m4ex,Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,d_to_m_m4ex(Fin1m4ex),
d_to_m_m4ex(Fin2m4ex),
d_to_m_m4ex(Fin3m4ex),
d_to_m_m4ex(Fin4m4ex),
d_to_m_m4ex(Fin5m4ex),d_to_m_m4ex(Text1m4ex),
d_to_m_m4ex(Text2m4ex),
d_to_m_m4ex(Text3m4ex),
d_to_m_m4ex(Text4m4ex),
d_to_m_m4ex(Text5m4ex))

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX EFT NB MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
+ fmls(adj4Fin2m1ex,21,21,nealmon)
+ fmls(adj4Fin3m1ex,21,21,nealmon)
+ fmls(adj4Fin4m1ex,21,21,nealmon)
+ fmls(adj4Fin5m1ex,21,21,nealmon)
  + fmls(adj4Txt1m1ex,21,21,nealmon)
+ fmls(adj4Txt2m1ex,21,21,nealmon)
+ fmls(adj4Txt3m1ex,21,21,nealmon)
+ fmls(adj4Txt4m1ex,21,21,nealmon)
+ fmls(adj4Txt5m1ex,21,21,nealmon),
  start = list(
    adj4Fin1m1ex = c(0,0),
adj4Fin2m1ex = c(0,0),
adj4Fin3m1ex = c(0,0),
adj4Fin4m1ex = c(0,0),
adj4Fin5m1ex = c(0,0),
    adj4Txt1m1ex = c(0,0),
adj4Txt2m1ex = c(0,0),
adj4Txt3m1ex = c(0,0),
adj4Txt4m1ex = c(0,0),
adj4Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
adj4Fin2m1ex = adj4Fin2m1ex,
adj4Fin3m1ex = adj4Fin3m1ex,
adj4Fin4m1ex = adj4Fin4m1ex,
adj4Fin5m1ex = adj4Fin5m1ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
adj4Txt2m1ex = adj4Txt2m1ex,
adj4Txt3m1ex = adj4Txt3m1ex,
adj4Txt4m1ex = adj4Txt4m1ex,
adj4Txt5m1ex = adj4Txt5m1ex
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse

# INF Q4 EX EFT B EW 

set.seed(957)

X = data.frame(
    inf.lag0m4ex,
    Econ1m4ex,
Econ2m4ex,
Econ3m4ex,
Econ4m4ex,
Econ5m4ex,
    d_to_m_m4ex(Fin1m4ex),
d_to_m_m4ex(Fin2m4ex),
d_to_m_m4ex(Fin3m4ex),
d_to_m_m4ex(Fin4m4ex),
d_to_m_m4ex(Fin5m4ex),
    d_to_m_m4ex(Text1m4ex),
d_to_m_m4ex(Text2m4ex),
d_to_m_m4ex(Text3m4ex),
d_to_m_m4ex(Text4m4ex),
d_to_m_m4ex(Text5m4ex),
    block.survey.exm4, 
    block.labor.exm4,
    block.real.exm4,
    block.price.exm4
)

index_cutoff <- floor(0.8 * dim(X)[1])
X_train = X[1:index_cutoff,]
X_test = X[index_cutoff:nrow(X),]

model = lm(inf.lag0m4ex ~ .,data=X_train)

predictions = predict(model, newdata = X_test)

rmse = sqrt(mean((predictions - X_test$inf.lag0m4ex)^2)) * 100
rmse

# INF Q4 EX EFT B MIDAS 

set.seed(957)
model = midas_r(
  inf.lag0m4ex ~ 1 
  + Econ1m4ex
+ Econ2m4ex
+ Econ3m4ex
+ Econ4m4ex
+ Econ5m4ex
  + fmls(adj4Fin1m1ex,21,21,nealmon)
+ fmls(adj4Fin2m1ex,21,21,nealmon)
+ fmls(adj4Fin3m1ex,21,21,nealmon)
+ fmls(adj4Fin4m1ex,21,21,nealmon)
+ fmls(adj4Fin5m1ex,21,21,nealmon)
  + fmls(adj4Txt1m1ex,21,21,nealmon)
+ fmls(adj4Txt2m1ex,21,21,nealmon)
+ fmls(adj4Txt3m1ex,21,21,nealmon)
+ fmls(adj4Txt4m1ex,21,21,nealmon)
+ fmls(adj4Txt5m1ex,21,21,nealmon)
  + block.survey.exm4
  + block.labor.exm4
  + block.real.exm4
  + block.price.exm4
  ,
  start = list(
    adj4Fin1m1ex = c(0,0),
adj4Fin2m1ex = c(0,0),
adj4Fin3m1ex = c(0,0),
adj4Fin4m1ex = c(0,0),
adj4Fin5m1ex = c(0,0),
    adj4Txt1m1ex = c(0,0),
adj4Txt2m1ex = c(0,0),
adj4Txt3m1ex = c(0,0),
adj4Txt4m1ex = c(0,0),
adj4Txt5m1ex = c(0,0)
    )
)

index_cutoff = floor(0.8 * length(inf.lag0m4ex))
ylength = length(inf.lag0m4ex)

avgroll = average_forecast(
  list(model),
  data = list(
    inf.lag0m4ex = inf.lag0m4ex, 
    Econ1m4ex = Econ1m4ex,
    Econ2m4ex = Econ2m4ex,
    Econ3m4ex = Econ3m4ex,
    Econ4m4ex = Econ4m4ex,
    Econ5m4ex = Econ5m4ex,
    adj4Fin1m1ex = adj4Fin1m1ex,
adj4Fin2m1ex = adj4Fin2m1ex,
adj4Fin3m1ex = adj4Fin3m1ex,
adj4Fin4m1ex = adj4Fin4m1ex,
adj4Fin5m1ex = adj4Fin5m1ex,
    adj4Txt1m1ex = adj4Txt1m1ex,
adj4Txt2m1ex = adj4Txt2m1ex,
adj4Txt3m1ex = adj4Txt3m1ex,
adj4Txt4m1ex = adj4Txt4m1ex,
adj4Txt5m1ex = adj4Txt5m1ex,
    block.survey.exm4 = block.survey.exm4,
    block.labor.exm4 = block.labor.exm4,
    block.real.exm4 = block.real.exm4,
    block.price.exm4 = block.price.exm4
  ),
  insample = 1:index_cutoff,
  outsample = (index_cutoff+1):ylength,
  type = 'rolling'
)

rmse = sqrt(avgroll$accuracy$average[1,2]) * 100
rmse


