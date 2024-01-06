#------------------------------------------------------------------------
### DOWNLOADING US DATA
#------------------------------------------------------------------------

# Put data files in the same folder as ``getUSdata.R`

#------------------------------------------------------------------------
### 0 IMPORT PACKAGES. ESTABLISH API KEY.
#------------------------------------------------------------------------

# Used packages
library(tidyverse)
library(purrr)
library(fredr)
library(readxl)
library(quantmod)
library(factoextra)
library(FactoMineR)
library(forecast)
library(corrplot)
library(missMDA)
library(midasr)
library(astsa)

# Set your FRED Key
fredr_set_key("ac7fd08d9f2c9ad01c28fb92fc5b3c85")

#------------------------------------------------------------------------
### 1 IMPORT ECONOMIC DATA (MONTHLY AND QUARTERLY DATA)
#------------------------------------------------------------------------

### 1.1 Import data from Federal Reserve St. Louis Economic Data

# Names from Metadata file
names1 = read_excel("Metadata.xlsx",sheet = 1,col_names = "Names")
names = names1$Names

# Creating dates (first day of each month)
data <- data.frame(
  date = seq(as.Date("1900-01-01"), 
             as.Date(paste0(substr(Sys.Date(), 1, 4), "-", 
                            substr(Sys.Date(), 6, 7), "-01")), by="1 month")
)

# Importing data from FRED and adding it to the dataframe by date
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

# Eliminating all observations prior to 1999 and posterior to March 2023
data <- data %>%
  filter(date >= as.Date("1999-1-1")) %>%
  filter(date <= as.Date("2023-3-31")) %>%
  arrange(desc(date))

### 1.2 Importing data from the OECD

# Importing data from Excel file
df = read_excel("OECDUSSurveyData.xlsx",sheet=1)

# Adding the data
for (i in 2:ncol(df)) {
  data <- data %>%
    left_join(df[,c(1,i)], by="date") %>%
    tibble()
}

# Renaming the data
ec.data.US <- data
rm(data)

# Adjusting dates so they are end of month, end of quarter
ec.data.US$date = ec.data.US$date + months(1) - 24*60*60
ec.data.US$gdpc1 = c(ec.data.US$gdpc1[-c(1:2)],rep(NA,2))
ec.data.US$ulcnfb = c(ec.data.US$ulcnfb[-c(1:2)],rep(NA,2))
ec.data.US$a261rx1q020sbea = c(ec.data.US$a261rx1q020sbea[-c(1:2)],rep(NA,2))

#------------------------------------------------------------------------------------
### 2 IMPORTING FINANCIAL INFORMATION (DAILY DATA)
#------------------------------------------------------------------------------------

### 2.1 Import data from Federal Reserve St. Louis Economic Data

# Importing names
names2 = read_excel("Metadata.xlsx",sheet = 2,col_names = "Names")
names = names2$Names

# Generating daily dates
data <- data.frame(
  date = seq(as.Date("1900-01-01"), 
             as.Date(paste0(substr(Sys.Date(), 1, 4), "-", 
                            substr(Sys.Date(), 6, 7), "-01")), by="1 day")
) 

# Importing the data
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

# Eliminating all observations prior to last week of 1999 and posterior to march 2023
data <- data %>%
  filter(date >= as.Date("1999-12-25")) %>%
  filter(date <= as.Date("2023-3-31")) %>%
  arrange(desc(date))

### 2.2A Data from Yahoo Finance (To be substituted by Bloomberg data)

# Importing names
#names3 = read_excel("Metadata.xlsx",sheet = 3,col_names = "Names")
#names = names3$Names

# Getting data from Yahoo Finance
#for (name in names){
#  getSymbols(name,from="1999-01-25", to="2023-4-1")
#}

# Merging data
#output_list <- list()

#names.mod = names
#names.mod[8] = "GSPC" #Format that name appropriately before merging

#for (name in names.mod[8:length(names.mod)]) {
#  df <- get(name)
#  output_df <- cbind("date" = index(df), as_tibble(df[, c(5, 6)]))
#  output_list[[name]] <- output_df
#}

#for (name in names.mod[1:7]) {
#  df <- get(name)
#  output_df <- cbind("date" = index(df), as_tibble(df[, 6]))
#  output_list[[name]] <- output_df
#}

#join_by_date <- function(df1, df2) {
#  full_join(df1, df2, by = "date")
#}

#result_df <- reduce(output_list, join_by_date)

### 2.2B Data from Bloomberg

bloomberg = read_excel("BloombergExtract.xlsx",sheet=1)
bloomberg = rename(bloomberg,date=Date)

### 2.3 Merging data

data = left_join(data,bloomberg,by="date")

# Renaming data
daily.data.US <- data
rm(data)

#------------------------------------------------------------------------------------
### 3 TRANSFORMING THE DAILY DATA AND OBTAINING FINANCIAL FACTORS (DAILY)
#------------------------------------------------------------------------------------

# Computing the daily inflation expectation variables
# Substituting the yield on inflation protected bonds by their spread
# with respect to non inflation protected corresponding bonds

attach(daily.data.US)
inflation_vars = c("dfii5","dfii7","dfii10","dfii20","dfii30")
corresponding_vars = c("dgs5","dgs7","dgs10","dgs20","dgs30")

for (i in 1:length(inflation_vars)){
  vari = inflation_vars[i]
  if (substr(vari, nchar(vari), nchar(vari))=="0"){
    a = paste0("sinf",substr(vari, nchar(vari)-1, nchar(vari)))
  } else {
    a = paste0("sinf",substr(vari, nchar(vari), nchar(vari)))
  }
  daily.data.US = daily.data.US %>%
    mutate(!!a := get(corresponding_vars[i])-get(inflation_vars[i])) %>%
    select(-one_of(inflation_vars))
}

# Eliminating all dates that correspond to weekends 

daily.data.US = daily.data.US[as.numeric(format(daily.data.US$date, "%u"))<=5,]

### 3.1 Making the daily series stationary
# Some transformations are log differences
# Others are level differences
# REVISE INDEXES AFTER GETTING EIKON DATA

# Creating function that does log transformation
add_min_log = function(x){
  if(min(x,na.rm = TRUE)<=0){
    m = -min(x,na.rm = TRUE)+1 # avoids negative numbers in series
  }else{
    m=0
  }
  return(log(x+m))
}

# Transformation to logs, of some of the variables
indexes.vars.log.difference = 20:38 
daily.data.US[,indexes.vars.log.difference] = apply(daily.data.US[,indexes.vars.log.difference],
                                                2,add_min_log)

# Doing first difference (when previoys day was holiday, lag 2 difference)
returns = function(x){
  d = rev(diff((rev(x))))
  m = which(is.na(x))
  for (i in m){
    d[i-1]=x[i-1]-x[i+1]
  }
  return(d)
}

aux = daily.data.US
aux = slice(aux,-n()) # remove last row after doing first difference
for (i in 2:dim(daily.data.US)[2]){
  aux[[i]]=returns(daily.data.US[[i]])
}

daily.data.US = aux
rm(aux)

# EM-PCA imputation
empca = imputePCA(select(daily.data.US,-date),ncp = 5,scale = TRUE,method = "Regularized")
empca = as.data.frame(empca$completeObs)
EMPCA = PCA(empca,scale.unit = TRUE,graph = FALSE)

daily_factors = as.data.frame(EMPCA$ind$coord)
daily_factors$date = daily.data.US$date
financial_factors = daily_factors[, c(ncol(daily_factors), 1:(ncol(daily_factors)-1))]

# Exporting the data after filling missing values
empca$date=daily.data.US$date
empca = empca[, c(ncol(empca), 1:(ncol(empca)-1))]
write.csv(empca,"financial_vars.csv")

#------------------------------------------------------------------------------------
### 4 IMPORTING AND PROCESSING TEXTUAL DATA (DAILY)
#------------------------------------------------------------------------------------

# Importing the data, which is already normalized
text = read_csv("TopicsAggNorm_US.csv")
text_NC = text %>% select(-ends_with("NP")) %>% 
  select(-c(2,3,204)) %>% 
  rename(date = news_date) %>% 
  arrange(desc(row_number()))
text_NC$date = as.Date(text_NC$date,format = "%d/%m/%Y")

# Generating all dates, removing weekends, and adding data.
# Then, computing percentage increase (series have unit root)

df.aux = data.frame(date=rev(seq(as.Date("2000-01-01"), as.Date("2023-03-31"), by = "day")))

df.aux = df.aux %>% left_join(text_NC) %>%
  filter(format(date,"%u")<=5)

aux = df.aux
aux = slice(aux,-n()) # remove last row after doing first difference
for (i in 2:dim(df.aux)[2]){
  aux[[i]]=returns(add_min_log(df.aux[[i]]))
}

text_NC = aux %>% filter(date <= as.Date("2023-03-10"))
rm(df.aux,aux)

# PCA of all daily data together (not executed)
#merge = as.data.frame(full_join(daily.data.US,text_NC,by="date"))
#mpca = PCA(select(merge,-date),scale.unit = TRUE, graph = FALSE,ncp = 20)
#mpca$eig
#round(mpca$var$coord,1) # See notes

# Impute Missing Values using EM-PCA

em.text = imputePCA(select(text_NC,-date),
                    ncp = 5,scale = TRUE,method = "Regularized")
em.text = as.data.frame(em.text$completeObs)

# PCA of text data only
tNCpca = PCA(em.text,scale.unit = FALSE,graph = FALSE)
text_dims = as.data.frame(tNCpca$ind$coord)
text_dims$date = text_NC$date
text_factors = text_dims[, c(ncol(text_dims), 1:(ncol(text_dims)-1))]

# Correlation between financial and text daily factors
merge = as.data.frame(full_join(financial_factors,text_factors,by="date"))
corrplot(cor(select(merge,-date),use = "pairwise.complete.obs", method = "kendall"))

# Exporting the data after filling missing values
em.text$date = text_NC$date
em.text = em.text[, c(ncol(em.text), 1:(ncol(em.text)-1))]
write.csv(em.text,"text_vars.csv")

#------------------------------------------------------------------------------
### 5 TREATING MONTHLY DATA
#------------------------------------------------------------------------------

### 5.0 Transformation functions

percentage.increase = function(x){
  x = rev(x)
  xx = x
  for (i in 2:length(x)){
    xx[i]=x[i]/x[i-1]-1
  }
  xx[1]=NA
  return(rev(xx))
}

level.increase = function(x){
  x = rev(x)
  xx = x
  for (i in 2:length(x)){
    xx[i]=x[i]-x[i-1]
  }
  xx[1]=NA
  return(rev(xx))
}

transf3 = function(x){
  xx = percentage.increase(x)
  xxx = level.increase(xx)
  return(xxx)
}

### 5.1 Dealing with quarterly data

# Separating the monthly variables from the quarterly ones
quarterly.data.US1 = select(ec.data.US,c("date","gdpc1","ulcnfb","a261rx1q020sbea"))
quarterly.data.US2 = na.omit(quarterly.data.US1) #Only quarterly data

# Linearly interpolating series to move them to monthly frequency and compute the factors
# Adding required NAs at the end of the sample (non published dates)

### THIS STEP HERE CHANGES AS PUBLICATIONS ARE BEING RELEASED

quarterly.data.US1$gdpc1 = c(as.numeric(zoo::na.approx(quarterly.data.US1$gdpc1)),rep(NA,2))
quarterly.data.US1$ulcnfb = c(as.numeric(zoo::na.approx(quarterly.data.US1$ulcnfb)),
                              rep(NA,2))
quarterly.data.US1$a261rx1q020sbea = 
  c(as.numeric(zoo::na.approx(quarterly.data.US1$a261rx1q020sbea)),rep(NA,2))

# Deciding transformation using the non-interpolated data

auto.arima(quarterly.data.US2$gdpc1,seasonal = FALSE) # Integrada de orden 1 
auto.arima(quarterly.data.US2$ulcnfb,seasonal = FALSE) #Integrada de orden 2 
auto.arima(quarterly.data.US2$a261rx1q020sbea,seasonal = FALSE) # Integrada de orden 1

# Doing transformations 

quarterly.data.US1$gdpc1 = percentage.increase(quarterly.data.US1$gdpc1)
quarterly.data.US1$ulcnfb = transf3(quarterly.data.US1$ulcnfb)
quarterly.data.US1$a261rx1q020sbea = percentage.increase(quarterly.data.US1$a261rx1q020sbea)


### 5.1 Deciding differentiation monthly data
monthly.data.US = select(ec.data.US,-c("gdpc1","ulcnfb","a261rx1q020sbea"))
attach(monthly.data.US)

# Importing information about seasonal treatment of the series
metadata = read_excel("Metadata.xlsx",sheet = 4,col_names = FALSE)
names(metadata)=c("code","freq","seas.adj","trans")
mdata = subset(metadata, freq == "m")

# Function to decide differences
tostationary = function(x,f,s=TRUE){
  x = ts(rev(x),start = c(1999,1),end=c(2023,4),frequency = f)
  a = auto.arima(x,approximation = TRUE, lambda = NULL,seasonal = s)
  d = a$arma[6] # regular differences
  D = a$arma[7] # seasonal differences
  if (d!=0){
    if(D!=0){
      x = diff(diff(x,differences=D,lag = f),differences = d,lag=1)
    }else{
      x = diff(x,differences = d,lag=1)
    }
  }else{
    if(D!=0){
      x = diff(x,differences=D,lag = f)
    }
  }
  return(list("d"=d,"D"=D))
}

# Computing differences
diffs.monthly = vector(mode = "list")
for (name in mdata$code){
  s = mdata$seas.adj[which(mdata$code==name)]
  if(s==1){s=FALSE}else{s=TRUE} # do not adjust already seasonally adjusted series
  st = tostationary(get(tolower(name)),12,s)
  diffs.monthly[[name]]["Regular_differences"] = st[1]
  diffs.monthly[[name]]["Seasonal_differences"] = st[2]
}

lapply(diffs.monthly, function(x) x[[1]]) #Regular differences
lapply(diffs.monthly, function(x) x[[2]]) #Seasonal differences

### 5.2 Creating monthly transformations

# Variables with no transformation
monthly.trans0 = subset(monthly.data.US, 
                        select = tolower(filter(metadata, freq=="m",trans==0)$code))
monthly.trans0$date = monthly.data.US$date

# Variables with percentage increase
monthly.trans1 = subset(monthly.data.US, 
                        select = tolower(filter(metadata, freq=="m",trans==1)$code))
monthly.trans1 = as.data.frame(apply(monthly.trans1,2,percentage.increase))
monthly.trans1$date = monthly.data.US$date

# Variables with level increase
monthly.trans2 = subset(monthly.data.US, 
                        select = tolower(filter(metadata, freq=="m",trans==2)$code))
monthly.trans2 = as.data.frame(apply(monthly.trans2,2,level.increase))
monthly.trans2$date = monthly.data.US$date

# Variables with level increase of percentage growth
monthly.trans3 = subset(monthly.data.US, 
                        select = tolower(filter(metadata, freq=="m",trans==3)$code))
monthly.trans3 = as.data.frame(apply(monthly.trans3,2,transf3))
monthly.trans3$date = monthly.data.US$date

# Merging all transformations. Filtering 

monthly.transformed = full_join(monthly.trans0,monthly.trans1,by="date") %>%
  full_join(monthly.trans2,by="date") %>%
  full_join(monthly.trans3,by="date") %>%
  full_join(quarterly.data.US1,by="date") %>%
  filter(date >= as.Date("2000-1-1"))
monthly.transformed <- monthly.transformed[, c("date",
                                               setdiff(names(monthly.transformed), "date"))]

### 5.3 Creating factors. Imputting using EM-PCA

empca.m = imputePCA(select(monthly.transformed,-date),
                    ncp = 5,scale = TRUE,method = "Regularized")
empca.m = as.data.frame(empca.m$completeObs)
MPCA = PCA(empca.m,scale.unit = TRUE,graph = FALSE)

economic_factors = as.data.frame(MPCA$ind$coord)
economic_factors$date = monthly.transformed$date
economic_factors = economic_factors[, c(ncol(economic_factors), 1:(ncol(economic_factors)-1))]

# Exporting full dataset of economic variables
empca.m$date = monthly.transformed$date
empca.m = empca.m[, c(ncol(empca.m), 1:(ncol(empca.m)-1))]
write.csv(empca.m,"econ_vars.csv")

#------------------------------------------------------------------------------
### 6 FINAL FEATURES
#------------------------------------------------------------------------------

### 6.0 Summary of information created so far

# Daily data financial information: financial_factors
# Daily data text topics information: text_factors
# Monthly data economic information: economic_factors

### 6.1 Creating additional blocks

metadata = read_excel("Metadata.xlsx",sheet = 5,col_names = TRUE)

# Survey Block: all info coming from surveys
selection =filter(metadata,Survey==1)
names = tolower(selection$INDICATOR)
block.survey = select(empca.m,all_of(names))
pca = PCA(block.survey,scale.unit = TRUE,ncp=1,graph = FALSE)
block.survey = as.data.frame(pca$ind$coord)[,1]

# Labor Block: all info concerning labor market
selection =filter(metadata,Labor==1)
names = tolower(selection$INDICATOR)
block.labor = select(empca.m,all_of(names))
pca = PCA(block.labor,scale.unit = TRUE,ncp = 1,graph = FALSE)
block.labor = as.data.frame(pca$ind$coord)[,1]

# Real Block: all info concerning indicators of economic production
selection =filter(metadata,Real==1)
names = tolower(selection$INDICATOR)
block.real = select(empca.m,all_of(names))
pca = PCA(block.real,scale.unit = TRUE,ncp = 1,graph = FALSE)
block.real = as.data.frame(pca$ind$coord)[,1]

# Price Block: all info concerning prices and inflation
selection =filter(metadata,Price==1)
names = tolower(selection$INDICATOR)
block.price = select(empca.m,all_of(names))
pca = PCA(block.price,scale.unit = TRUE,ncp = 1,graph = FALSE)
block.price = as.data.frame(pca$ind$coord)[,1]

# Merging all blocks to the monthly factors

economic_factors = cbind(economic_factors,
                           block.survey,block.labor,block.real,block.price)

corrplot(cor(select(economic_factors,-date),use = "pairwise.complete.obs", method = "kendall"),
         method = "number", type = "upper")


### 6.2 Clearing memory
objects_to_keep <- c("financial_factors", "text_factors","economic_factors",
                     "ec.data.US","daily.data.US","text_NC","monthly_transformed")
all_objects <- ls()
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)
rm(all_objects,objects_to_remove)

### 6.3 Export factor datasets
write.csv(financial_factors,"financial_factors.csv")
write.csv(text_factors,"text_factors.csv")
write.csv(economic_factors,"economic_factors.csv")




