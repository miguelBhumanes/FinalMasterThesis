The repository is organized as follows: 

1 Data folder contains*:
- "Metadata.xlsx": file that describes the data and contains information used by "getUSdata.R" to download and create all the regressors of the models. 
- "getUSdata.R": R file that downloads all the data, cleans it and creates the PCA factors used by the Dynamic Factor and Neural Network models. 
- "BloombergExtract": Bloomberg extract with including some of the financial indicators used by "getUSdata.R".
- "OECDUSSurveyData.xlsx": Extract of monthly economic indicators from OECD.Stat for the US. 

2 Final Factors: a folder containing 2 csv files with the created PCA factors for each type of data*. To be used by future researchers that do not want to modify the regressors and want to focus only on comparing different architectures. 

3 "AR.py": Python code to test the stationarity of the dependent variables (GDP growth and CPI inflation) and implement the autoregressive models. 

4 "DFM.R": R code that implements and tests all the Dynamic Factor Models. 

5 "NN.py": Python code that implements and tests all the neural network models. 

6 "requirements.txt": file with all the used R and Python packages, and the version used. This file was not designed to be used directly as a standard requirements.txt files for Python projects. Please, copy and paste the list of Python packages to another file to install all the requirements, and do an equivalent R code to install all the packages.

* Please note that the data corresponding to the text-based indicator is proprietary data of Dr. Argimiro Arratia and is therefore not included in this public repository. 


