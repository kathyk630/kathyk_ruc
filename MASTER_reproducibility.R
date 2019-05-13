# This file contains instructions for reproducing the data, all
# analyses, and plots contained in the report.
#Please download the data and add it into your working path.

## Necessary packages
library(readr) 
library(plyr) 
library(dplyr)
library(tidyr) 
library(data.table)
library(tibble) 
library(stringr) 
library(ggplot2) 
library(lubridate)
library(reshape2)
library(grid) 
library(Rmisc)
library(doSNOW)
library(doRNG)
library(foreach)
library(parallel)
library(tseries)
library(forecast)

##########################
setwd("../Code")
##########################
#### EDA & Analysis ######
##########################
source("code1.R")
##########################

#Please run "code2.ipynb" on jupyter notebook
