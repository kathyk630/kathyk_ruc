# kathyk_ruc
## Advanced Applied Stats - Project
### Introduction
This project focuses on solving the problem of predicting the future web traffic for approximately 145,000 Wikipedia articles. Detailed data description is covered in the following section. Making future prediction on sequential or temporal observations has emerged in many key real-world problems. By forecasting the future values of multiple web traffic time series, we can answer some questions like how many severs you need in reality and what your total cost for next month is when you need to use external severs. If the performance is satisfactory, similar methods can be applied to other websites to predict their web traffic, and it can help people make smart advertisement decisions and make profit.

### Data Availability
Unfortunately, the dataset is too big to be uploaded. Here is the [link](https://www.kaggle.com/c/web-traffic-time-series-forecasting/data) to download data. 

### Code
Data processing and ARIMA fitting are done with R, and LSTM fitting is in Python.

### Optional Information
R version 3.4.0 was used for the analysis of this project. R packages including readr, plyr, dplyr, tidyr, data.table, tibble, stringr, ggplot2, lubridate, reshape2, doSNOW, doRNG, foreach, parallel, tseries, forecast.

### Shiny
Since the dataset is really big, it takes the shinyapps.io quite a while to generate corresponding figures, and the shinyapps.io may disconnect from the server when in the bad luck (actually, this often happens). So please download the shinyApp.zip, unzip it, and run it on the RStudio by using code runApp("~/shinyApp") to see those interactive figures. 

## Instruction for Use
### Reproducibility
All data processing and analyses are reproducible, as well as all figures in the report.

Thanks.
