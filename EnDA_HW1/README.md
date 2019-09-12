# EnDA_Coursework project \#1

## 1.Du_WU_HWK1.R
### Description: 
A function in R, downloads and formats data from the WeatherUnder-ground website given inputs: month, day, year, and station ID. 

### Input
Du_WU_HWK1(month=mm, day=dd, year=yyyy, station_id='xxx')

### Output
dataframe that has columns for time, relative humidity (RH), temperature (&deg;C),dew-point temperature (&deg;C), wind speed (m/s), and wet-bulb temperature (&deg;C).

## 2.Du_TXPUC_HWK1.R
### Description: 
A function in R scrapes all of the PDFs linked from 【Texas Public Utilities Commission website](https://www.puc.texas.gov/industry/electric/rates/RESrate/RESratearc.aspx), and creates a dataframe of all the results.

### Input
Du_TXPUC_HWK1(month='M', year='YY')

### Output
a csv file of the cleaned results from a single month
