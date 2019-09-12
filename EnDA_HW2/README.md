# EnDA_Coursework project \#2
## EIA Data Cleaning: DU_JIANWEI_HWK2_EIA
### Description: 
A function in R, for data cleaning and data processing. In detail, merge and clean EIA Form 860 and EIA Form 923 from 2007 to 2016. Given input year, the function reads in specific csv data sheets and outputs a clean data sheet and comparisons from the original data. 

### Input
DU_JIANWEI_HWK2_EIA <- function(year)

### Output
- Print comparisons as Nameplate_Capacity% and Net.Generation.MWh%.
- A clean data sheet named 'DU_JIANWEI_HWK2_EIA_year_DATA.csv'

### Tip
- New variable: Capacity.factor = Net.Generation.MWh / (Nameplate.Capacity*8760)
- Unit transform: Heat.Rate = Elec.Fuel.Consumption.MMBtu * 1000 / Net.Generation.MWh
- The "Technology" column is only availiable in the F860 data from 2014 to 2016. Thus for 2007-2013 F860 data you will need to create the column. Hint: Using the 2016 data, get unique combinations of prime mover, energy source, and Technology then merge that with other years that don't have the Technology column in the 860 data.