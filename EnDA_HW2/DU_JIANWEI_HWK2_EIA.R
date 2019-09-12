DU_JIANWEI_HWK2_EIA <- function(year) {
  
  #read csv files
  d <- getwd()
  m0 <- read.csv(paste(d, "/ME397_A2_data_csv/F860_", year, ".csv", sep=""), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  n0 <- read.csv(paste(d, "/ME397_A2_data_csv/F923_", year, ".csv", sep=""), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  p0 <- read.csv(paste(d, "/ME397_A2_data_csv/F860_", 2016, ".csv", sep=""), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  
  #initialization
  if (m0[dim(m0)[1],2] == ''){
    m0 <- m0[-dim(m0)[1],]
  }
  p0 <- p0[-dim(p0)[1],]
  p0$Nameplate.Capacity..MW. <- as.numeric(gsub(',', '', as.character(p0$Nameplate.Capacity..MW.)))
  p <- unique(p0[,c('Prime.Mover','Energy.Source.1','Technology')])
  
  #in case of 2007 & 2008
  if (year <= 2008) {
    
    m0$NAMEPLATE <- as.numeric(gsub(',', '', as.character(m0$NAMEPLATE)))
    n0$NET.GENERATION..megawatthours. <- as.numeric(gsub(',', '', as.character(n0$NET.GENERATION..megawatthours.)))
    n0$ELEC.FUEL.CONSUMPTION.MMBTUS <- as.numeric(gsub(',', '', as.character(n0$ELEC.FUEL.CONSUMPTION.MMBTUS)))
    
    m1 <- as.data.frame(aggregate(x = m0$NAMEPLATE, by = list(m0$UTILCODE, m0$PLNTCODE, m0$PRIMEMOVER, m0$ENERGY_SOURCE_1, m0$UTILNAME, m0$PLNTNAME), FUN = sum))
    names(m1) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Nameplate.Capacity')
    
    m2 <- as.data.frame(aggregate(x = m0$OPERATING_YEAR, list(m0$UTILCODE, m0$PLNTCODE, m0$PRIMEMOVER, m0$ENERGY_SOURCE_1, m0$UTILNAME, m0$PLNTNAME), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
    names(m2) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Operating.Year')
    
    m3 <- merge(x = m1, y = m2, by = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name'))
    
    n1 <- as.data.frame(aggregate(x = n0$NET.GENERATION..megawatthours., by = list(n0$Operator.ID, n0$Plant.ID, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n1) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation.MWh')
    n2 <- as.data.frame(aggregate(x = n0$ELEC.FUEL.CONSUMPTION.MMBTUS, by = list(n0$Operator.ID, n0$Plant.ID, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n2) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Elec.Fuel.Consumption.MMBtu')
    
    n3 <- merge(x = n1, y = n2, by = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    
    m <- merge(x = m3, y = n3, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    m <- merge(x = m, y = p, by = c('Prime.Mover','Energy.Source.1'), all.x = T)
    m <- m[, c(3,5,4,11,6,1,2,7,8,9,10)]
    
    f1 <- sum(m$Nameplate.Capacity)/sum(m0$NAMEPLATE)
    f2 <- sum(m$Net.Generation.MWh)/sum(n0$NET.GENERATION..megawatthours.)
    
  } else if ( year <= 2010 ) {  #in case of 2009 & 2010
    
    m0$NAMEPLATE <- as.numeric(gsub(',', '', as.character(m0$NAMEPLATE)))
    n0$NET.GENERATION..megawatthours. <- as.numeric(gsub(',', '', as.character(n0$NET.GENERATION..megawatthours.)))
    n0$ELEC.FUEL.CONSUMPTION.MMBTUS <- as.numeric(gsub(',', '', as.character(n0$ELEC.FUEL.CONSUMPTION.MMBTUS)))
    
    m1 <- as.data.frame(aggregate(x = m0$NAMEPLATE, by = list(m0$UTILITY_ID, m0$PLANT_CODE, m0$PRIME_MOVER, m0$ENERGY_SOURCE_1, m0$UTILITY_NAME, m0$PLANT_NAME), FUN = sum))
    names(m1) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Nameplate.Capacity')
    
    m2 <- as.data.frame(aggregate(x = m0$OPERATING_YEAR, list(m0$UTILITY_ID, m0$PLANT_CODE, m0$PRIME_MOVER, m0$ENERGY_SOURCE_1, m0$UTILITY_NAME, m0$PLANT_NAME), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
    names(m2) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Operating.Year')
    
    m3 <- merge(x = m1, y = m2, by = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name'))
    
    n1 <- as.data.frame(aggregate(x = n0$NET.GENERATION..megawatthours., by = list(n0$Operator.ID, n0$Plant.ID, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n1) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation.MWh')
    n2 <- as.data.frame(aggregate(x = n0$ELEC.FUEL.CONSUMPTION.MMBTUS, by = list(n0$Operator.ID, n0$Plant.ID, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n2) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Elec.Fuel.Consumption.MMBtu')
    
    n3 <- merge(x = n1, y = n2, by = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    
    m <- merge(x = m3, y = n3, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    m <- merge(x = m, y = p, by = c('Prime.Mover','Energy.Source.1'), all.x = T)
    m <- m[, c(3,5,4,11,6,1,2,7,8,9,10)]
    
    f1 <- sum(m$Nameplate.Capacity)/sum(m0$NAMEPLATE)
    f2 <- sum(m$Net.Generation.MWh)/sum(n0$NET.GENERATION..megawatthours.)
  
  } else if ( year == 2011 ) {  #in case of 2011
  
    m0$NAMEPLATE <- as.numeric(gsub(',', '', as.character(m0$NAMEPLATE)))
    n0$Net.Generation..Megawatthours. <- as.numeric(gsub(',', '', as.character(n0$Net.Generation..Megawatthours.)))
    n0$Elec.Fuel.Consumption.MMBtu <- as.numeric(gsub(',', '', as.character(n0$Elec.Fuel.Consumption.MMBtu)))
    
    m1 <- as.data.frame(aggregate(x = m0$NAMEPLATE, by = list(m0$UTILITY_ID, m0$PLANT_CODE, m0$PRIME_MOVER, m0$ENERGY_SOURCE_1, m0$UTILITY_NAME, m0$PLANT_NAME), FUN = sum))
    names(m1) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Nameplate.Capacity')
    
    m2 <- as.data.frame(aggregate(x = m0$OPERATING_YEAR, list(m0$UTILITY_ID, m0$PLANT_CODE, m0$PRIME_MOVER, m0$ENERGY_SOURCE_1, m0$UTILITY_NAME, m0$PLANT_NAME), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
    names(m2) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Operating.Year')
    
    m3 <- merge(x = m1, y = m2, by = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name'))
    
    n1 <- as.data.frame(aggregate(x = n0$Net.Generation..Megawatthours., by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n1) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation.MWh')
    n2 <- as.data.frame(aggregate(x = n0$Elec.Fuel.Consumption.MMBtu, by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n2) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Elec.Fuel.Consumption.MMBtu')
    
    n3 <- merge(x = n1, y = n2, by = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    
    m <- merge(x = m3, y = n3, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    m <- merge(x = m, y = p, by = c('Prime.Mover','Energy.Source.1'), all.x = T)
    m <- m[, c(3,5,4,11,6,1,2,7,8,9,10)]
    
    f1 <- sum(m$Nameplate.Capacity)/sum(m0$NAMEPLATE)
    f2 <- sum(m$Net.Generation.MWh)/sum(n0$Net.Generation..Megawatthours.)
    
  } else if (year <= 2013) {  #in case of 2012 & 2013
  
    m0$Nameplate.Capacity..MW. <- as.numeric(gsub(',', '', as.character(m0$Nameplate.Capacity..MW.)))
    n0$Net.Generation..Megawatthours. <- as.numeric(gsub(',', '', as.character(n0$Net.Generation..Megawatthours.)))
    n0$Elec.Fuel.Consumption.MMBtu <- as.numeric(gsub(',', '', as.character(n0$Elec.Fuel.Consumption.MMBtu)))
    
    m1 <- as.data.frame(aggregate(x = m0$Nameplate.Capacity..MW., by = list(m0$Utility.ID, m0$Plant.Code, m0$Prime.Mover, m0$Energy.Source.1, m0$Utility.Name, m0$Plant.Name), FUN = sum))
    names(m1) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Nameplate.Capacity')
    
    m2 <- as.data.frame(aggregate(x = m0$Operating.Year, list(m0$Utility.ID, m0$Plant.Code, m0$Prime.Mover, m0$Energy.Source.1, m0$Utility.Name, m0$Plant.Name), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
    names(m2) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name', 'Operating.Year')
    
    m3 <- merge(x = m1, y = m2, by = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Plant.Name'))
    
    n1 <- as.data.frame(aggregate(x = n0$Net.Generation..Megawatthours., by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n1) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation.MWh')
    n2 <- as.data.frame(aggregate(x = n0$Elec.Fuel.Consumption.MMBtu, by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n2) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Elec.Fuel.Consumption.MMBtu')
    
    n3 <- merge(x = n1, y = n2, by = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    
    m <- merge(x = m3, y = n3, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    m <- merge(x = m, y = p, by = c('Prime.Mover','Energy.Source.1'), all.x = T)
    m <- m[, c(3,5,4,11,6,1,2,7,8,9,10)]
    
    f1 <- sum(m$Nameplate.Capacity)/sum(m0$Nameplate.Capacity..MW.)
    f2 <- sum(m$Net.Generation.MWh)/sum(n0$Net.Generation..Megawatthours.)
    
  } else if ( year <= 2016) {  #in case of 2014-2016
    
    m0$Nameplate.Capacity..MW. <- as.numeric(gsub(',', '', as.character(m0$Nameplate.Capacity..MW.)))
    n0$Net.Generation..Megawatthours. <- as.numeric(gsub(',', '', as.character(n0$Net.Generation..Megawatthours.)))
    n0$Elec.Fuel.Consumption.MMBtu <- as.numeric(gsub(',', '', as.character(n0$Elec.Fuel.Consumption.MMBtu)))
    
    m1 <- as.data.frame(aggregate(x = m0$Nameplate.Capacity..MW., by = list(m0$Utility.ID, m0$Plant.Code, m0$Prime.Mover, m0$Energy.Source.1, m0$Utility.Name, m0$Technology, m0$Plant.Name), FUN = sum))
    names(m1) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Technology', 'Plant.Name', 'Nameplate.Capacity')
    
    m2 <- as.data.frame(aggregate(x = m0$Operating.Year, by = list(m0$Utility.ID, m0$Plant.Code, m0$Prime.Mover, m0$Energy.Source.1, m0$Utility.Name, m0$Technology, m0$Plant.Name), FUN = function(x) c(avg = mean(x), max = max(x), min = min(x))))
    names(m2) <- c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Technology', 'Plant.Name', 'Operating.Year')
    
    m3 <- merge(x = m1, y = m2, by = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1', 'Utility.Name', 'Technology', 'Plant.Name'))
    
    n1 <- as.data.frame(aggregate(x = n0$Net.Generation..Megawatthours., by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n1) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Net.Generation.MWh')
    n2 <- as.data.frame(aggregate(x = n0$Elec.Fuel.Consumption.MMBtu, by = list(n0$Operator.Id, n0$Plant.Id, n0$Reported.Prime.Mover, n0$Reported.Fuel.Type.Code), FUN = sum))
    names(n2) <- c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code', 'Elec.Fuel.Consumption.MMBtu')
    
    n3 <- merge(x = n1, y = n2, by = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    
    m <- merge(x = m3, y = n3, by.x = c('Utility.ID', 'Plant.Code', 'Prime.Mover', 'Energy.Source.1'), by.y = c('Operator.Id', 'Plant.Id', 'Reported.Prime.Mover', 'Reported.Fuel.Type.Code'))
    m <- m[,c(1,5,2,6,7,3,4,8,9,10,11)]
    
    f1 <- sum(m$Nameplate.Capacity)/sum(m0$Nameplate.Capacity..MW.)
    f2 <- sum(m$Net.Generation.MWh)/sum(n0$Net.Generation..Megawatthours.)
    
}

  #output the results
  print(paste("Nameplate_Capacity in final dataset: ", sprintf("%.2f",f1*100),"%",sep=""))
  print(paste("Net.Generation.MWh in final dataset: ", sprintf("%.2f",f2*100),"%",sep=""))
  
  #create the new csv file
  m$cap_fac <- (m$Net.Generation.MWh)/(m$Nameplate.Capacity * 8760)
  m$Heat.Rate <- (m$Elec.Fuel.Consumption.MMBtu * 1000 )/ (m$Net.Generation.MWh)
  m$Year <- year
  write.csv(x = m, file = paste('DU_JIANWEI_HWK2_EIA_',year,'_DATA.csv', sep = ''), row.names = F)
  
}