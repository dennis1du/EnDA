Du_TXPUC_HWK1 <- function(month, year) {
  
  #packages loaded 
  library(pdftools)
  library(xml2)
  library(rvest)
  
  #data collecting
  TP_URL <- paste('https://www.puc.texas.gov/industry/electric/rates/RESrate/rate', year, "/", month, year, "Rates.pdf", sep = '')
  download.file(url = TP_URL, destfile = 'the_chosen_one.pdf', mode = 'wb')
  raw_txt <- pdf_text("the_chosen_one.pdf")
  txt <- strsplit(raw_txt, "\n")
  pages <- length(txt)

  x <- matrix(vector(), 0, 5, dimnames=list(c(), c('Plan', 'kWh500', 'kWh1000', 'kWh1500', 'kWh2000')))
  for(i in 1:pages){
    
    lines <- length(txt[[i]])
    
    for(j in 1:lines){
      
      line <- strsplit(txt[[i]][j], split = "  ")
      line <- line[[1]][!line[[1]] == ""]
      
      if(length(line) == 5){
        
        x <- rbind(x, line)
        
      }
    }
  }
  
  #data processing
  x <- as.data.frame(x)
  x$year <- strsplit(year, split = '')[[1]][2]
  x$month <- month
  rownames(x) <- NULL
  x = transform(x, kWh500 = as.numeric(as.character(kWh500)), kWh1000 = as.numeric(as.character(kWh1000)), kWh1500 = as.numeric(as.character(kWh1500)), kWh2000 = as.numeric(as.character(kWh2000)), year = as.numeric(year))
  write.csv(x, file = paste("Du_TXPUC_HWK1_", month, "_", year, ".csv", sep = ""), row.names = FALSE)
  
}