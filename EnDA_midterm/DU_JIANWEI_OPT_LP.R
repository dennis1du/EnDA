DU_JIANWEI_OPT_LP <- function(){
  # read data
  dir <- getwd()
  eia <- read.csv(paste(dir,"/RHODES_JOSHUA_EIA_2016_data.csv",sep=''), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)
  fuel <- read.csv(paste(dir,"/fuel_properties.csv",sep=''), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

  # preprocess data
  sub <- c("BIT","BLQ","DFO","LIG","NG","NUC","RC","RFO","SUB","WC","WDS")
  e <- eia[which(eia$Energy.Source.1 %in% sub & eia$Heat.Rate>0 & eia$Heat.Rate<20000 & eia$Nameplate.Capacity>50),]
  f <- fuel[,c(1,3,4)]
  t1 <- c()
  t2 <- c()
  for (i in 1:nrow(e)){
      t1[i] <- e[i,"Elec.Fuel.Consumption.MMBtu"]*f[f$energy_name == e[i,"Energy.Source.1"],"dol_per_mmBtu"]/e[i,"Net.Generation.MWh"]
      t2[i] <- e[i,"Elec.Fuel.Consumption.MMBtu"]*f[f$energy_name == e[i,"Energy.Source.1"],"kg_CO2_per_mmBtu"]/(e[i,"Net.Generation.MWh"]*1000)
  }
  d <- cbind(e, t1, t2)
  names(d)[c(ncol(d) - 1, ncol(d))] <- c("dol_per_MWh", "ton_CO2_per_MWh")

  # set parameters
  D <- 550000
  E <- 350000
  p1 <- rep(1,nrow(d))
  p2 <- d$ton_CO2_per_MWh
  p3 <- rep(0,nrow(d))
  p4 <- d$Nameplate.Capacity
  c <- d$dol_per_MWh

  # lp model
  library(lpSolveAPI)
  model <- make.lp(2,nrow(d))

  set.row(model, 1, p1)
  set.row(model, 2, p2)
  set.rhs(model, c(D, E))
  set.constr.type(model, c(">=","<="))
  set.bounds(model, lower = p3, upper = p4)

  set.objfn(model, c)

  # solution
  solve(model)

  x1 <- get.variables(model)
  part1 <- cbind(d, x1)
  names(part1)[c(4,ncol(part1))] <- c("fuel_type", "solution1")

  obj <- get.objective(model)
  cat(sprintf("The objective of the problem is: $%.2f\n", obj))

  con1 <- get.constraints(model)[1]
  con2 <- get.constraints(model)[2]
  avcost <- obj/550000
  cat(sprintf("The average cost of generation is: $%.2f/MWh\n", avcost))
  cat(sprintf("The total amount of power generated is: %.2fMWh\n", con1))
  cat(sprintf("The total amount of co2 emitted is: %.2fMWh\n", con2))

  cat(sprintf("Table of Generation per Fuel Type\n"))
  sol_fuel <- aggregate(solution1~fuel_type, data = part1, FUN = sum)
  print(sol_fuel)

  # visualization
  library(ggplot2)
  results2 <- data.frame(Eg = E, Cost = avcost)
  for (Eg in seq(E - 10000, 230000, by = -10000)){
    set.rhs(model,Eg,2)
    solve(model)
    y <- get.objective(model)/550000
    df <- c(Eg,y)
    results2 <- rbind(results2,df)
  }
  results2 <- results2[order(results2$Eg),]

  x2 <- get.variables(model)
  part2 <- cbind(part1, x2)
  names(part2)[ncol(part2)] <- "solution2"
  results1 <- aggregate(solution2 - solution1~fuel_type, data = part2, FUN = sum)
  names(results1)[2] <- "change"

  df <- transform(results1, judge = ifelse(change>0, "Y", "N"))
  g1 <- ggplot(df, aes(x = fuel_type, y = change, fill = judge)) + 
    geom_bar(stat = "identity") + 
    theme(legend.position = "") + 
    scale_fill_manual(values = c("blue", "red")) + 
    xlab("Fuel Type") + ylab("Changes in Dispatch from High to Low Emission Cap")
  ggsave(file = "DU_JIANWEI_FUEL_CHANGE.pdf", height = 6, width = 9)

  g2 <- ggplot(data = results2, aes(x = Eg, y = Cost, color = 'red')) + 
    geom_line() + ylim(c(0,30)) + 
    theme(legend.position = "none") + 
    xlab('Emission Cap(Tons)') + ylab('Dispatch Cost ($/MWh)')
  ggsave(file = 'DU_JIANWEI_COST_CHANGE.png', height = 6, width = 9)
}