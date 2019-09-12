DU_JIANWEI_Rfunction <- function(){
	# data reading
	dir <- getwd()
	eia <- read.csv(paste(dir,"/RHODES_JOSHUA_EIA_2016_data.csv",sep=''), fileEncoding = 'UTF-8-BOM', stringsAsFactors = F)

	# preprocessing
	e0 <- aggregate(eia$Nameplate.Capacity, by = list(round(eia$Operating.Year.avg, -1), eia$Technology), FUN = sum)
	names(e0) <- c('year', 'tech', 'MW_cap')
	e1 <- reshape(data = e0, idvar = "tech", timevar = "year", direction = "wide")
	e1[is.na(e1)] <- 0
	e1$techSum <- rowSums(e1[,-1], na.rm = T)
	e1 <- e1[e1$techSum > 2000,]
	e1 <- e1[,-ncol(e1)]
	e2 <- e1[,-1]
	e2 <- e2[ , order(names(e2))]
	data <- cbind(e1[,1], e2)
	names(data)[1] <- 'tech'

	# visualization and output
	pdf(file = 'DU_JIANWEI_EIA_US_CAPACITY_STACKEDBAR.pdf', height = 6, width = 9)

	set.seed(15)
	cols <- colors()[round(runif(nrow(data), min = 1, max = length(colors())))]

	par(mar = c(5.1, 6.1, 4.1, 2.1))
	mids <- barplot(as.matrix(data[2:14]/1000), las = 1, col = cols, xaxt = 'n', yaxt = 'n', xlab = '')

	axis(side = 1, cex.axis = 1, at = mids, labels = seq(from = 1900, to = 2020, by = 10)) 
	axis(side = 2, cex.axis = 1.5, at = seq(from = 0, to = 250, by = 50), labels = seq(from = 0, to = 250, by = 50), las = 1) 

	mtext(side = 1, cex = 2, line = 3, text = 'Decade ending')
	mtext(side = 2, line = 4, cex = 2, text = 'Capacity (GW)')
	mtext(side = 3, line = 0, cex = 2, text = 'Capacity (GW) added per decade to the\n US grid by type')

	legend('topleft', legend = data$tech, fill = cols, cex = 1, bty = 'n')

	dev.off()
}