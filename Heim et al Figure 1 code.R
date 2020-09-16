sizeData <- read.delim("supplementary_data_file.txt")
timescale <- read.delim("timescale.txt")

#convert character vector to factor
phyla <- as.factor(sizeData$phylum)

#extract the levels from the factor vector (in other words, extract the unique phylum)
phyla <- levels(phyla)

#get the number of unique phylum
nPhyla <- length(phyla)

#assign colors to object
myCols <- c("lightpink3", "lightgoldenrod3", "slateblue4", "black", "skyblue2")

#pair each color with each phylum and create new column in original dataframe
sizeData$color <- myCols[match(sizeData$phylum, phyla)]

#create empty dataframe that will hold the mean and quantiles from the loop
trends <- data.frame('interval'=timescale$interval_name, 'mean'=NA, 'fifth'=NA, 'ninetyFifth'=NA)

#calc mean and quantiles using a loop
for(i in 1:nrow(timescale)) {
	temp <- subset(sizeData, fad_age > timescale$age_top[i] & lad_age < timescale$age_bottom[i])
	trends$mean[i] <- mean(temp$log10_volume)
	trends$fifth[i] <- quantile(temp$log10_volume, 0.05)
	trends$ninetyFifth[i] <- quantile(temp$log10_volume, 0.95)
}

dev.new()
#plot using segments() and lines()
plot(timescale$age_mid, type="n", xlab="Geologic time (Ma)", ylab=expression('Biovolume'~('log'[10]~'mm'^3)), xlim=c(500, 450), ylim=range(sizeData$log10_volume), lwd=2)
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume, col=sizeData$color)
lines(timescale$age_mid, trends$mean, lwd=2) #plot the mean values
lines(timescale$age_mid, trends$fifth, lwd=0.75) # plot the 5th quantile
lines(timescale$age_mid, trends$ninetyFifth, lwd=0.75) # plot the 95th quantile
legend(550, 13.2, phyla, xpd = TRUE, horiz = TRUE, bty = "n", pch = 19, col = myCols, cex = 0.85, pt.cex=1.75) #create the legend and place it on top of the plot


