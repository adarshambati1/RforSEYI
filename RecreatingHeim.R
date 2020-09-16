sizeData <- read.delim("supplementary_data_file.txt")
timescale <- read.delim("timescale.txt")
dev.new()
plot(timescale$age_mid, type="n", xlab="Geologic time (Ma)", ylab=expression('Biovolume'~('log'[10]~'mm'^3)), xlim=c(max(timescale$age_bottom), 0), ylim=range(sizeData$log10_volume), lwd=2)
segments(sizeData$fad_age, sizeData$log10_volume, sizeData$lad_age, sizeData$log10_volume)

