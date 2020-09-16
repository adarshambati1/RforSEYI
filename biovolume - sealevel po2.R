dev.new()
#plot using segments() and lines()
plot(timescale$age_mid, type="n", xlab="Geologic time (Ma)", ylab=expression('Biovolume'~('log'[10]~'mm'^3)), xlim=c(max(timescale$age_bottom), min(timescale$age_bottom)), ylim=range(log10(class$calc_max_vol)), lwd=2)
segments(ceph$fad_age, log10(ceph$max_vol), ceph$lad_age, log10(ceph$max_vol), col="light blue")
lines(timescale$age_mid, trends$mean, lwd=2) #plot the mean values
lines(timescale$age_mid, trends$fifth, lwd=0.75) # plot the 5th quantile
lines(timescale$age_mid, trends$ninetyFifth, lwd=0.75) # plot the 95th quantile
#legend(550, 13.2, phyla, xpd = TRUE, horiz = TRUE, bty = "n", pch = 19, col = myCols, cex = 0.85, pt.cex=1.75) #create the legend and place it on top of the plot
par(new = TRUE)
plot(timescale$SeaLevel, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ",xlim=c(max(timescale$age_bottom), min(timescale$age_bottom)), ylim=range(timescale$SeaLevel))
axis(side=4, labels=TRUE)
mtext("Sea Level", side=4, line=3)
lines(timescale$age_mid, timescale$SeaLevel, lwd=2, col="pink")
legend("top", c("biovolume", "sea levels"),
       col = c("black", "pink"), lty = c(1, 1))

dev.new()
#plot using segments() and lines()
plot(timescale$age_mid, type="n", xlab="Geologic time (Ma)", ylab=expression('Biovolume'~('log'[10]~'mm'^3)), xlim=c(max(timescale$age_bottom), min(timescale$age_bottom)), ylim=range(log10(class$calc_max_vol)), lwd=2)
segments(ceph$fad_age, log10(ceph$max_vol), ceph$lad_age, log10(ceph$max_vol), col="light blue")
lines(timescale$age_mid, trends$mean, lwd=2) #plot the mean values
lines(timescale$age_mid, trends$fifth, lwd=0.75) # plot the 5th quantile
lines(timescale$age_mid, trends$ninetyFifth, lwd=0.75) # plot the 95th quantile
#legend(550, 13.2, phyla, xpd = TRUE, horiz = TRUE, bty = "n", pch = 19, col = myCols, cex = 0.85, pt.cex=1.75) #create the legend and place it on top of the plot
par(new = TRUE)
plot(timescale$pO2, type="n", xaxt="n", yaxt="n", xlab=" ", ylab=" ",xlim=c(max(timescale$age_bottom), min(timescale$age_bottom)), ylim=range(timescale$pO2))
axis(side=4, labels=TRUE)
mtext("pO2", side=4, line=3)
lines(timescale$age_mid, timescale$pO2, lwd=2, col="pink")
legend("top", c("biovolume", "pO2"),
       col = c("black", "pink"), lty = c(1, 1))