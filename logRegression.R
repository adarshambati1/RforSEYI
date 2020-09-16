nematodes <- read.delim("nematodes.txt")

rhabditida <- subset(nematodes, order == "Rhabditida" & ! is.na(length_mm))

rhabditida$logL <- log10(rhabditida$length_mm)
dev.new()
plot(rhabditida$logL, rhabditida$zooparasite, xlab="log biovolume (cubic mm)", ylab="parasitic", pch=16, col=rgb(0.2,0.2,0.2,0.4), cex=1.5)

glm.eqn <-"zooparasite ~ logL"
zoo.glm <- glm(glm.eqn, family=binomial(logit), data=rhabditida)

summary(zoo.glm)

points(rhabditida$logL, zoo.glm$fitted, col="red", pch=16)