data <- read.delim("ordovician_data.txt", header=TRUE)
timescale <-read.delim("ordovician_timescale.txt")
ecospacedata<-read.csv("ecospace.csv")

trends <- data.frame('interval'=timescale$interval_name, 'mean'=NA, 'fifth'=NA, 'ninetyFifth'=NA)
molluscs <- subset(data,data$phylum=="Mollusca" )
ceph <- subset(data,data$class=="Cephalopoda" )
#Checking For Normal between Log Volume and Log Length
dev.new()
hist(log10(molluscs$calc_max_vol), breaks = 20) # approx. normal
dev.new()
hist(log10(molluscs$max_length), breaks = 20) # approx. normal
plot(log10(molluscs$max_length)~log10(molluscs$calc_max_vol))
lenvol<-lm(log10(molluscs$max_length)~log10(molluscs$calc_max_vol)) # checking for R^2 value (really high)
summary(lenvol) #Checking for weird residuals
par(mfrow=c(2,2))
plot(lenvol)
par(mfrow=c(1,1))


#Analysis
LOMEceph<-subset(ceph, ceph$lad_age<=467.3 & ceph$lad_age >=443.4)
hist(LOMEceph$lad_age, breaks = c(467.3,458.4, 453.0,445.2,443.4), axis =FALSE, ylim=c(0,67), xlab="Geologic Time", ylab ="Frequency", main = "Histogram of Cephalopod Extinction",  xlim = c(467.3, 443.4), freq =TRUE, labels = c("Hirn.", "Katian", "Sandbian", "Darriwilian"))
ceph$logvol<-log10(ceph$calc_max_vol)

gas<-subset(data,data$class=="Gastropoda")
gas$logvol<-log10(gas$calc_max_vol)

biv<-subset(data,data$class=="Bivalvia")
biv$logvol<-log10(biv$calc_max_vol)


#---------------------------------------------- Main THree
analysisgas <- data.frame("Name"=timescale$interval_name,"Mean"=0,"Age_Bottom"=NA, "Mid_Age"=NA, "Age_Top"=NA, "Odds" =NA, "nGenera"=NA, "nExtinct" =NA)
for (i in 1:length(timescale)){
  temp<-subset(gas, gas$fad_age >=timescale$age_top[i] & gas$lad_age<=timescale$age_bottom[i])
  temp$extinct<-0
  temp$extinct[temp$lad_age==timescale$age_bottom[i]]<-1
  size<-temp$logvol
  glmod10<-glm(temp$extinct~size, data=temp, family="binomial", maxit=100)
  analysisgas$Odds[i] <-summary(glmod10)$coefficients[2,1]
  analysisgas$StdErr[i] <-summary(glmod10)$coefficients[2,2]
  analysisgas$nGenera[i]<-nrow(temp)
  analysisgas$Age_Bottom[i]<-timescale$age_bottom[i]
  analysisgas$Mid_Age[i]<-timescale$age_mid[i]
  analysisgas$Age_Top[i]<-timescale$age_top[i]
  analysisgas$Mean[i]<-mean(temp$logvol)
  analysisgas$Fifty[i]<-quantile(temp$logvol,0.05)
  analysisgas$NinetyFive[i]<-quantile(temp$logvol,0.95)
  analysisgas$nExtinct[i]<-sum(temp$extinct)
  analysisgas$Percent_Ex[i]<-(nrow(temp[temp$extinct==1,])/nrow(temp))*100
}
analysisbiv <- data.frame("Name"=timescale$interval_name,"Mean"=0,"Age_Bottom"=NA, "Mid_Age"=NA, "Age_Top"=NA, "Odds" =NA, "nGenera"=NA, "nExtinct" =NA)
for (i in 1:length(timescale)){
  temp<-subset(biv, biv$fad_age >=timescale$age_top[i] & biv$lad_age<=timescale$age_bottom[i])
  temp$extinct<-0
  temp$extinct[temp$lad_age==timescale$age_bottom[i]]<-1
  size<-temp$logvol
  glmod12<-glm(temp$extinct~size, data=temp, family="binomial", maxit=100)
  analysisbiv$Odds[i] <-summary(glmod12)$coefficients[2,1]
  analysisbiv$StdErr[i] <-summary(glmod12)$coefficients[2,2]
  analysisbiv$nGenera[i]<-nrow(temp)
  analysisbiv$Age_Bottom[i]<-timescale$age_bottom[i]
  analysisbiv$Mid_Age[i]<-timescale$age_mid[i]
  analysisbiv$Age_Top[i]<-timescale$age_top[i]
  analysisbiv$Mean[i]<-mean(temp$logvol)
  analysisbiv$Fifty[i]<-quantile(temp$logvol,0.05)
  analysisbiv$NinetyFive[i]<-quantile(temp$logvol,0.95)
  analysisbiv$nExtinct[i]<-sum(temp$extinct)
  analysisbiv$Percent_Ex[i]<-(nrow(temp[temp$extinct==1,])/nrow(temp))*100
}


analysis <- data.frame("Name"=timescale$interval_name,"Mean"=0,"Age_Bottom"=NA, "Mid_Age"=NA, "Age_Top"=NA, "Odds" =NA, "nGenera"=NA, "nExtinct" =NA)
for (i in 1:length(timescale)){
  temp<-subset(ceph, ceph$fad_age >=timescale$age_top[i] & ceph$lad_age<=timescale$age_bottom[i])
  temp$extinct<-0
  temp$extinct[temp$lad_age==timescale$age_bottom[i]]<-1
  size<-temp$logvol
  glmod<-glm(temp$extinct~size, data=temp, family="binomial", maxit=100)
  analysis$Odds[i] <-summary(glmod)$coefficients[2,1]
  analysis$StdErr[i] <-summary(glmod)$coefficients[2,2]
  analysis$nGenera[i]<-nrow(temp)
  analysis$Age_Bottom[i]<-timescale$age_bottom[i]
  analysis$Mid_Age[i]<-timescale$age_mid[i]
  analysis$Age_Top[i]<-timescale$age_top[i]
  analysis$Mean[i]<-mean(temp$logvol)
  analysis$Fifty[i]<-quantile(temp$logvol,0.05)
  analysis$NinetyFive[i]<-quantile(temp$logvol,0.95)
  analysis$nExtinct[i]<-sum(temp$extinct)
  analysis$Percent_Ex[i]<-(nrow(temp[temp$extinct==1,])/nrow(temp))*100
}


labelss<-c("Tre","Flo","Dap","Dar","San","Kat","Hir")
dev.new()
jpeg(file = "Cephalopoda.jpeg", width = 960, height =480)   # The directory you want to save the file in

plot(analysis$Odds~analysis$Mid_Age, pch=16, main="Extinction Risk as a Function of Cephalopoda Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysis$Odds[4:7]~analysis$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysis$Odds[1:3]~analysis$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysis$Mid_Age, analysis$Odds+1.96*analysis$StdErr,
       analysis$Mid_Age, analysis$Odds-1.96*analysis$StdErr, 
       angle=90, length=0.001, lwd=1.5) 
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysis$Mid_Age[i], analysis$Odds[i],
         analysis$Mid_Age[z], analysis$Odds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient for Extinction Risk", line=2.5)
mtext(side = 1, "Smaller Body Size", padj = -2 , cex = 0.75, font =2)
mtext(side = 3, "Larger Body Size", padj = 2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Cephalopods during the LOME", "Cephalopods prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))
dev.off()




cor.test(timescale$pO2, analysis$Odds)
cor.test(timescale$SeaLevel, analysis$Odds)

molluscs[molluscs$circ == "closed",]$circ = 1
molluscs[molluscs$circ == "open",]$circ = 0
molluscs$circ<-as.factor(molluscs$circ)
molluscs$logvol<-log10(molluscs$calc_max_vol)
analysismolluscs <- data.frame("Name"=timescale$interval_name,"Mean"=0,"Age_Bottom"=NA, "Mid_Age"=NA, "Age_Top"=NA, "nGenera"=NA, "nExtinct" =NA)
for (i in 1:length(timescale)){
  temp2<-subset(molluscs, molluscs$fad_age >=timescale$age_top[i] & molluscs$lad_age<=timescale$age_bottom[i])
  temp2$extinct<-0
  temp2$extinct[temp2$lad_age==timescale$age_bottom[i]]<-1
  size<-temp2$logvol
  size2<-temp3$logvol
  glmod2<-glm(temp2$extinct~size, data=temp2, family="binomial", maxit=100)
  glmod3<-glm(temp2$extinct~circ, data=temp2, family="binomial", maxit=100)
  glmod4<-glm(circ~size, data=temp2, family="binomial",maxit=100)
  analysismolluscs$CircvSize[i]<-summary(glmod4)$coefficients[2,1]
  analysismolluscs$CircvSizeSTD[i]<-summary(glmod4)$coefficients[2,2]
  
  analysismolluscs$CircvExtinctOdds[i]<- summary(glmod3)$coefficients[2,1]
  analysismolluscs$CircvExtinctSTD[i]<- summary(glmod3)$coefficients[2,2]
  
  analysismolluscs$ExtinctvOdds[i] <-summary(glmod2)$coefficients[2,1]
  analysismolluscs$ExtinctvOddsSTD[i] <-summary(glmod2)$coefficients[2,2]
  
  analysismolluscs$nGenera[i]<-nrow(temp2)
  analysismolluscs$Age_Bottom[i]<-timescale$age_bottom[i]
  analysismolluscs$Mid_Age[i]<-timescale$age_mid[i]
  analysismolluscs$Age_Top[i]<-timescale$age_top[i]
  analysismolluscs$Mean[i]<-mean(temp2$logvol)
  analysismolluscs$Fifty[i]<-quantile(temp2$logvol,0.05)
  analysismolluscs$NinetyFive[i]<-quantile(temp2$logvol,0.95)
  analysismolluscs$nExtinct[i]<-sum(temp2$extinct)
  analysismolluscs$Percent_Ex[i]<-(nrow(temp2[temp2$extinct==1,])/nrow(temp2))*100
  analysismolluscs$Percent_Closed[i]<-(nrow(temp2[temp2$circ==1,])/nrow(temp2))*100
}

cor.test(timescale$pO2, analysismolluscs$ExtinctvOdds)
cor.test(timescale$SeaLevel, analysismolluscs$ExtinctvOdds)

cor.test(analysismolluscs$CircvExtinctOdds, timescale$pO2)
cor.test(analysismolluscs$CircvExtinctOdds, timescale$SeaLevel)
cor.test(timescale$pO2, analysismolluscs$CircvSize)
cor.test(timescale$SeaLevel, analysismolluscs$CircvSize)
#
molluscs$tiering2<-molluscs$tiering
molluscs$tiering2[molluscs$tiering>=2] <-0
molluscs$motility2<-molluscs$motility
molluscs$motility2[molluscs$motility>=3]<-0
molluscs$motility2[molluscs$motility<3]<-1
molluscs$feeding2<-molluscs$feeding
molluscs$feeding2[molluscs$feeding==5]<-1
molluscs$feeding2[molluscs$feeding!=5]<-0
tier <-as.data.frame(subset(molluscs, molluscs$tiering2<=1))
mot <-as.data.frame(subset(molluscs, molluscs$motility2<=1))
feed <-as.data.frame(subset(molluscs, molluscs$feeding2<=1))
for (i in 1:length(timescale)){
  temp4<-subset(tier, tier$fad_age>=timescale$age_top[i] & tier$lad_age<=timescale$age_bottom[i])
  temp5<-subset(mot, mot$fad_age>=timescale$age_top[i] & mot$lad_age<=timescale$age_bottom[i])
  temp6<-subset(feed, feed$fad_age>=timescale$age_top[i] & feed$lad_age<=timescale$age_bottom[i])
  temp4$extinct<-0
  temp5$extinct<-0
  temp6$extinct<-0
  temp4$extinct[temp4$lad_age==timescale$age_bottom[i]]<-1
  temp5$extinct[temp5$lad_age==timescale$age_bottom[i]]<-1
  temp6$extinct[temp6$lad_age==timescale$age_bottom[i]]<-1
  glmod7<-glm(extinct~tiering2,data=temp4, family="binomial", maxit=100)
  glmod8<-glm(extinct~motility2,data=temp5, family="binomial", maxit=100)
  glmod9<-glm(extinct~feeding2,data=temp6, family="binomial", maxit=100)
  analysismolluscs$TieringvExtinctOdds[i] <-summary(glmod7)$coefficients[2,1]
  analysismolluscs$TieringvExtinctSTD[i] <-summary(glmod7)$coefficients[2,2]
  analysismolluscs$MotilityvExtinctOdds[i] <-summary(glmod8)$coefficients[2,1]
  analysismolluscs$MotilityvExtinctSTD[i] <-summary(glmod7)$coefficients[2,2]
  analysismolluscs$FeedingvExtinctOdds[i] <-summary(glmod9)$coefficients[2,1]
  analysismolluscs$FeedingvExtinctSTD[i] <-summary(glmod7)$coefficients[2,2]
  
}

for (i in 1:length(timescale)){
  temp4<-subset(tier, tier$fad_age>=timescale$age_top[i] & tier$lad_age<=timescale$age_bottom[i])
  temp5<-subset(mot, mot$fad_age>=timescale$age_top[i] & mot$lad_age<=timescale$age_bottom[i])
  temp6<-subset(feed, feed$fad_age>=timescale$age_top[i] & feed$lad_age<=timescale$age_bottom[i])
  size1 <-temp4$logvol
  size2 <-temp5$logvol
  size3 <-temp6$logvol
  glmod7<-glm(tiering2~size1,data=temp4, family="binomial", maxit=100)
  glmod8<-glm(motility2~size2,data=temp5, family="binomial", maxit=100)
  glmod9<-glm(feeding2~size3,data=temp6, family="binomial", maxit=100)
  analysismolluscs$TieringvBDSOdds[i] <-summary(glmod7)$coefficients[2,1]
  analysismolluscs$TieringvBDSSTD[i] <-summary(glmod7)$coefficients[2,2]
  analysismolluscs$MotilityvBDSOdds[i] <-summary(glmod8)$coefficients[2,1]
  analysismolluscs$MotilityvBDSSTD[i] <-summary(glmod7)$coefficients[2,2]
  
  analysismolluscs$FeedingvBDSOdds[i] <-summary(glmod9)$coefficients[2,1]
  analysismolluscs$FeedingvBDSSTD[i] <-summary(glmod7)$coefficients[2,2]
  
}
cor.test(analysismolluscs$Mean, timescale$pO2)
cor.test(analysismolluscs$FeedingvBDSOdds, timescale$pO2)
cor.test(analysismolluscs$MotilityvBDSOdds, timescale$pO2)
cor.test(analysismolluscs$TieringvBDSOdds, timescale$pO2)
cor.test(analysismolluscs$FeedingvBDSOdds, timescale$SeaLevel)
cor.test(analysismolluscs$MotilityvBDSOdds, timescale$SeaLevel)
cor.test(analysismolluscs$TieringvBDSOdds, timescale$SeaLevel)

cor.test(analysismolluscs$FeedingvExtinctOdds, timescale$pO2)
cor.test(analysismolluscs$MotilityvExtinctOdds, timescale$pO2)
cor.test(analysismolluscs$TieringvExtinctOdds, timescale$pO2)
cor.test(analysismolluscs$FeedingvExtinctOdds, timescale$SeaLevel)
cor.test(analysismolluscs$MotilityvExtinctOdds, timescale$SeaLevel)
cor.test(analysismolluscs$TieringvExtinctOdds, timescale$SeaLevel)

dev.new()
pdf(file = "Molluscs.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 6) # The height of the plot in inches
par(mfrow=c(2,2))
plot(analysismolluscs$ExtinctvOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Mollusca Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$ExtinctvOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1, col ="gray30")
points(analysismolluscs$ExtinctvOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1, col ="gray30")
arrows(analysismolluscs$Mid_Age, analysismolluscs$ExtinctvOdds+1.96*analysismolluscs$ExtinctvOddsSTD,
       analysismolluscs$Mid_Age, analysismolluscs$ExtinctvOdds-1.96*analysismolluscs$ExtinctvOddsSTD, 
       angle=90, length=0.001, lwd=1.5, col ="gray30") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$ExtinctvOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$ExtinctvOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}
axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Extinction Risk Log. Reg. Coefficient", line=2.5)
mtext(side = 1, "Smaller Body Size", padj = -2 , cex = 0.75, font =2)
mtext(side = 3, "Larger Body Size", padj = 2 , cex = 0.75, font =2)
legend("bottomright", legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("gray30", "gray30"), lty=1, cex=0.8, pch=c(17,2))

plot(analysismolluscs$ExtinctvOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Cephalopoda Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")

points(analysis$Odds[4:7]~analysis$Mid_Age[4:7], pch=0,
       xaxt="n", xlab="", ylab="", las=1, col ="red")
points(analysis$Odds[1:3]~analysis$Mid_Age[1:3], pch=15,
       xaxt="n", xlab="", ylab="", las=1, col ="red")
arrows(analysis$Mid_Age, analysis$Odds+1.96*analysis$StdErr,
       analysis$Mid_Age, analysis$Odds-1.96*analysis$StdErr, 
       angle=90, length=0.001, lwd=1.5,col ="red") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysis$Odds[i],
         analysismolluscs$Mid_Age[z], analysis$Odds[z], 
         angle=90, length=0.001, lwd=1.5, col ="red", lty="dashed")
}
axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Extinction Risk Log. Reg. Coefficient", line=2.5)
mtext(side = 1, "Smaller Body Size", padj = -2 , cex = 0.75, font =2)
mtext(side = 3, "Larger Body Size", padj = 2 , cex = 0.75, font =2)
legend("bottomright", legend=c("Cephalopods during the LOME", "Cephalopods prior to the LOME"),
       col=c("red", "red"), lty=1, cex=0.8, pch=c(15,0))

plot(analysismolluscs$ExtinctvOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Bivalvia Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysisbiv$Odds[4:7]~analysisbiv$Mid_Age[4:7], pch=1,
       xaxt="n", xlab="", ylab="", las=1, col ="blue")
points(analysisbiv$Odds[1:3]~analysisbiv$Mid_Age[1:3], pch=16,
       xaxt="n", xlab="", ylab="", las=1, col ="blue")
arrows(analysisbiv$Mid_Age, analysisbiv$Odds+1.96*analysisbiv$StdErr,
       analysisbiv$Mid_Age, analysisbiv$Odds-1.96*analysisbiv$StdErr, 
       angle=90, length=0.001, lwd=1.5,col ="blue") 
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysisbiv$Odds[i],
         analysismolluscs$Mid_Age[z], analysisbiv$Odds[z], 
         angle=90, length=0.001, lwd=1.5, col ="blue", lty="dashed")
}
axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Extinction Risk Log. Reg. Coefficient", line=2.5)
mtext(side = 1, "Smaller Body Size", padj = -2 , cex = 0.75, font =2)
mtext(side = 3, "Larger Body Size", padj = 2 , cex = 0.75, font =2)
legend("bottomright", legend=c("Bivalves during the LOME", "Bivalves prior to the LOME"),
       col=c("blue", "blue"), lty=1, cex=0.8, pch=c(16,1))

plot(analysismolluscs$ExtinctvOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Gastropoda Body Size",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysisgas$Odds[4:7]~analysisgas$Mid_Age[4:7], pch=5,
       xaxt="n", xlab="", ylab="", las=1, col ="green")
points(analysisgas$Odds[1:3]~analysisgas$Mid_Age[1:3], pch=18,
       xaxt="n", xlab="", ylab="", las=1, col ="green")
arrows(analysisgas$Mid_Age, analysisgas$Odds+1.96*analysisgas$StdErr,
       analysisgas$Mid_Age, analysisgas$Odds-1.96*analysisgas$StdErr, 
       angle=90, length=0.001, lwd=1.5, col ="green") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysisgas$Odds[i],
         analysismolluscs$Mid_Age[z], analysisgas$Odds[z], 
         angle=90, length=0.001, lwd=1.5, col ="green", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Extinction Risk Log. Reg. Coefficient", line=2.5)
mtext(side = 1, "Smaller Body Size", padj = -2 , cex = 0.75, font =2)
mtext(side = 3, "Larger Body Size", padj = 2 , cex = 0.75, font =2)
legend("bottomright", legend=c("Gastropods during the LOME", "Gastropods prior to the LOME"),
       col=c("green", "green"), lty=1, cex=0.8, pch=c(18,5))

dev.off()

#__________________________________________________
dev.new()
jpeg(file = "MolluscExtinctionvsSeaandpO2.jpeg", width = 960, height =480)   # The directory you want to save the file in
par(mfrow=c(1,2))
plot(analysismolluscs$Percent_Ex~timescale$age_mid,xlim=c(485.4,443.4), ylim=c(0,80),type="n", main="Extinction Rates for Molluscs and pO2 Levels Over Ordovician Stages", xlab="Geologic Time across the Ordovician in MYA", ylab="Percent Extinction")
points(analysismolluscs$Percent_Ex~timescale$age_mid, pch=2,
       xaxt="n", xlab="", ylab="", las=1,col ="yellowgreen")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$Percent_Ex[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="yellowgreen", lty="solid")
}
points(analysis$Percent_Ex~timescale$age_mid, pch=10,
       xaxt="n", xlab="", ylab="", las=1,  col ="firebrick")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysis$Mid_Age[i], analysis$Percent_Ex[i],
         analysis$Mid_Age[z], analysis$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="firebrick", lty="dashed")
}
points(analysisgas$Percent_Ex~timescale$age_mid, pch=15,
       xaxt="n", xlab="", ylab="", las=1,  col ="darkorange")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisgas$Mid_Age[i], analysisgas$Percent_Ex[i],
         analysisgas$Mid_Age[z], analysisgas$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="darkorange", lty="dashed")
}
points(analysisbiv$Percent_Ex~timescale$age_mid, pch=23,
       xaxt="n", xlab="", ylab="", las=1,  col ="mediumpurple4")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisbiv$Mid_Age[i], analysisbiv$Percent_Ex[i],
         analysisbiv$Mid_Age[z], analysisbiv$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="mediumpurple4", lty="solid")
}
par(new = TRUE)#This code specifies that we want to overlay a second plot.
plot(timescale$pO2~timescale$age_mid, pch = 17, col ="red", axes = FALSE, xlab = '', ylab = '', xlim =c(485.4,443.4), ylim=c(15,21)) # This code draws the second plot (i.e. the green triangles).
axis(side = 4, labels=TRUE, col="red") # This code adds the axis labels on the right side.
mtext("pO2 in Percent of Atmosphere at Each Stage", side =4, padj=-2, outer=FALSE)
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisbiv$Mid_Age[i], timescale$pO2[i],
         analysisbiv$Mid_Age[z], timescale$pO2[z], 
         angle=90, length=0.001, lwd=1.5, col ="red", lty="solid")
}
legend("top", legend=c("Molluscan Extinction Rate", "Cephalopodan Extinction Rate", "Gastropodan Extinction Rate", "Bivalvian Extinction Rate","pO2 Level" ),
       col=c("yellowgreen","firebrick","darkorange" , "mediumpurple4", "red"), lty=c("solid", "dashed", "dashed", "solid","solid"), cex=0.8, pch=c(2,10, 15,23,17))


plot(analysismolluscs$Percent_Ex~timescale$age_mid,xlim=c(485.4,443.4), ylim=c(0,80),type="n",main="Extinction Rates for Molluscs And Sea Levels Over Ordovician Stages",  xlab="Geologic Time across the Ordovician in MYA", ylab="Percent Extinction")
points(analysismolluscs$Percent_Ex~timescale$age_mid, pch=2,
       xaxt="n", xlab="", ylab="", las=1,col ="yellowgreen")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$Percent_Ex[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="yellowgreen", lty="solid")
}
points(analysis$Percent_Ex~timescale$age_mid, pch=10,
       xaxt="n", xlab="", ylab="", las=1,  col ="firebrick")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysis$Mid_Age[i], analysis$Percent_Ex[i],
         analysis$Mid_Age[z], analysis$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="firebrick", lty="dashed")
}
points(analysisgas$Percent_Ex~timescale$age_mid, pch=15,
       xaxt="n", xlab="", ylab="", las=1,  col ="darkorange")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisgas$Mid_Age[i], analysisgas$Percent_Ex[i],
         analysisgas$Mid_Age[z], analysisgas$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="darkorange", lty="dashed")
}
points(analysisbiv$Percent_Ex~timescale$age_mid, pch=23,
       xaxt="n", xlab="", ylab="", las=1,  col ="mediumpurple4")
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisbiv$Mid_Age[i], analysisbiv$Percent_Ex[i],
         analysisbiv$Mid_Age[z], analysisbiv$Percent_Ex[z], 
         angle=90, length=0.001, lwd=1.5, col ="mediumpurple4", lty="solid")
}
par(new = TRUE)#This code specifies that we want to overlay a second plot.
plot(timescale$SeaLevel~timescale$age_mid, pch = 17, col ="red", axes = FALSE, xlab = '', ylab = '',xlim =c(485.4,443.4), ylim=c(75,155)) # This code draws the second plot (i.e. the green triangles).
axis(side = 4, col="red", labels = TRUE) # This code adds the axis labels on the right side.
mtext("Sea Level in Meters Relative to Modern Sea Levels", side =4, padj=-2, outer=FALSE)
for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysisbiv$Mid_Age[i], timescale$SeaLevel[i],
         analysisbiv$Mid_Age[z], timescale$SeaLevel[z], 
         angle=90, length=0.001, lwd=1.5, col ="red", lty="solid")
}
legend("top", legend=c("Molluscan Extinction Rate", "Cephalopodan Extinction Rate", "Gastropodan Extinction Rate", "Bivalvian Extinction Rate","Sea Level" ),
       col=c("yellowgreen","firebrick","darkorange" , "mediumpurple4", "red"), lty=c("solid", "dashed", "dashed", "solid","solid"), cex=0.8, pch=c(2,10, 15,23,17))

dev.off()

#__________________________________________________




dev.new()
jpeg(file = "Tiering.jpeg", width = 960, height =480)   # The directory you want to save the file in

par(mfrow=c(1,2))
plot(analysismolluscs$TieringvBDSOdds~analysismolluscs$Mid_Age, pch=16, main="Body Size as a Function of Mollucsca Tiering Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, xlim =c(485.4,443.4), ylim =c(-3,3),col="white", )
abline(h=0, col="gray")
points(analysismolluscs$TieringvBDSOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$TieringvBDSOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$TieringvBDSOdds+1.96*analysismolluscs$TieringvBDSSTD,
       analysismolluscs$Mid_Age, analysismolluscs$TieringvBDSOdds-1.96*analysismolluscs$TieringvBDSSTD, 
       angle=90, length=0.001, lwd=1.5, col ="forestgreen") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$TieringvBDSOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$TieringvBDSOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient For Body Size", line=2.5)
mtext(side = 3, "Living in The Water Column" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Living on Sea Floor and Below", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))

plot(analysismolluscs$TieringvExtinctOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Mollucsca Tiering Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$TieringvExtinctOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$TieringvExtinctOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$TieringvExtinctOdds+1.96*analysismolluscs$TieringvExtinctSTD,
       analysismolluscs$Mid_Age, analysismolluscs$TieringvExtinctOdds-1.96*analysismolluscs$TieringvExtinctSTD, 
       angle=90, length=0.001, lwd=1.5, col ="forestgreen") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$TieringvExtinctOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$TieringvExtinctOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient for Extinction Risk", line=2.5)
mtext(side = 3, "Living in the Water Column" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Living on Sea Floor and Below", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))
dev.off()


#-----------------------------------------------------------
dev.new()
jpeg(file = "Circulation.jpeg", width = 960, height =480)   # The directory you want to save the file in
par(mfrow=c(1,2))
plot(analysismolluscs$CircvSize~analysismolluscs$Mid_Age, pch=16, main="Body Size as a Function of Mollucsca Circulation Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$CircvSize[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$CircvSize[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$CircvSize+1.96*analysismolluscs$CircvSizeSTD,
       analysismolluscs$Mid_Age, analysismolluscs$CircvSize-1.96*analysismolluscs$CircvSizeSTD, 
       angle=90, length=0.001, lwd=1.5, col ="red") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$CircvSize[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$CircvSize[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient For Body Size", line=2.5)
mtext(side = 3, "Closed Circulation" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Open Circulation", padj = -2 , cex = 0.75, font =2)

legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))



plot(analysismolluscs$CircvExtinctOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Mollucsca Circulation Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$CircvExtinctOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$CircvExtinctOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$CircvExtinctOdds+1.96*analysismolluscs$CircvExtinctSTD,
       analysismolluscs$Mid_Age, analysismolluscs$CircvExtinctOdds-1.96*analysismolluscs$CircvExtinctSTD, 
       angle=90, length=0.001, lwd=1.5, col ="red") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$CircvExtinctOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$CircvExtinctOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient for Extinction Risk", line=2.5)
mtext(side = 3, "Closed Circulation" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Open Circulation", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))
dev.off()
#________________________________________________

dev.new()
jpeg(file = "Feeding.jpeg", width = 960, height =480)   # The directory you want to save the file in
par(mfrow=c(1,2))
plot(analysismolluscs$FeedingvBDSOdds~analysismolluscs$Mid_Age, pch=16, main="Body Size as a Function of Mollucsca Feeding Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$FeedingvBDSOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$FeedingvBDSOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$FeedingvBDSOdds+1.96*analysismolluscs$FeedingvBDSSTD,
       analysismolluscs$Mid_Age, analysismolluscs$FeedingvBDSOdds-1.96*analysismolluscs$FeedingvBDSSTD, 
       angle=90, length=0.001, lwd=1.5, col ="purple") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$FeedingvBDSOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$FeedingvBDSOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient For Body Size", line=2.5)
mtext(side = 3, "Predator" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Prey", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))



plot(analysismolluscs$FeedingvExtinctOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Mollucsca Feeding Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$FeedingvExtinctOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$FeedingvExtinctOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$FeedingvExtinctOdds+1.96*analysismolluscs$FeedingvExtinctSTD,
       analysismolluscs$Mid_Age, analysismolluscs$FeedingvExtinctOdds-1.96*analysismolluscs$FeedingvExtinctSTD, 
       angle=90, length=0.001, lwd=1.5, col ="purple") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$FeedingvExtinctOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$FeedingvExtinctOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient for Extinction Risk", line=2.5)
mtext(side = 3, "Predator" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Prey", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))
dev.off

#__________________________________________
dev.new()
jpeg(file = "Motility.jpeg", width = 960, height =480)   # The directory you want to save the file in
#    width = 12, # The width of the plot in inches
 #   height = 6) # The height of the plot in inches
par(mfrow=c(1,2))
plot(analysismolluscs$MotilityvBDSOdds~analysismolluscs$Mid_Age, pch=16, main="Body Size as a Function of Mollucsca Motility Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$MotilityvBDSOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$MotilityvBDSOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$MotilityvBDSOdds+1.96*analysismolluscs$MotilityvBDSSTD,
       analysismolluscs$Mid_Age, analysismolluscs$MotilityvBDSOdds-1.96*analysismolluscs$MotilityvBDSSTD, 
       angle=90, length=0.001, lwd=1.5, col ="blue") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$MotilityvBDSOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$MotilityvBDSOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient For Body Size", line=2.5)
mtext(side = 3, "Free-Moving" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Sedentary", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))



plot(analysismolluscs$MotilityvExtinctOdds~analysismolluscs$Mid_Age, pch=16, main="Extinction Risk as a Function of Mollucsca Motility Data",
     xaxt="n", xlab="Geologic Time across the Ordovician", ylab="", las=1, ylim =c(-3,3),col="white", xlim=c(485.4,443.4))
abline(h=0, col="gray")
points(analysismolluscs$MotilityvExtinctOdds[4:7]~analysismolluscs$Mid_Age[4:7], pch=2,
       xaxt="n", xlab="", ylab="", las=1)
points(analysismolluscs$MotilityvExtinctOdds[1:3]~analysismolluscs$Mid_Age[1:3], pch=17,
       xaxt="n", xlab="", ylab="", las=1)
arrows(analysismolluscs$Mid_Age, analysismolluscs$MotilityvExtinctOdds+1.96*analysismolluscs$MotilityvExtinctSTD,
       analysismolluscs$Mid_Age, analysismolluscs$MotilityvExtinctOdds-1.96*analysismolluscs$MotilityvExtinctSTD, 
       angle=90, length=0.001, lwd=1.5, col ="blue") 

for (i in 1:(length(timescale)-1)){
  z <-i+1
  arrows(analysismolluscs$Mid_Age[i], analysismolluscs$MotilityvExtinctOdds[i],
         analysismolluscs$Mid_Age[z], analysismolluscs$MotilityvExtinctOdds[z], 
         angle=90, length=0.001, lwd=1.5, col ="gray30", lty="dashed")
}

axis(side=1, at=c(481.55,473.85,468.65,462.85,455.70, 449.10, 444.30), labels=labelss, las=1)
mtext(side=2, "Regression Coefficient for Extinction Risk", line=2.5)
mtext(side = 3, "Free-Moving" , padj = 2 , cex = 0.75, font =2)
mtext(side = 1, "Sedentary", padj = -2 , cex = 0.75, font =2)
legend(465,-2, legend=c("Molluscs during the LOME", "Molluscs prior to the LOME"),
       col=c("black", "black"), lty=1, cex=0.8, pch=c(17,2))
dev.off()



cor.test(analysismolluscs$Percent_Ex, timescale$SeaLevel)
cor.test(analysismolluscs$Percent_Ex[1:5], timescale$pO2[1:5])
cor.test(analysismolluscs$Percent_Ex[1:6], timescale$pO2[1:6])




cor.test(analysismolluscs$TieringvExtinctOdds, analysismolluscs$CircvExtinctOdds)
cor.test(analysismolluscs$TieringvExtinctOdds, analysismolluscs$FeedingvExtinctOdds)
cor.test(analysismolluscs$FeedingvExtinctOdds, analysismolluscs$CircvExtinctOdds)

cor.test(analysismolluscs$MotilityvExtinctOdds, analysismolluscs$CircvExtinctOdds)
cor.test(analysismolluscs$MotilityvExtinctOdds, analysismolluscs$TieringvExtinctOdds)
cor.test(analysismolluscs$MotilityvExtinctOdds, analysismolluscs$FeedingvExtinctOdds)
#_________________________________________________________________________________