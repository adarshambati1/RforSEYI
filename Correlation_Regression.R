#1. Correlation can be positive or negative. Positive correlation between two variables indicates that both increase in parallel. Negative correlation indicates that as one variable increases, the other decreases in a similar fashion. As correlation becomes closer to one or negative one, strenght of the relation ship increases
x <- runif(1000)
y <- rnorm(1000)
z <- x + y

dev.new(height=6, width=12) # this opens a 6” x 12” plot window
par(mfrow=c(1,2), pch=16) # this says make two plots and use solid points
hist(x, main="Uniform Distribution")
hist(y, main="Normal Distribution")
#4. As X is a uniform Distribution, adding it to y should not affect the normal distribution. Thus, z should be approx. normal as well.
dev.new()
hist(z, main="X+Y")

dev.new()
plot(y,x, pch=16)
#5. They do not seem correlated. There are no upward or downward trends. The least squares regression would probably have a close to zero slope, indicating no correlation This makes sense as both are randomly generated.
dev.new()
plot(z,y,pch =16)
#6. They seem very closely correlated. There is a clear upward trend, indicating a positive slope, and a positive correlation.

cor.test(x,y)
#8. The estimated correlation coefficient is 0.01353631

cor.test(y,z)
#9. The estimated correlation coefficient is 0.9617379. The correlation estimate for x and y is extremely close to zero indicating little to none correlation, whereas the correlation estimate for y and z are extremely close to positive one, indicating positive correlation. We can 95% confident that the true correlation between x and y is between -0.04849747 and 0.07546607. We can also be 95% confident that the true correlation lies between  0.9567897 and 0.9661293
#9. The total range of values are -1 to 1. -1 indicates a complete negative correlation. 1 indicates a complete positive correlation. and 0 indicates no correlation



lepidoptera <- read.delim(file='lepidoptera.txt')
lepidoptera$logfinalBody<-log(lepidoptera$finalBody)
lepidoptera$logfinalWing<-log(lepidoptera$finalWing)
dev.new()
plot(lepidoptera$logfinalBody, lepidoptera$logfinalWing,pch=16)

dev.new()
par(mfrow=c(2,2), pch=16) # this says make two plots and use solid point
hist(lepidoptera$finalBody, breaks=10)
hist(lepidoptera$finalWing)
hist(lepidoptera$logfinalBody)
hist(lepidoptera$logfinalWing)
#The log transformed variables are normally distributed


cor.test(lepidoptera$logfinalBody, lepidoptera$logfinalWing)
#12. The correlation coefficient for the log transformed variables is estimated to be 0.8904519

#______________________________________________
#Regression
myRegression <- glm(log10(lepidoptera$finalBody) ~ log10(lepidoptera$finalWing))
#y = 0.8703704x -0.04559341

0.8703704*(13.8)-0.04559341
#15. The expected width of a butterfly with a wing length of 13.8 mm is 11.96552
dev.new()
plot(lepidoptera$logfinalBody, lepidoptera$logfinalWing,pch=16)
abline(myRegression, lty=2, col="red")
#______________________________________________
#logistic Regression
IUCN <- read.delim("SOM_data.txt")
livingchordates <- subset.data.frame(IUCN, IUCN$phylum=="Chordata" & IUCN$extant=="t")
dev.new()
livingchordates$mxlengthlog <-log10(livingchordates$max_length)
boxplot(log10(livingchordates$max_length)~livingchordates$maxthreat)

extlogreg <- glm(maxthreat~mxlengthlog, family=binomial(),data=livingchordates)

summary(extlogreg)

extlogreg <- glm(minthreat~mxlengthlog, family=binomial(),data=livingchordates)

summary(extlogreg)




