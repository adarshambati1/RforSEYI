brine <- read.table("brine.txt", sep=",", header = TRUE)
brine2<-read.table("brine2.csv", sep=",", header = TRUE, skip=1)
cadmium<-read.table("cadmium.txt")
cadmium2<-read.table("cadmium2.txt", header = TRUE)
cadmium3<-scan("cadmium3.txt")
carbonates<-read.table("carbonates.txt", sep=",",header = TRUE)
c2 <-read.table("c2.txt", sep=",", header = TRUE)
gasoline <-read.table("gasoline.txt", row.names=1, sep=",", header = TRUE)
nashvillecarbonates<-read.table("NashvilleCarbonates.csv", sep=",", header = TRUE, row.names=1)
ozone <-read.table("ozone.txt", header=TRUE)
trilobites3 <-read.table("trilobites3.txt", header=TRUE)
waves<-read.table("waves.txt", sep=",",header=TRUE)

#-------------------------------------
worms <-read.table("worms.csv", sep=",", header =TRUE, row.names = 1)
subset.data.frame(worms,worms$Soil.pH==4.0 )
subset.data.frame(worms,worms$Vegetation=="Arable" )
worms["Ashurst",]
subset.data.frame(worms,worms$Slope>8.0 )
subset.data.frame(worms,worms$Damp!=TRUE )
subset.data.frame(worms,worms$Area<=2 )
subset.data.frame(worms,worms$Worm.density!=4 )
subset.data.frame(worms,worms$Vegetation=="Grassland" | worms$Vegetation=="Arable")
subset.data.frame(worms,worms$Area>=3 & worms$Slope>=2)
subset.data.frame(worms,worms$Vegetation=="Grassland" & worms$Damp==TRUE)

attach(worms)
subset.data.frame(worms, Soil.pH==4.0 )
subset.data.frame(worms,Vegetation=="Arable" )
worms["Ashurst",]
subset.data.frame(worms,Slope>8.0 )
subset.data.frame(worms,Damp!=TRUE )
subset.data.frame(worms,Area<=2 )
subset.data.frame(worms,Worm.density!=4 )
subset.data.frame(worms,Vegetation=="Grassland" | Vegetation=="Arable")
subset.data.frame(worms,Area>=3 & Slope>=2)
subset.data.frame(worms,Vegetation=="Grassland" & Damp==TRUE)

detach(worms)
worms[c(1:10),]
worms[c(1:5,9),]
worms[-1,]
worms[-c(10:15),]


worms$Damp
worms$Vegetation
worms$Area
worms[2:4]
worms[c(2,4)]
worms[-5]
rownames(worms)


subset.data.frame(worms[c(2,4)], worms$Area>4)
subset.data.frame(worms[c(1,6)], worms$Vegetation=="Grassland")
subset.data.frame(worms[c(4,6)], worms$Area>2 & worms$Damp!=TRUE)
subset.data.frame(worms[c(1,2)], worms$Vegetation!="Grassland")
subset.data.frame(worms[-c(3)], worms$Vegetation!="Meadow")
rm(worms)
