library("plyr")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("ggmap")
library("rgdal")
library("rgeos")
library("maptools")
library("tidyr")
library("mapproj") 
library("RColorBrewer")

get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)
myPal1 = colorRampPalette(brewer.pal(9,"Greens"))(100)
####################################################################################################
## First, read and prepare Data
####################################################################################################

VAgrowth = read.csv("Data/Derived/VAGrowth.csv", stringsAsFactors=F)
determinants = read.csv("Data/Derived/DeterminantsOrigineel.csv", stringsAsFactors=F)
determinants <- mutate(determinants,
                               bevtot = (bevtot - mean(bevtot))/sd(bevtot),
                               popdens = (popdens - mean(popdens))/sd(popdens),
                               hoogopl = (hoogopl - mean(hoogopl))/sd(hoogopl),
                               RenDpub = (RenDpub - mean(RenDpub))/sd(RenDpub),
                               Patent = (Patent - mean(Patent))/sd(Patent),
                               RenDbus = (RenDbus - mean(RenDbus))/sd(RenDbus),
                               Bbweg = (Bbweg - mean(Bbweg))/sd(Bbweg),
                               Bblucht = (Bblucht - mean(Bblucht))/sd(Bblucht),
                               Cong = (Cong - mean(Cong))/sd(Cong)
)
VAgrowth <- merge(VAgrowth, determinants, by = "Region")
VAgrowth <- filter(VAgrowth, Year<=2007)
Regions <- readOGR(dsn = "Data/Derived", layer="RegionsRiE2010")
Regions <- spTransform(Regions, CRS("+init=epsg:4326")) 
DataPic <- ddply(VAgrowth, "Region", summarise,
              SAgr = sum(SAgrStruc), 
              SEenM = sum(SEenMStruc),
              SCons = sum(SConsStruc),
              SDis = sum(SDisStruc),
              SServ = sum(SServStruc),
              SNMServ = sum(SNMServStruc),
              STot = sum(SAgrStruc+SDisStruc+SEenMStruc+SConsStruc+SDisStruc+SServStruc+SNMServStruc),
              DAgr = sum(SAgrDem), 
              DEenM = sum(SEenMDem),
              DCons = sum(SConsDem),
              DDis = sum(SDisDem),
              DServ = sum(SServDem),
              DMServ = sum(SNMServDem),
              DTot = sum(SAgrDem+SDisDem+SEenMDem+SConsDem+SDisDem+SServDem+SNMServDem),
              TAgr = sum(SAgrTot), 
              TEenM = sum(SEenMTot),
              TCons = sum(SConsTot),
              TDis = sum(SDisTot),
              TServ = sum(SServTot),
              TNMServ = sum(SNMServTot),
              TTot = sum(SAgrTot+SDisTot+SEenMTot+SConsTot+SDisTot+SServTot+SNMServTot),
              PopulationDensity = mean(popdens),
              Popdens = mean(popdens),
              RenDbus = mean(RenDbus),
              Bbweg = mean(Bbweg),
              Bblucht = mean(Bblucht),
              Cong = mean(Cong), 
              hoogopl = mean(hoogopl)
              )

DataPic <- rename(DataPic, gamscode=Region)
Regions <- merge(Regions, DataPic, by="gamscode")
Regions_f <- fortify(Regions, region = "gamscode")
Regions_f <- left_join(Regions_f, Regions@data, by = c("id" = "gamscode"))
####################################################################################################
## Make scatterplot of structural and demand led growth
####################################################################################################

p1 <- ggplot(DataPic, aes(DTot, TTot)) +  geom_point(aes(size = PopulationDensity)) +  geom_smooth() + ylim(0, 110000)+ xlim(-30000,110000) + 
    scale_size_area() + ylab("Total value added") + xlab("Demand-led value added growth")
p2 <- ggplot(DataPic, aes(STot, TTot)) +  geom_point(aes(size = PopulationDensity)) +  geom_smooth() +   ylim(0, 110000)+xlim(-30000,110000) + 
    scale_size_area() + ylab("Total value added") + xlab("Structural value added growth") + theme(legend.position="none")

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")

g <- grid.arrange(p1, p2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))
g <- arrangeGrob(p1, p2, legend, ncol=3, widths=c(2.3, 2.3, 0.8))

ggsave(file='Output/Fig/Decomposition.pdf', g, , width = 9, height = 4.5) #saves g

####################################################################################################
## Make maps from structural and demand led growth
####################################################################################################

map1 <- spplot(Regions, "TTot",col.regions=myPal1,cuts=8, at = seq(0, 140000, by = 2000), main="Total growth 2000-2007")
map2 <- spplot(Regions, "STot",col.regions=myPal,cuts=8, at = seq(-30000, 30000, by = 1000), main="Structural growth 2000-2007")

map <- grid.arrange(map1, map2, ncol = 2)

pdf('Output/Fig/VAGrowth.pdf')
map
dev.off()

pdf('Output/Fig/TTot.pdf')
map1
dev.off()

pdf('Output/Fig/STot.pdf')
map2
dev.off()

####################################################################################################
## Make maps from determinants
####################################################################################################
mapPopDens <- spplot(Regions, "Popdens",col.regions=myPal,cuts=8, at = seq(-2, 2, by =0.1), main="Population density")
mapCong <- spplot(Regions, "Cong",col.regions=myPal,cuts=8, at = seq(-2, 2, by =0.1), main="Congestion")
mapConnectivityRoad <- spplot(Regions, "Bbweg",col.regions=myPal,cuts=8, at = seq(-2, 2, by =0.1), main="Connectivity by Road")
mapHoogopl <- spplot(Regions, "hoogopl",col.regions=myPal,cuts=8, at = seq(-2, 2, by =0.1), main="Proportion higher educated")
mapConnectivityAir <- spplot(Regions, "Bblucht",col.regions=myPal,cuts=8, at = seq(-2, 2, by =0.1), main="Connectivity by air")

pdf('Output/Fig/PopDens.pdf')
mapPopDens
dev.off()

pdf('Output/Fig/Congestion.pdf')
mapCong
dev.off()

pdf('Output/Fig/ConnectivityAir.pdf')
mapConnectivityAir
dev.off()

pdf('Output/Fig/ConnectivityRoad.pdf')
mapConnectivityRoad
dev.off()

pdf('Output/Fig/Hoogopl.pdf')
mapHoogopl
dev.off()
