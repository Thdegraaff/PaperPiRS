####################################################################################################
## Author:      Thomas de Graaff
## What:        Make figures of GWR of Total Sector
## Edited:      10-12-2014
####################################################################################################
library("spdep")
library("maptools")
library("GWmodel")
library("plyr")
library("RColorBrewer")
####################################################################################################
## Read Data
####################################################################################################
determinants = read.csv("Data/Derived/Determinants.csv", stringsAsFactors=F)
Data = read.csv("Data/Derived/DataForEstimations.csv", stringsAsFactors=F)
copatents <- read.csv("Data/Derived/copatents1978_2011.csv", stringsAsFactors=F, na.strings = "0", header =TRUE, row.names=1)
fdistock2010 <- read.csv("Data/Derived/fdistock2010.csv", stringsAsFactors=F, na.strings = "0", header =TRUE, row.names=1)
Regions<-readShapePoly("Data/Derived/RegionsRiE2010.shp")
####################################################################################################
## Transform Data
####################################################################################################
Data <- data.frame(Data,"gamscode"=Data$Region)
Regions <- merge(Regions, Data, by="gamscode")
Regions <- Regions[order(Regions$id.x),]

Regions$STotDep[is.na(Regions$STotDep)] <- 0.01

Regions$sTot <- log(Regions$sTot)
Regions$sTot[is.na(Regions$sTot)] <- 0

Regions$nTot <- log(Regions$nTot)
Regions$nTot[is.na(Regions$nTot)] <- 0

Regions$popdens <- log(Regions$popdens)
Regions$RenDpriv <- log(Regions$RenDpriv)
Regions$Connectivity <- log(Regions$Connectivity)
Regions$Cong <- log(Regions$Cong)

formula <- log(STotDep) ~ log(STot0) + sTot + nTot + bevtot + popdens + RenDpriv + RenDpub+ Connectivity + Cong

copatents[is.na(copatents)] <- 0.01
copatents <- 1/copatents
fdistock2010[is.na(fdistock2010)] <- 0.01
fdistock2010 <- 1/fdistock2010
####################################################################################################
## Make Figures with copatents
####################################################################################################
bw.a <- bw.gwr(formula, data=Regions, approach="AICc",kernel="gaussian",dMat = copatents,adaptive =TRUE)
gwr.res <- gwr.basic(formula, data=Regions,bw=bw.a,kernel="gaussian",dMat=copatents, adaptive =TRUE)
a <- gwr.res$lm

pdf("Output/Fig/PatentsSTotpopdens.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$popdens_TV) >= 1.96) * gwr.res$SDF$popdens
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using patents coefficient popdens on Total")
dev.off()

pdf("Output/Fig/PatentsSTotRenDpriv.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$RenDpriv_TV) >= 1.96) * gwr.res$SDF$RenDpriv
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using patents coefficient RenDprivon Total")
dev.off()

pdf("Output/Fig/PatentsSTotConnectivity.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Connectivity_TV) >= 1.96) * gwr.res$SDF$Connectivity
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using patents coefficient Connectivity on Total")
dev.off()

pdf("Output/Fig/PatentsTotCong.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Cong_TV) >= 1.96) * gwr.res$SDF$Cong
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using patents coefficient Cong on Total")
dev.off()

####################################################################################################
## Make Figures with FDI
####################################################################################################
bw.a <- bw.gwr(formula, data=Regions, approach="AICc",kernel="gaussian",dMat = fdistock2010,adaptive =TRUE)
gwr.res <- gwr.basic(formula, data=Regions,bw=bw.a,kernel="gaussian",dMat=fdistock2010, adaptive =TRUE)
b <- gwr.res$lm

pdf("Output/Fig/FDISTotpopdens.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$popdens_TV) >= 1.96) * gwr.res$SDF$popdens
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using FDI coefficient popdens on Total")
dev.off()

pdf("Output/Fig/FDISTotRenDpriv.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$RenDpriv_TV) >= 1.96) * gwr.res$SDF$RenDpriv
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using FDI coefficient RenDprivon Total")
dev.off()

pdf("Output/Fig/FDISTotConnectivity.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Connectivity_TV) >= 1.96) * gwr.res$SDF$Connectivity
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using FDI coefficient Connectivity on Total")
dev.off()

pdf("Output/Fig/FDISTotCong.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Cong_TV) >= 1.96) * gwr.res$SDF$Cong
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using FDI coefficient Cong on Total")
dev.off()

####################################################################################################
## Make Figures with distance
####################################################################################################
bw.a <- bw.gwr(formula, data=Regions, approach="AICc",kernel="gaussian", adaptive =TRUE)
gwr.res <- gwr.basic(formula, data=Regions,bw=bw.a,kernel="gaussian", adaptive =TRUE)

pdf("Output/Fig/DistanceSTotbevtot.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$bevtot_TV) >= 1.96) * gwr.res$SDF$bevtot
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient bevtot on Total")
dev.off()

pdf("Output/Fig/DistanceSTotpopdens.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$popdens_TV) >= 1.96) * gwr.res$SDF$popdens
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient popdens on Total")
dev.off()

pdf("Output/Fig/DistanceSTotRenDpriv.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$RenDpriv_TV) >= 1.96) * gwr.res$SDF$RenDpriv
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient RenDpriv on Total")
dev.off()

pdf("Output/Fig/DistanceSTotRenDpub.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$RenDpub_TV) >= 1.96) * gwr.res$SDF$RenDpub
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient RenDpub on Total")
dev.off()

pdf("Output/Fig/DistanceSTotConnectivity.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Connectivity_TV) >= 1.96) * gwr.res$SDF$Connectivity
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient Connectivity on Total")
dev.off()

pdf("Output/Fig/DistanceSTotCong.pdf")
gwr.res$SDF$value <- (abs(gwr.res$SDF$Cong_TV) >= 1.96) * gwr.res$SDF$Cong
spplot(gwr.res$SDF,"value",col.regions=brewer.pal(9,"PuOr"),cuts=8,main="GWR using Distance coefficient Cong on Total")
dev.off()

####################################################################################################
## End of script
####################################################################################################