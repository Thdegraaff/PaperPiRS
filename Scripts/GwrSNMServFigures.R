####################################################################################################
## Author:      Thomas de Graaff
## What:        Make figures of GWR of Non-market Services Sector
## Edited:      20-04-2015
####################################################################################################
library("spdep")
library("maptools")
library("GWmodel")
library("plyr")
library("RColorBrewer")
library("ggplot2")
####################################################################################################
## Read Data
####################################################################################################
determinants = read.csv("Data/Derived/Determinants.csv", stringsAsFactors=F)
Data = read.csv("Data/Derived/DataForEstimations.csv", stringsAsFactors=F)
copatents <- read.csv("Data/Derived/copatents1978_2011.csv", stringsAsFactors=F, na.strings = "0", header =TRUE, row.names=1)
fdistock2010 <- read.csv("Data/Derived/fdistock2010.csv", stringsAsFactors=F, na.strings = "0", header =TRUE, row.names=1)
RCMat = read.csv("Data/Derived/SNMServRCMat.csv", header = FALSE)
Regions<-readShapePoly("Data/Derived/RegionsRiE2010.shp")
####################################################################################################
## Transform Data
####################################################################################################
Data <- data.frame(Data,"gamscode"=Data$Region)
Regions <- merge(Regions, Data, by="gamscode")
Regions <- Regions[order(Regions$id.x),]
    
Regions$SNMServDep[Regions$SNMServDep <= 0] <- 0.01

Regions$sNMServ <- log(Regions$sNMServ)
Regions$sNMServ[is.na(Regions$sNMServ)] <- 0

Regions$nNMServ <- log(Regions$nNMServ)
Regions$nNMServ[is.na(Regions$nNMServ)] <- 0

formula <- log(SNMServDep) ~ log(SNMServ0) + sNMServ + nNMServ + bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong
varlist <- c("bevtot", "popdens", "hoogopl", "RenDpub", "Patent", "RenDbus", "Bbweg", "Bblucht", "Cong")

copatents[is.na(copatents)] <- 0.01
copatents <- 1/copatents
fdistock2010[is.na(fdistock2010)] <- 0.01
fdistock2010 <- 1/fdistock2010

RCMat[is.na(RCMat)] <- 0
RCMat <- 1/RCMat

myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)
####################################################################################################
## Make Figures with Revealed Competition matrix
####################################################################################################
bw.a <- bw.gwr(formula, data=Regions, approach="CV",kernel="gaussian",dMat = RCMat,adaptive =TRUE)
gwr.res <- gwr.basic(formula, data=Regions, bw=bw.a,kernel="gaussian",dMat=RCMat, adaptive =TRUE)
gwr.res

for (i in varlist) {
    temp   <- paste("gwr.res$SDF$",i, sep="")
    temp   <- eval(parse(text = temp))
    tempTV <- paste("gwr.res$SDF$",i,"_TV", sep="")
    tempTV <- eval(parse(text = tempTV))    
    gwr.res$SDF$value <- (abs(tempTV) >= 1.96) * temp
    # tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, main=paste("GWR using RC coefficient",i,"on Non-market Services",sep=" "))
    tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, at = seq(-0.15, 0.15, by = 0.005))
    pdf(paste("Output/Fig/RCNMServ",i,".pdf", sep=""), height = 6)
    print(tempplot)
    dev.off()
    DataTemp <- data.frame(Regions$gamscode,gwr.res$SDF$value)
    colnames(DataTemp) <- c("Gamscode","Value GWR") 
    write.table(DataTemp, file = paste("Data/DataRoxana/RCNMServ",i,".csv", sep=""), row.names = FALSE ,sep = ",")    
}
####################################################################################################
## Make Figures with copatents
####################################################################################################
# bw.a <- bw.gwr(formula, data=Regions, approach="AICc",kernel="bisquare",dMat = copatents,adaptive =TRUE)
# gwr.res <- gwr.basic(formula, data=Regions, bw=bw.a,kernel="bisquare",dMat=copatents, adaptive =TRUE)
# gwr.res
# 
# for (i in varlist) {
#     temp   <- paste("gwr.res$SDF$",i, sep="")
#     temp   <- eval(parse(text = temp))
#     tempTV <- paste("gwr.res$SDF$",i,"_TV", sep="")
#     tempTV <- eval(parse(text = tempTV))    
#     gwr.res$SDF$value <- (abs(tempTV) >= 1.96) * temp
#     tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, main=paste("GWR using copatents coefficient",i,"on Non-market Services",sep=" "))
#     jpeg(paste("Output/Fig/CoPatentNMServ",i,".jpg", sep=""))
#     print(tempplot)
#     dev.off()
# }
# 
# ####################################################################################################
# ## Make Figures with FDI
# ####################################################################################################
# bw.a <- bw.gwr(formula, data=Regions, approach="AICc",kernel="bisquare",dMat = fdistock2010,adaptive =TRUE)
# gwr.res <- gwr.basic(formula, data=Regions, bw=bw.a,kernel="bisquare",dMat=fdistock2010, adaptive =TRUE)
# gwr.res
# 
# for (i in varlist) {
#     temp   <- paste("gwr.res$SDF$",i, sep="")
#     temp   <- eval(parse(text = temp))
#     tempTV <- paste("gwr.res$SDF$",i,"_TV", sep="")
#     tempTV <- eval(parse(text = tempTV))    
#     gwr.res$SDF$value <- (abs(tempTV) >= 1.96) * temp
#     tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, main=paste("GWR using FDI coefficient",i,"on Non-market Services",sep=" "))
#     jpeg(paste("Output/Fig/FDINMServ",i,".jpg", sep=""))
#     print(tempplot)
#     dev.off()
# }

####################################################################################################
## Make Figures with distance
####################################################################################################
bw.a <- bw.gwr(formula, data=Regions, approach="CV",kernel="gaussian",adaptive =TRUE)
gwr.res <- gwr.basic(formula, data=Regions, bw=bw.a,kernel="gaussian", adaptive =TRUE)
gwr.res

for (i in varlist) {
    temp   <- paste("gwr.res$SDF$",i, sep="")
    temp   <- eval(parse(text = temp))
    tempTV <- paste("gwr.res$SDF$",i,"_TV", sep="")
    tempTV <- eval(parse(text = tempTV))    
    gwr.res$SDF$value <- (abs(tempTV) >= 1.96) * temp
    # tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, main=paste("GWR using Distance coefficient",i,"on Non-market Services",sep=" "))
    tempplot <- spplot(gwr.res$SDF,"value",col.regions=myPal,cuts=8, at = seq(-0.15, 0.15, by = 0.005))
    pdf(paste("Output/Fig/DistanceNMServ",i,".pdf", sep=""), height = 6)
    print(tempplot)
    dev.off()
    DataTemp <- data.frame(Regions$gamscode,gwr.res$SDF$value)
    colnames(DataTemp) <- c("Gamscode","Value GWR") 
    write.table(DataTemp, file = paste("Data/DataRoxana/DistanceNMServ",i,".csv", sep=""), row.names = FALSE ,sep = ",")    
}

####################################################################################################
## End of script
####################################################################################################
rm(list = ls())