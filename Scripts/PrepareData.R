library("plyr")
library("dplyr")

DepRate1 = 0.03
DepRate2 = 0.05

####################################################################################################
## Read Data
####################################################################################################
GrowthTotal = read.csv("Data/Derived/VAStructuralGrowth.csv", stringsAsFactors=F)
VACapital   = read.csv("Data/Derived/VACapital.csv", stringsAsFactors=F) 
VAgrowth    = read.csv("Data/Derived/VAGrowth.csv", stringsAsFactors=F)
StockData   = read.csv("Data/Derived/databasis.csv", stringsAsFactors=F)
Employment  = read.csv("Data/Derived/Employment.csv", stringsAsFactors=F)
DeterminantsOriginal = read.csv("Data/Derived/DeterminantsOrigineel.csv", stringsAsFactors=F)
DeterminantsOriginal <- mutate(DeterminantsOriginal,
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
Determinants         = read.csv("Data/Derived/Determinants.csv", stringsAsFactors=T)
####################################################################################################
## Filter and select data
####################################################################################################
StockData       = select(StockData, Year, gamscode, TotalYV2, AgrYV2, EMYV2, ConYV2, DistYV2, ServYV2, NMServYV2)
Determinants    = select(Determinants, Region, Country)
Interest        = select(Employment, gamscode, I)
Interest        = group_by(Interest, gamscode)
Interest        = summarize(Interest, I = mean(I))
Y2000           = filter(StockData, Year==2000)
Y2000$Region    = Y2000$gamscode
E2000           = filter(Employment, Year==2000)
E2000$Region    = E2000$gamscode
E2010           = filter(Employment, Year==2007)
E2010$Region    = E2010$gamscode
VAgrowth        = filter(VAgrowth, Year<=2007)
K2000           = filter(VACapital, Year == 2000)
K2010           = filter(VACapital, Year == 2007)
K2000           <- mutate(K2000, 
                        KAgr = SAgrCap/(Interest$I/100), 
                        KEM  = SEenMCap/(Interest$I/100),
                        KCon = SConsCap/(Interest$I/100),
                        KDis = SDisCap/(Interest$I/100),
                        KServ = SServCap/(Interest$I/100),
                        KNMServ = SNMServCap/(Interest$I/100),
                        KTot = KAgr + KEM + KCon + KDis + KServ + KNMServ)
K2010           <- mutate(K2010, 
                          KAgr = SAgrCap/(Interest$I/100), 
                          KEM  = SEenMCap/(Interest$I/100),
                          KCon = SConsCap/(Interest$I/100),
                          KDis = SDisCap/(Interest$I/100),
                          KServ = SServCap/(Interest$I/100),
                          KNMServ = SNMServCap/(Interest$I/100),
                          KTot = KAgr + KEM + KCon + KDis + KServ + KNMServ)

# Data <- mutate(Data, STot = sum(SAgrStruc+SDisStruc+SEenMStruc+SConsStruc+SServStruc+SNMServStruc))
Data <- mutate(GrowthTotal, STot = SAgr+SDis+SEenM+SCons+SServ+SNMServ)
Data <- merge(Data, Y2000, by = "Region")
Data <- merge(Data, E2000, by ="Region")
Data <- merge(Data, E2010, by ="Region")
Data <- merge(Data, Determinants, by ="Region")
Data <- merge(Data, DeterminantsOriginal, by = "Region")
Data$AgrE.y[is.na(Data$AgrE.y)] <- 0.0001
Data <- mutate(Data, 
               TotE.y = AgrE.y + EME.y + ConE.y + DistE.y + ServE.y + NMServE.y, 
               TotE.x = AgrE.x + EME.x + ConE.x + DistE.x + ServE.x + NMServE.x 
        )
####################################################################################################
## Calculate the endogeneous variable (growth per worker)
####################################################################################################
Data <- arrange(Data, Region)
Data <- ddply(Data, "Region", mutate,
                STotDep    = ((TotalYV2+STot)/TotE.y)/(TotalYV2/TotE.x),
                SAgrDep    = ((AgrYV2+SAgr)/AgrE.y)/(AgrYV2/AgrE.x),
                SEenMDep   = ((EMYV2+SEenM)/EME.y)/(EMYV2/EME.x),
                SConsDep   = ((ConYV2+SCons)/ConE.y)/(ConYV2/ConE.x),
                SDisDep    = ((DistYV2+SDis)/DistE.y)/(DistYV2/DistE.x),
                SServDep   = ((ServYV2+SServ)/ServE.y)/(ServYV2/ServE.x), 
                SNMServDep = ((NMServYV2+SNMServ)/NMServE.y)/(NMServYV2/NMServE.x)
                )
#               STotDep    = ((TotalYV2+STot))/(TotalYV2),
#               SAgrDep    = ((AgrYV2+SAgr))/(AgrYV2),
#               SEenMDep   = ((EMYV2+SEenM))/(EMYV2),
#               SConsDep   = ((ConYV2+SCons))/(ConYV2),
#               SDisDep    = ((DistYV2+SDis))/(DistYV2),
#               SServDep   = ((ServYV2+SServ))/(ServYV2), 
#               SNMServDep = ((NMServYV2+SNMServ))/(NMServYV2)
#         )
# STotDep    = (STot),
# SAgrDep    = (SAgr),
# SEenMDep   = (SEenM),
# SConsDep   = (SCons),
# SDisDep    = (SDis),
# SServDep   = (SServ), 
# SNMServDep = (SNMServ)
# )
####################################################################################################
## Calculate the lagged variable (value added at time = 0)
####################################################################################################
Data <- ddply(Data, "Region", mutate,
              STot0    = TotalYV2/TotE.x, 
              SAgr0    = AgrYV2/AgrE.x, 
              SEenM0   = EMYV2/EME.x,
              SCons0   = ConYV2/ConE.x,               
              SDis0    = DistYV2/DistE.x, 
              SServ0   = ServYV2/ServE.x, 
              SNMServ0 = NMServYV2/NMServE.x
              )
# Data <- ddply(Data, "Region", mutate,
#               STot0    = TotalYV2, 
#               SAgr0    = AgrYV2, 
#               SEenM0   = EMYV2,
#               SCons0   = ConYV2,               
#               SDis0    = DistYV2, 
#               SServ0   = ServYV2, 
#               SNMServ0 = NMServYV2
#               )
####################################################################################################
## Calculate the savings rate variable (s = (Delta K + delta K) / Y )
####################################################################################################
Data <- mutate(Data, 
               sTot    = ((K2010$KTot    - K2000$KTot)   /11 + DepRate1*K2000$KTot) /TotalYV2,
               sAgr    = ((K2010$KAgr    - K2000$KAgr)   /11 + DepRate1*K2000$KAgr) /AgrYV2,
               sEM     = ((K2010$KEM     - K2000$KEM)    /11 + DepRate1*K2000$KEM)  /EMYV2,
               sCon    = ((K2010$KCon    - K2000$KCon)   /11 + DepRate1*K2000$KCon) /ConYV2,
               sDist   = ((K2010$KDis    - K2000$KDis)  /11 + DepRate1*K2000$KDis)/DistYV2,
               sServ   = ((K2010$KServ   - K2000$KServ)  /11 + DepRate1*K2000$KServ)/ServYV2,
               sNMServ = ((K2010$KNMServ - K2000$KNMServ)/11 + DepRate1*K2000$KNMServ)/NMServYV2
               )
####################################################################################################
## Calculate the worker growth rate variable (n + g + delta)
####################################################################################################
Data <- mutate(Data, 
               nTot    = (log(TotE.y*1000) - log(TotE.x*1000))/11 + DepRate2,
               nAgr    = (log(AgrE.y*1000) - log(AgrE.x*1000))/11 + DepRate2,
               nEM     = (log(EME.y*1000)  - log(EME.x*1000))/11 + DepRate2,
               nCon    = (log(ConE.y*1000) - log(ConE.x*1000))/11 + DepRate2,
               nDist   = (log(DistE.y*1000) - log(DistE.x*1000))/11 + DepRate2,
               nServ   = (log(ServE.y*1000) - log(ServE.x*1000))/11 + DepRate2,
               nNMServ = (log(NMServE.y*1000) - log(NMServE.x*1000))/11 + DepRate2
               )
####################################################################################################
## Generate country dummies
####################################################################################################
Data <- ddply(Data, "Region", mutate,
              Austria = as.numeric(Country == "Austria"),
              Belgium = as.numeric(Country == "Belgium"),
              Czech = as.numeric(Country == "Czech Republic"),
              Denmark = as.numeric(Country == "Denmark" ),
              Estland = as.numeric(Country == "Estonia"),
              Finland = as.numeric(Country == "Finland"),
              France = as.numeric(Country == "France"),
              Germany = as.numeric(Country == "Germany"),
              Greece = as.numeric(Country == "Greece"),
              Hungary = as.numeric(Country == "Hungary"),
              Ireland = as.numeric(Country == "Ireland"),
              Italy = as.numeric(Country == "Italy"),
              Latvia = as.numeric(Country == "Letland"),
              Lithuania = as.numeric(Country == "Lithouania"),
              Luxembourg = as.numeric(Country == "Luxembour"),
              Malta = as.numeric(Country == "Malta"),
              Netherlands = as.numeric(Country == "Netherlands"),
              Norway = as.numeric(Country == "Norway"),
              Poland = as.numeric(Country == "Poland"),
              Portugal = as.numeric(Country == "Portugal"),
              Slovenia= as.numeric(Country == "Slovenia"),
              Slovakia = as.numeric(Country == "Slowakia"),
              Spain = as.numeric(Country == "Spain"),
              Sweden = as.numeric(Country == "Sweden"),              
              UK = as.numeric(Country == "UK")
                )
####################################################################################################
## Keep final data and write to file
####################################################################################################

write.csv(Data, "Data/Derived/DataForEstimations.csv", row.names=TRUE)

rm(list = ls())
