library("stargazer")
library("lfe")
library("flexmix")
library("spdep")
library("maptools")
library("ctv")
library("plyr")
library("dplyr")
library("RColorBrewer")

Data = read.csv("Data/Derived/DataForEstimations.csv")

attach(Data)

####################################################################################################
## First for Growth in absolute value
####################################################################################################

Lag <- log(STot0) 
s   <- log(sTot)
# s[is.na(s)] <- -10
n   <- log(nTot)

fe.Tot  <- felm(log(STotDep) ~ Lag + s + n + bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.Tot <- lm(  log(STotDep) ~ Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  , data=Data)
summary(ols.Tot)
summary(fe.Tot)

Lag <- log(SAgr0) 
s   <- log(sAgr)
# s[is.na(s)] <- -10
n   <- log(nAgr)
# n[is.na(n)] <- -10
fe.Agr    <- felm(log(SAgrDep) ~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  + G(Country) , data=Data)
ols.Agr    <- lm( log(SAgrDep) ~ Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  , data=Data)
Lag <- log(SEenM0) 
s   <- log(sEM)
# s[is.na(s)] <- -10
n   <- log(nEM)
# n[is.na(n)] <- -10
fe.EM     <- felm( log(SEenMDep) ~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  + G(Country) , data=Data)
ols.EM    <- lm( log(SEenMDep) ~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  , data=Data)
Lag <- log(SCons0) 
s   <- log(sCon) 
# s[is.na(s)] <- -10
n   <- log(nCon)
fe.Con    <- felm( log(SConsDep)~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  + G(Country) , data=Data)
ols.Con    <- lm( log(SConsDep) ~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SDis0) 
s   <- log(sDist) 
# s[is.na(s)] <- -10
n   <- log(nDist)
fe.Dis    <- felm( log(SDisDep) ~ Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  + G(Country) , data=Data)
ols.Dis    <- lm( log(SDisDep) ~  Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SServ0) 
s   <- log(sServ) 
# s[is.na(s)] <- -10
n   <- log(nServ)
fe.Serv   <- felm( log(SServDep)~ Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  + G(Country) , data=Data)
ols.Serv    <- lm( log(SServDep) ~ Lag + s + n + bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong  , data=Data)
Lag <- log(SNMServ0) 
s   <- log(sNMServ) 
# s[is.na(s)] <- -10
n   <- log(nNMServ)
fe.NMServ <- felm( log(SNMServDep) ~ Lag + s + n +bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.NMServ    <- lm( log(SNMServDep) ~ Lag + s + n + bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)

stargazer(fe.Tot, fe.Agr, fe.EM, fe.Con, fe.Dis, fe.Serv, fe.NMServ, 
          omit.stat=c("ser","rsq","f"), align=FALSE, out="Output/Tables/FEEstimationsgrowthPC.tex", star.cutoffs = c(0.10, 0.05, 0.01),
          dep.var.caption  = "Dependent variable: log(value added per worker in 2007) - log(value added per worker in 2000)",
          dep.var.labels   = c("Tot.", "Agr.", "EenM.", "Cons.", "Dis.", "Serv.", "NMServ."), 
          title="Determinants of total value added per worker (with country-fixed effects)", no.space=TRUE)

stargazer(ols.Tot, ols.Agr, ols.EM, ols.Con, ols.Dis, ols.Serv, ols.NMServ, 
          omit.stat=c("ser","rsq","f"), align=FALSE, out="Output/Tables/OLSEstimationsGrowthPC.tex", star.cutoffs = c(0.10, 0.05, 0.01),
          dep.var.caption  = "Dependent variable: log(value added in 2007) - log(value added in 2000)",
          dep.var.labels   = c("Tot.", "Agr.", "EenM.", "Cons.", "Dis.", "Serv.", "NMServ."), 
          title="Determinants of total value added per worker (without country-fixed effects)",  no.space=TRUE)


####################################################################################################
## Now for savingsrate
####################################################################################################

Lag <- log(STot0) 
s   <- log(sTot)
# s[is.na(s)] <- -10
n   <- log(nTot)
fe.Tot    <- felm( s ~ bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.Tot    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong, data=Data)

Lag <- log(SAgr0) 
s   <- log(sAgr)
# s[is.na(s)] <- -10
n   <- log(nAgr)
# n[is.na(n)] <- -10
fe.Agr    <- felm( s ~ bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong+ G(Country) , data=Data)
ols.Agr    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SEenM0) 
s   <- log(sEM)
# s[is.na(s)] <- -10
n   <- log(nEM)
# n[is.na(n)] <- -10
fe.EM     <- felm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.EM    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong, data=Data)
Lag <- log(SCons0) 
s   <- log(sCon) 
# s[is.na(s)] <- -10
n   <- log(nCon)
fe.Con    <- felm( s~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong+ G(Country) , data=Data)
ols.Con    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SDis0) 
s   <- log(sDist) 
# s[is.na(s)] <- -10
n   <- log(nDist)
fe.Dis    <- felm( s ~ bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.Dis    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SServ0) 
s   <- log(sServ) 
# s[is.na(s)] <- -10
n   <- log(nServ)
fe.Serv   <- felm( s~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.Serv    <- lm( s ~ bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)
Lag <- log(SNMServ0) 
s   <- log(sNMServ) 
# s[is.na(s)] <- -10
n   <- log(nNMServ)
fe.NMServ <- felm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong + G(Country) , data=Data)
ols.NMServ    <- lm( s ~  bevtot + popdens + hoogopl + RenDpub + Patent + RenDbus + Bbweg + Bblucht + Cong , data=Data)

stargazer(fe.Tot, fe.Agr, fe.EM, fe.Con, fe.Dis, fe.Serv, fe.NMServ, 
          omit.stat=c("ser","rsq","f"), align=FALSE, out="Output/Tables/FEEstimationssavings.tex", star.cutoffs = c(0.10, 0.05, 0.01),
          dep.var.caption  = "Dependent variable: log(Output per capita in 2007)",
          dep.var.labels   = c("Tot.", "Agr.", "EenM.", "Cons.", "Dis.", "Serv.", "NMServ."), 
          title="Determinants of savingsrate (with country-fixed effects)", no.space=TRUE)

stargazer(ols.Tot, ols.Agr, ols.EM, ols.Con, ols.Dis, ols.Serv, ols.NMServ, 
          omit.stat=c("ser","rsq","f"), align=FALSE, out="Output/Tables/OLSEstimationssavings.tex", star.cutoffs = c(0.10, 0.05, 0.01),
          dep.var.caption  = "Dependent variable: log(Output per capita in 2007)",
          dep.var.labels   = c("Tot.", "Agr.", "EenM.", "Cons.", "Dis.", "Serv.", "NMServ."), 
          title="Determinants of savingsrate (without country-fixed effects)", no.space=TRUE)


rm(list = ls())
