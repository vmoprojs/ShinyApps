############################ Limpio todo ####################################
gc() #Vacia la memoria virtual (elimina "basura")
cat("\014") # Limpio consola
rm(list = ls()) #Limpio espacio de trabajo

# source("https://bioconductor.org/biocLite.R")
# biocLite("impute")
library(impute)
library(vegan)

########################### Cargando paquetes ###############################
mainfolder <- "~/Google Drive/DireccionTesis/OjedaOscar"
codefolder <- "/Scripts"
datafolder <- "/Data"
# setwd (paste(mainfolder,datafolder,sep=""))
setwd("~/Google Drive/DireccionTesis/OjedaOscar/Scripts/App/data")

db <- rio::import("pwt91.xlsx", which = "Data")


# Las variables a analizar 
# corresponden a la secccion "National accounts-based variables"
# rgdpna,rconna,rdana,rnna,rkna,rtfpna,rwtfpna,labsh,irr,delta

# rgdpna:	Real GDP at constant 2011 national prices (in mil. 2011US$)
# rconna:	Real consumption at constant 2011 national prices (in mil. 2011US$)
# rdana:	Real domestic absorption at constant 2011 national prices (in mil. 2011US$)
# rnna:	Capital stock at constant 2011 national prices (in mil. 2011US$)
# rkna:	Capital services at constant 2011 national prices (2011=1)
# rtfpna:	TFP at constant national prices (2011=1)
# rwtfpna:	Welfare-relevant TFP at constant national prices (2011=1)
# labsh:	Share of labour compensation in GDP at current national prices
# irr:	Real internal rate of return
# delta:	Average depreciation rate of the capital stock



# Real GDP, employment and population levels:
target.vars <- c("rgdpe","rgdpo","pop","emp","avh","hc")

# Current price GDP, capital and TFP
target.vars <- c("ccon","cda","cgdpe","cgdpo","cn","ck","ctfp","cwtfp")

# National accounts-based variables:
target.vars <- c("rgdpna","rconna","rdana","rnna","rkna","rtfpna",
                 "rwtfpna","labsh","irr","delta")

# Exchange rates and GDP price levels:
target.vars <- c("xr","pl_con","pl_da","pl_gdpo")

# Data information variables:
target.vars <- c("i_cig","i_xm","i_xr","i_outlier","i_irr","cor_exp","statcap")

# Shares in CGDPo:
target.vars <- c("csh_c","csh_i","csh_g","csh_x","csh_m","csh_r")

# Price levels, expenditure categories and capital
target.vars <- c("pl_c","pl_i","pl_g","pl_x","pl_m","pl_n","pl_k")


yrs.base <- 2016
refdatos <- db[db$year==yrs.base,]

pais <- "ECU"
ECbase <- refdatos[refdatos$countrycode==pais,c("countrycode","year",target.vars)]

# ctry <- unique(db$countrycode)

results <- list()
ng <- 10#number of groups
yrs <- unique(db$year)
na.sol <- NULL # NA and NROW
init <- 31 #initiation year
contador <- 1

Ksol = NULL
for (j in  init:length(yrs))
{
  # j = 65#2014
  refdatos <- db[db$year==yrs[j],c("countrycode","year",target.vars)]
  refdatos[refdatos$countrycode==pais,] <- ECbase #replace ECU values
  na.sol <- rbind(na.sol,c(sum(complete.cases(as.matrix(refdatos[,-(1:2)]))),nrow(as.matrix(refdatos[,-(1:2)]))))
  temp1 <- impute.knn(as.matrix(refdatos[,-(1:2)]))$data
  rownames(temp1) <- refdatos[,"countrycode"]
  
  # normalize data:
  spe.norm <- decostand(temp1, "normalize")
  spe.ch <- vegdist(spe.norm, "euc")
  
  kmodel <- pam(spe.ch,ng)
  auxK = cbind(kmodel$clustering,yrs[j])
  Ksol = rbind(Ksol,auxK )
  
  # Find Ecuador's group and data
  
  gbase <- list()
  for(i in 1:ng)
  {
    # i = 1
    gbase[[i]] <- as.vector(refdatos[,1][kmodel$cluster==i])
  }
  gbase
  nbelong <- grep(pais,gbase)# number of group which "ECU" belongs to
  
  
  
  
  Cnamesbase <- db$country[match(gbase[[nbelong]],db$countrycode)]#Country names in EC group
  Cdatabase <- refdatos[match(gbase[[nbelong]],refdatos$countrycode),-(1:2)]# Data in EC group
  temp1.aux <- temp1[match(gbase[[nbelong]],rownames(temp1)),]# Data in EC group
  # rownames(Cdatabase) <- Cnamesbase
  # colnames(temp1.aux) <- nomindices
  # names(Cdatabase) <- c("iso3c","birth_rate","fertility","life_exp","mortality","pob_tot","urban_pop")
  # names(Cdatabase) <- c("iso3c",nomindices)
  
  solbase <- data.frame(countrycode = rownames(temp1.aux),country = Cnamesbase,Cdatabase)
  
  # ST: cleaning outliers inside group
  spe.norm.aux <- decostand(temp1.aux, "normalize")
  centers <- spe.norm.aux[which(kmodel$medoids[nbelong]==rownames(spe.norm.aux)) ,]
  # centers <- kmodel$centers[nbelong,1:7] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.
  distances <- sqrt(rowSums((spe.norm.aux - (centers))^2))
  outliers <- order(distances, decreasing=T)[1:round(.2*nrow(solbase))]
  print(outliers) # these rows are 5 top outliers
  (spe.norm.aux[-outliers,])
  # pairs(temp1.aux)
  nnn <- rownames(spe.norm.aux[-outliers,]) #names of members of ecuador group
  if(!pais%in%nnn) {nnn =  c(pais,nnn)}
  # r.nnn <- ctry$region[match(nnn,tolower(countries_wb$iso3c))] # region nnn belong to
  # table(r.nnn) # number of countries per regiosn that ecuador is similar to
  outliers 
  g.fin <- solbase[solbase$countrycode%in%nnn,]
  # g.fin$region <- r.nnn
  # sol <- cbind(cbind(as.character(g.fin$country)),cbind(g.fin$region))
  # sol <- cbind(cbind(as.character(g.fin$country)),cbind(g.fin$region))
  # END: cleaning outliers inside group
  
  print(yrs[j])
  # sol <- data.frame(country = Cnamesyealy,Cdatayearly)
  # results[[j]] <- sol
  results[[contador]] <- g.fin
  contador <- contador+1
}
names(results) <- paste("year",yrs[init:length(yrs)],sep="")
Ksol = data.frame(  countrycode = rownames(Ksol), Cat = Ksol[,1],year =  Ksol[,2])
gh = as.data.frame.matrix(table(Ksol$year,Ksol$Cat))
str()

head(Ksol)
Ksol[,1]==pais
(p = Ksol[,2][which(Ksol[,3]==2000 & Ksol[,1]=="ECU")])
Ksol[which(Ksol[,3]==2000 & Ksol[,2]==p),]
results$year2000

nbelong


mapdata = get_data_from_map(download_map_data("custom/world-palestine-highres"))

# save(mapdata, file = "mapdata.RData")

getwd()

pw = rio::import("~/Google Drive/DireccionTesis/OjedaOscar/Scripts/App/data/pwt91.xlsx",which = "Data")

pw = pw[pw$year==2017,]
mapdata$`iso-a3` %in% pw$countrycode


mapdata$`iso-a3`
any(duplicated(pw$countrycode))

df1 = data.frame ( isoMap = mapdata$`iso-a3`)
df2 = data.frame (isoMap = pw$countrycode, Val = pw$pl_x)
names(df2) = c("isoMap","Val")
names(df1)
dfVal = merge(df1,df2, by = "isoMap",all.x = TRUE,incomparables = 0)
table(dfVal$Val)


dfVal$isoMap[which(as.character(dfVal$isoMap)==pais)]
dfVal$Val[which((dfVal$isoMap)==pais)]


any(duplicated(dfVal$isoMap))

any(duplicated(df.aux$isoMap))

dfVal[duplicated(dfVal$isoMap),]

df.aux = mapdata %>% 
  select(PROVINCIA = `iso-a3`) %>% arrange(PROVINCIA)
df.aux = data.frame(isoMap = df.aux$PROVINCIA)
head(df.aux)
dfVal1 = plyr::join(df.aux,dfVal, by= "isoMap",type="left")
str(dfVal1)
glimpse(mapdata)

# glimpse(mapdata)

data_ecuador <- (mapdata %>% 
  select(PROVINCIA = `iso-a3`)) %>% arrange(PROVINCIA)%>%
  mutate(X = 1:215, Cat = dfVal$Val)
glimpse(data_ecuador)

data_ecuador <- mapdata %>% 
  select(PROVINCIA = `hc-a2`) %>% 
  mutate(X = 1:215, Cat = c(rep(1,100),rep(2,80),rep(3,35)))



# results$year2015 <- ss
(results$year2017)
na.sol



ss <- results$year2017[,"countrycode"][-which(results$year2017[,"countrycode"]==pais)]
net.dat <- data.frame(countrycode = pais, link = as.character(ss))

simpleNetwork(net.dat,zoom = TRUE)



library(dplyr)
sol = head(bind_rows(results, .id = "year"))
str(sol)



