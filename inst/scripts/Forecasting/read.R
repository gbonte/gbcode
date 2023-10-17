library(gstat)
library(readr)
STdata<-list()
STnames<-NULL


###

dirname="/Users/gianlucabontempi/Dropbox/bontempi_office/Rlang/gbcode2/inst/scripts/Forecasting/data"

sensBxl  <- read_excel(paste(dirname,"/dataset_per_minutes.xlsx",sep=""))
D=t(as.matrix(sensBxl))
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"sensBxl")

###
ped1 <- read_csv(paste(dirname,"/PedTS1.csv",sep=""))
D=as.matrix(ped1)
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pedestrian1")
###
ped2 <- read_csv(paste(dirname,"/PedTS2.csv",sep=""))
D=as.matrix(ped2)
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pedestrian2")
###
ped3 <- read_csv(paste(dirname,"/PedTS3.csv",sep=""))
D=as.matrix(ped3)
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pedestrian3")
###
ped4 <- read_csv(paste(dirname,"/PedTS4.csv",sep=""))
D=as.matrix(ped4)
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pedestrian4")
###
ped5 <- read_csv(paste(dirname,"/PedTS5.csv",sep=""))
D=as.matrix(ped5)
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pedestrian5")

## 
load(paste(dirname,"/TSlorenz96.Rdata",sep=""))
D=TSlor96.10
STdata<-c(STdata,list(D))
STnames<-c(STnames,"lorenz96.10")
###

load(paste(dirname,"/TSlorenz96.Rdata",sep=""))
D=TSlor96.100
STdata<-c(STdata,list(D))
STnames<-c(STnames,"lorenz96.100")
###

pruschadays <- read_table(paste(dirname,"/Pruscha_Days5.csv",sep=""))
D=as.matrix(pruschadays[,5:14])
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pruschadays")

pruschayears <- read_table(paste(dirname,"/Pruscha_Years5.csv",sep=""))
D=as.matrix(pruschayears[,3:12])
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"pruschayears")


###
data(wind)
D=wind[,4:15]
print(dim(D))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"wind")

###

jena_climate <- read_csv(paste(dirname,"/jena_climate_2009_2016.csv",sep=""))
D=as.matrix(jena_climate[seq(1,NROW(jena_climate),by=12),c(0, 1, 5, 7, 8, 10, 11)+2])
## hourly subsampling and feat sel according to https://keras.io/examples/timeseries/timeseries_weather_forecasting/
STdata<-c(STdata,list(D))
STnames<-c(STnames,"jena_climate")
###

panama_kaggle_train <- read_csv(paste(dirname,"/panama_kaggle_train.csv",sep=""))
D=as.matrix(panama_kaggle_train[,2:14])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"panama_kaggle")
###
Paris_mobility_final_df_real <- read_csv(paste(dirname,"/Paris_mobility_final_df_real.csv",sep=""))
D=as.matrix(Paris_mobility_final_df_real[,3:6])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"Paris_mobility")
###
transformer_ETTh1 <- read_csv(paste(dirname,"/transformer.ETTh1.csv",sep=""))
D=as.matrix(transformer_ETTh1[,2:7])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"transformer_ETTh1")
###
transformer_ETTm1 <- read_csv(paste(dirname,"/transformer.ETTm1.csv",sep=""))
D=as.matrix(transformer_ETTm1[,2:7])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"transformer_ETTm1")

###

transformer_ETTh2 <- read_csv(paste(dirname,"/transformer.ETTh2.csv",sep=""))
D=as.matrix(transformer_ETTh2[,2:7])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"transformer_ETTh2")

###
transformer_ETTm2 <- read_csv(paste(dirname,"/transformer.ETTm2.csv",sep=""))
D=as.matrix(transformer_ETTm2[,2:7])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"transformer_ETTm2")

###
visuelle_gtrends_data <- read_csv(paste(dirname,"/visuelle_gtrends_data.csv",sep=""))
D=as.matrix(visuelle_gtrends_data[,2:96])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"visuelle_gtrends")

###
electricity <- read_csv(paste(dirname,"/electricity.csv",sep=""),col_names=FALSE)
D=as.matrix(electricity)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"electricity")

###
exchange <- read_csv(paste(dirname,"/exchange_rate.csv",sep=""),col_names=FALSE)
D=as.matrix(exchange)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"exchange")

##

solar <- read_csv(paste(dirname,"/solar_AL.csv",sep=""),col_names=FALSE)
D=as.matrix(solar[100:NROW(solar),])
STdata<-c(STdata,list(D))
STnames<-c(STnames,"solar")


##

traffic <- read_csv(paste(dirname,"/traffic.csv",sep=""),col_names=FALSE)
D=as.matrix(traffic)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"traffic")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Aotizhongxin_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA1=PRSA[,c(6:15,17)]
D=as.matrix(PRSA1)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Aotizhongxin")


PRSA<- read_csv(paste(dirname,"/PRSA_Data_Changping_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA2=PRSA[,c(6:15,17)]
D=as.matrix(PRSA2)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Changping")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Dingling_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Dingling")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Dongsi_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Dongsi")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Guanyuan_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Guanyuan")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Gucheng_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Gucheng")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Huairou_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Huairou")


PRSA<- read_csv(paste(dirname,"/PRSA_Data_Nongzhanguan_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Nongzhanguan")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Shunyi_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Shunyi")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Tiantan_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Tiantan")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Wanliu_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Wanliu")

PRSA<- read_csv(paste(dirname,"/PRSA_Data_Wanshouxigong_20130301-20170228.csv",sep=""),col_names=FALSE,skip=1)
PRSA3=PRSA[,c(6:15,17)]
D=as.matrix(PRSA3)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PRSA_Data_Wanshouxigong")

load(paste(dirname,"/bench.temp.50.Rdata",sep=""))
D=as.matrix(Temp)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.temp.50")

load(paste(dirname,"/bench.temp.100.Rdata",sep=""))
D=as.matrix(Temp)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.temp.100")

load(paste(dirname,"/bench.temp.200.Rdata",sep=""))
D=as.matrix(Temp)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.temp.200")

load(paste(dirname,"/bench.vola.0.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.0.Rdat")

load(paste(dirname,"/bench.vola.1.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.1.Rdat")

load(paste(dirname,"/bench.vola.2.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.2.Rdat")

load(paste(dirname,"/bench.vola.3.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.3.Rdat")

load(paste(dirname,"/bench.vola.4.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.4.Rdat")

load(paste(dirname,"/bench.vola.5.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.5.Rdat")

load(paste(dirname,"/bench.vola.6.Rdata",sep=""))
D=as.matrix(X)
STdata<-c(STdata,list(D))
STnames<-c(STnames,"bench.vola.6.Rdat")


load(paste(dirname,"/WEAdata.Rdata",sep=""))

STdata<-c(STdata,WEAdata)
STnames<-c(STnames,paste("WEA",1:5,sep="."))

load(paste(dirname,"/NOAdata.Rdata",sep=""))

STdata<-c(STdata,NOAdata)
STnames<-c(STnames,paste("NOA",1:4,sep="."))

library("SLBDD")
D = get("PElectricity1344", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"PElectricity1344")


# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017

D = get("FREDMDApril19", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"FREDMDApril19")

D = get("CPIEurope200015", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"CPIEurope200015")

D = get("Stockindexes99world", asNamespace('SLBDD'))
STdata<-c(STdata,list(D[,2:NCOL(D)]))
STnames<-c(STnames,"Stockindexes99world")

D = get("UMEdata20002018", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"UMEdata20002018")

D = get("clothing", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"clothing")


D = get("TaiwanAirBox032017", asNamespace('SLBDD'))
STdata<-c(STdata,list(D))
STnames<-c(STnames,"TaiwanAirBox032017")



for (i in 1:length(STdata)){
  print(STnames[i])
  print(dim(STdata[[i]]))
 ## print(STdata[[i]][1:2,])
  
}

save(file=paste(dirname,"STdata_big.Rdata",sep="/"),list=c("STdata","STnames"))
print(length(STdata))
print(length(STnames))

STdata=STdata[1:10]
STnames=STnames[1:10]
save(file=paste(dirname,"STdata.Rdata",sep="/"),list=c("STdata","STnames"))
print(length(STdata))
print(length(STnames))