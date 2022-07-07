rm(list=ls())

setwd(paste(find.package("gbcode"),"scripts/Forecasting",sep="/"))
load("./data/bench.temp.200.Rdata")



D = get("clothing", asNamespace('SLBDD'))
D=remNA(Temp[1:400,1])
S=detectSeason(D,Ls=length(D),debug=FALSE)


par(mfrow=c(4,1))
plot(D,type="l")
lines(S$spattern+S$strend,col="red")
plot(D-S$spattern+S$strend,type="l",main="residual")
plot(S$spattern,type="l",main="seasonal")
plot(S$strend,type="l",main="trend")
