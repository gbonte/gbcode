library(fpp3)
library(fable.prophet)
N=200
TS=tsibble(
  date = as.Date(Sys.Date()) + 1:N,
  index='date',
  value = sin(2*pi*((1:N)/28))+rnorm(N,sd=0.12),
  value2 = sin(2*pi*((1:N)/28))+rnorm(N,sd=0.12)
)

fit<-TS%>%model(model1=prophet(value,order=3))

FF<-fit%>%forecast(h=7)
pred=data.frame(FF)[,".mean"]

G1<-global_economy%>%select(Country,Imports)%>%
  spread(Country,Imports)
G2<-global_economy%>%select(Country,Exports)%>%
  spread(Country,Exports)
G3<-global_economy%>%select(Country,Growth)%>%
  spread(Country,Growth)
G<-G1%>%full_join(G2,by='Year')%>%full_join(G3,by='Year')

G<-tibble(G)%>%select(-Year)          

ImportsTS<-data.frame(G)

countna<-function(y)
  length(which(is.na(y)))/length(y)
w<-which(apply(ImportsTS,2,countna)<0.01)
ImportsTS<-ImportsTS[,w]

######################
Ped<-pedestrian%>%
  select(Sensor,Count)%>%
  spread(Sensor,Count)

PedTS<-data.frame(tibble(Ped)%>%select(-Date_Time ) )
w<-which(!is.na(apply(PedTS,1,sum)))
wranges<-which(diff(w)>1)
PedTS1<-PedTS[w[1:wranges[1]],]
write.table(PedTS1,file="./data/PedTS1.csv",sep=",",row.names = FALSE,col.names = TRUE)
PedTS2<-PedTS[w[(1+wranges[1]):wranges[2]],]
write.table(PedTS2,file="./data/PedTS2.csv",sep=",",row.names = FALSE,col.names = TRUE)
PedTS3<-PedTS[w[(1+wranges[2]):wranges[3]],]
write.table(PedTS3,file="./data/PedTS3.csv",sep=",",row.names = FALSE,col.names = TRUE)
PedTS4<-PedTS[w[(1+wranges[3]):wranges[4]],]
write.table(PedTS4,file="./data/PedTS4.csv",sep=",",row.names = FALSE,col.names = TRUE)
PedTS5<-PedTS[w[(1+wranges[9]):wranges[10]],]
write.table(PedTS5,file="./data/PedTS5.csv",sep=",",row.names = FALSE,col.names = TRUE)


###########################
A1<-ansett%>% filter(Class=="Economy")%>% 
  mutate(Passengers=Passengers/1000)%>%select(Airports,Passengers)%>%
  spread(Airports,Passengers)
  
A2<-ansett%>% filter(Class=="Business")%>% 
  mutate(Passengers=Passengers/1000)%>%select(Airports,Passengers)%>%
  spread(Airports,Passengers)

A<-A1%>%full_join(A2,by='Week')

A<-tibble(A)%>%select(-Week) 
PassTS<-data.frame(A)


w<-which(apply(PassTS,2,countna)<0.01)
PassTS<-PassTS[,w]

##################

A1<-tibble(tourism)%>% filter(Purpose=="Business")%>% 
  dplyr::select(Quarter,Region,Trips)%>%
  spread(Region,Trips)

A2<-tibble(tourism)%>% filter(Purpose=="Visiting")%>% 
  dplyr::select(Quarter,Region,Trips)%>%
  spread(Region,Trips)

A<-A1%>%full_join(A2,by='Quarter')

A<-tibble(A)%>%select(-Quarter) 
TourTS<-data.frame(A)


w<-which(apply(TourTS,2,countna)<0.01)
TourTS<-TourTS[,w]

write.table(TourTS,file="./data/TourTS.csv",sep=",",row.names = FALSE,col.names = TRUE)