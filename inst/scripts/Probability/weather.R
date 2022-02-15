
library(tidyverse)

P<-function(model,...){
  M<-as_tibble(Model)
  args <- rlang::enexprs(...)
  
  sum(M%>%filter(!!! args)%>%select(p))
}

Model<-data.frame(z1=
c("CLEAR","CLEAR","CLEAR","CLEAR","CLOUDY","CLOUDY","CLOUDY","CLOUDY"),
z2=c("RISING","RISING","FALLING","FALLING","RISING","RISING","FALLING","FALLING"),
z3=c("DRY","WET","DRY","WET","DRY","WET","DRY","WET"),
p=c(0.4,0.07,0.08,0.1,0.09,0.11,0.03,0.12))


## P(CLEAR,RISING)
P(Model,z1=="CLEAR",z2=="RISING")

## P(CLOUDY)
P(Model,z1=="CLOUDY")

## P(DRY|CLEAR,RISING)
P(Model,z1=="CLEAR",z2=="RISING",z3=="DRY")/P(Model,z1=="CLEAR",z2=="RISING")


P(Model,z1=="CLOUDY",z2=="RISING")+P(Model,z1=="CLOUDY",z2=="FALLING")


P(Model,z1=="CLEAR",z2=="FALLING")/P(Model,z1=="CLEAR")


