rm(list=ls())
library(datasauRus)

library(dplyr)
library(ggplot2)
datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )

ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)

names_datasets=unlist(unique(datasaurus_dozen%>%select("dataset")))
print(names_datasets)

D1=
  data.frame(datasaurus_dozen%>%filter(dataset=="dino")%>%select("x","y"))

D2=
  data.frame(datasaurus_dozen%>%filter(dataset==names_datasets[9])%>%select("x","y"))

cat("Means D1=",apply(D1,2,mean),"\n Means D2=", apply(D2,2,mean) , "\n\n")
cat("Stds D1=",apply(D1,2,sd),"\n Stds D2=",apply(D2,2,sd) , "\n \n")
cat("Cor D1=",cor(D1),"\n Cor D2=",cor(D2) , "\n")

plot(D1)
points(D2,col="red")