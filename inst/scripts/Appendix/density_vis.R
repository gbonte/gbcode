# install.packages("ggplot2")
library(ggplot2)

# Data
set.seed(1)
N=200
X=runif(N,-1,1)
Y=X^3+rnorm(N,sd=0.15)
df <- data.frame(x =X , y = Y)
p<-ggplot(df, aes(x = x, y = y)) + geom_point()+ 
  ggtitle("Raw data")+ theme(
  plot.title = element_text(color="black", size=18, face="bold")
) 
print(p)
browser()
for (bd in c(0.1,0.2,0.3,0.8)){
  str=paste("bdwth=",bd,sep="")
  p<-ggplot(df, aes(x = x, y = y)) + geom_point()+
     ggtitle(str)+
  geom_density_2d_filled(alpha = 0.65,h=c(bd,bd))
 print(p)
 browser() 
}
