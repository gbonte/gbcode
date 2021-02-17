## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


library(plotly)

N=100000
X1=runif(N,-1/2,1/2)
X2=runif(N,-1/2,1/2)
X3=runif(N,-1/2,1/2)
X4=runif(N,-1/2,1/2)
Z=X1*X2*X3

Ig=which(Z<0)
Ir=which(Z>0)
Y=numeric(N)
Y[Ig]=1
Y[Ir]=-1

fig <- plot_ly()
fig <- fig %>% add_markers(x=X1[Ig],y=X2[Ig],z=X3[Ig],color = I('green'))
fig <- fig %>% add_markers(x=X1[Ir],y=X2[Ir],z=X3[Ir],color = I('red'))
fig

X=cbind(X1,X2,X3)

cor(X,Y)
cor(Z,Y)