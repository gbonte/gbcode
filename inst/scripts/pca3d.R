## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
library(plotly)

N=500

X1=rnorm(N,sd=1)
X2=rnorm(N,sd=1)
X3=rnorm(1)*X1+rnorm(1)*X2+rnorm(N,sd=5.2)


Xtilde=scale(cbind(X1,X2,X3))

x1 <- seq(-3, 3, length= 30)
x2 <- x1


S=svd(Xtilde)
V2=S$v[,1:2]

EV=eigen(t(Xtilde)%*%Xtilde)$vectors
EV=(S$v)[,1:2]
VY=array(c(EV[3,1],EV[3,2]),c(2,1))
VX=t(EV[1:2,1:2])

b=solve(VX)%*%VY


Z=Xtilde%*%V2

Dc=Z%*%t(V2)

f <- function(x1, x2,a,b) { a*x1+b*x2 }
z <- outer(x1, x2, f,b[2],b[1])
z[is.na(z)] <- 1
op <- par(bg = "white")

fig <- plot_ly()
fig <- fig %>% add_surface(x=x1,y=x2,z=z)
fig <- fig %>% add_markers(x=Xtilde[,1],y=Xtilde[,2],z=Xtilde[,3],
                           color = I('red'),
                           size = 120)

fig <- fig %>% add_markers(x=Dc[,1],y=Dc[,2],z=Dc[,3],
                           color = I('black'),
                           size = 120)

#fig <- fig %>% add_segments(x=Xtilde[1,1],xend=Dc[1,1],
#                            y=Xtilde[1,2],yend=Dc[1,3],
#                            z=Xtilde[1,3],zend=Dc[1,3],
#                            color = I('green'))
fig

