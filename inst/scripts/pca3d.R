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

E=eigen(t(Xtilde)%*%(Xtilde))
## check that t(Xtilde)%*%(Xtilde) = E$vectors%*%diag(E$values)%*%t(E$vectors)
## and that S$d^2=S$d

V2=S$v[,1:2]

EV=eigen(t(Xtilde)%*%Xtilde)$vectors
EV=(S$v)[,1:2]
VY=array(c(EV[3,1],EV[3,2]),c(2,1))
VX=t(EV[1:2,1:2])

b=solve(VX)%*%VY


Z=Xtilde%*%V2

Xtilde2=Z%*%t(V2)
RecE=(Xtilde-Xtilde2) ## reconstruction error

cat("Reconstruction error=",mean(apply(RecE^2,1,sum)), ":",S$d[3]^2/N,"\n")


f <- function(x1, x2,a,b) { a*x1+b*x2 }
z <- outer(x1, x2, f,b[2],b[1])
z[is.na(z)] <- 1
op <- par(bg = "white")

fig <- plot_ly()
fig <- fig %>% add_surface(x=x1,y=x2,z=z)
fig <- fig %>% add_markers(x=Xtilde[,1],y=Xtilde[,2],z=Xtilde[,3],
                           color = I('red'),
                           size = 120)

fig <- fig %>% add_markers(x=Xtilde2[,1],y=Xtilde2[,2],z=Xtilde2[,3],
                           color = I('black'),
                           size = 120)

#fig <- fig %>% add_segments(x=Xtilde[1,1],xend=Dc[1,1],
#                            y=Xtilde[1,2],yend=Dc[1,3],
#                            z=Xtilde[1,3],zend=Dc[1,3],
#                            color = I('green'))
fig

