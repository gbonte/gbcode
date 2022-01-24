N=1000
X1=runif(N,-1,1)
X2=runif(N,-1,1)
r=0.5
Y=numeric(N)
I1=which(X1^2+X2^2>r)
Y[I1]=1
I0=setdiff(1:N,I1)

plot(X1[I1],X2[I1],col="green",xlab="x1",ylab="x2", 
     main="Original space")

points(X1[I0],X2[I0],col="red")

X3=X1^2+X2^2

plot(X1[I1],X3[I1],col="green",xlab="x1",ylab="x3",
     main="Transformed space",ylim=c(-0.1,1))

points(X1[I0],X3[I0],col="red")
