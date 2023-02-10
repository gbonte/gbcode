rm(list=ls())

N=250

F=20 ## number of frequencies
omega=runif(F,1/N,(N-1)/(2*N))
SD=runif(2*F,1,2)
T=1:N

Y=numeric(N)
for (f in 1:F)
  Y=Y+SD[f]*sin(2*pi*omega[f]*T)+SD[F+f]*cos(2*pi*omega[f]*T)


par(mfrow=c(3,1))
plot(T,Y,type="l",main=paste("F=",F))



A=numeric(round(N/2))
B=numeric(round(N/2))
P=numeric(round(N/2))
for (j in 1:round(N/2)){
  A[j]=2/N*sum(Y*cos(2*pi*T*j/N))
  B[j]=2/N*sum(Y*sin(2*pi*T*j/N))
  P[j]=A[j]^2+B[j]^2
}

FFT=fft(Y)
FFT=(Mod(FFT)[2:(round(N/2)+1)])^2/N

plot((1:round(N/2))/N,P,type="l",main="periodogram")
lines((1:round(N/2))/N,FFT,type="l",col="blue")
for (f in 1:F)
  abline(v=omega[f],col="red")
plot((1:round(N/2))/N,FFT,type="l",main="fft")
for (f in 1:F)
  abline(v=omega[f],col="red")

