N=seq(2,100000,by=100)

plot(N,N^(-2/(M+2)),ylim=c(0,1))
for (M in seq(2,100,by=5))
  lines(N,N^(-2/(M+2)))