t = 1:200
plot.ts(x  <- 2*cos(2*pi*.2*t)*cos(2*pi*.01*t))# not shown
lines(cos(2*pi*.19*t)+cos(2*pi*.21*t), col=2)# the same
Px = Mod(fft(x))^2; plot(0:199/200, Px, type='o')# the periodogram




n = length(star)
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0)) 
plot(star, ylab="star magnitude", xlab="day")
Per= Mod(fft(star-mean(star)))^2/n
Freq=(1:n -1)/n
plot(Freq[1:50], Per[1:50], type='h', lwd=3, ylab="Periodogram",xlab="Frequency")
u= which.max(Per[1:50])# 22  freq=21/600=.035 cycles/day
uu = which.max(Per[1:50][-u])# 25  freq=25/600=.041 cycles/day
1/Freq[22]; 1/Freq[26]# period = days/cycle
text(.05, 7000,"24 day cycle");
text(.027, 9000,"29 day cycle")### another way to find the two peaks is to order on Per
y = cbind(1:50, Freq[1:50], Per[1:50]);y[order(y[,3]),]