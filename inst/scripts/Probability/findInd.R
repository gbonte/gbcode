
## Bivariate binary setting
##        y=0 | y=1
##      ------------------------
##  x=0 | p00 | p01   || p00+p01
##  x=1 | p10 | p11   || p10+p11
##      ----------------------
##   p00+p10  | p01+p11 
##
## Task: given p01 and p10, find the other probability weights 
## in order to satisfy the independence property 
p01=0.15
p10=0.3

## (p01+p11)(p10+p11)=p11
## p01*p10+p01 *p11+ p10 *p11 +p11^2 -p11=0
##
# p11^2+(p01+p10-1)p11+p01p10=0


R=polyroot(c(p01*p10,p01+p10-1,1))

if (all(abs(Im(R))<0.001)){
  reR=Re(R)
  p11=reR[which(reR<=1)]
  p00=1-p11-p10-p01
  cat("p00=",p00, "p01=",p01, "p10=", p10, " p11=",p11,"\n")
  print((p10+p11)*(p01+p11)-p11)
  
  print((p01+p00)*(p00+p10)-p00)
  print((p01+p00)*(p01+p11)-p01)
  print((p10+p00)*(p10+p11)-p10)
} else {
  print("no real solution")
}