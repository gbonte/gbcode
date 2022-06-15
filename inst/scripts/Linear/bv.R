
## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

#################################################################
# bv.R					     		        #
#################################################################
## Dataset D_N={x_i,y_i} :			
#	  y_i = beta_0 + beta_1x_i + w_i			#
#   with known  beta_0  and beta_1 and known w=Normal(0, sigma) 	#
#   						
## Experimental analysis of bias and variance of the least sqaure estimate 	#
#   and comparison with theoretical results
#################################################################



# preliminary
# =============
rm(list=ls())
par(ask=TRUE)
X<-seq(-10,10,by=0.04) 	# fixed xi
beta0<--1 		# y_i = -1 + x_i + Normal(0,5)
beta1<-1
sd.w<-10
N<-length(X)
R<-10000 		# number of MC trials


#                 ^                ^                ^
# Computation of Var(beta_1), and  Var(beta_0), and (sigma_w)≤, analytically and by simulation
# =====================================================================================
beta.hat.1<-numeric(R)
beta.hat.0<-numeric(R)
var.hat.w<-numeric(R)
Y.hat<-array(NA,c(R,N))
x.bar<-mean(X)
S.xx<-sum((X-x.bar)^2)
for (r in 1:R){
  Y<-beta0+beta1*X+rnorm(N,sd=sd.w)
  y.bar<-mean(Y)
  S.xy<-sum((X-x.bar)*Y)
  
  beta.hat.1[r]<-S.xy/S.xx
  beta.hat.0[r]<-y.bar-beta.hat.1[r]*x.bar
  
  Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*X
  var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(N-2)
}

# 
var.beta.hat.1<-(sd.w^2)/S.xx
print(paste("Theoretical var beta1=", var.beta.hat.1, "; Observed =",
            var(beta.hat.1) ))
hist(beta.hat.1, main=paste("Distribution of beta.hat.1: beta1=", beta1))

# beta_0
var.beta.hat.0<-(sd.w^2)*(1/N+(x.bar^2)/S.xx)
print(paste("Theoretical var beta0=", var.beta.hat.0, "; Observed =",
            var(beta.hat.0) ))
hist(beta.hat.0,main=paste("Distribution of beta.hat.0: beta0=", beta0))

# sigma_w
hist(var.hat.w,main=paste("Distribution of var.hat.w: var w=", sd.w^2))



#                                       ^
# Plot of predictions
#===================================================

plot(X,Y.hat[1,],type="l",
     main=paste("Variance of the prediction:",R, " repetitions"),
     ylim=c(-20,20))
for (r in 2:R){
  lines(X,Y.hat[r,])
}



#                             ^
# Empirical validation of : E[y(x)] = E[y(x)]  for all x
# ==================================================================

#                                    ^
# Plot of straigh line estimating  E[y(x)] (empirical mean)
lines(X,apply(Y.hat,2,mean),col="red")

# Affichage de la droite de rÈgression E[y(x)]= beta0 + beta1 x
lines(X,beta0+beta1*X,col="green")


#                              ^            2             -
# Empirical illustration : Var[y(x)] = sigma  [1/N + (x - x) / Sxx]
# ============================================================================

#					                              ^   ~
# Study of variance of predictions (Var[y | x]), analytical and in simulation.
for (i in 1:N){
  var.y.hat<-var(Y.hat[,i])
  
  th.var.y.hat<-sd.w^2*(1/N+((X[i]-x.bar)^2)/S.xx)
  print(paste("Theoretical var predic=",th.var.y.hat, "; Observed =",
              var.y.hat ))
}

