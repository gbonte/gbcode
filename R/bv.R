#################################################################
# bv.R					     		        #
#################################################################
## Dataset D_N={x_i,y_i} o˘:			#
#	  y_i = beta_0 + beta_1x_i + w_i			#
#   with known  beta_0  and beta_1 and known w=Normal(0, sigma) 	#
#   							#
## Experimental analysis of bias and variance of the least sqaure estimate 	#
#   and comparison with theoretical results
#################################################################

BiasLS<-function(){

  # preliminary
  # =============
  rm(list=ls())
  par(ask=TRUE)
  X<-seq(-10,10,by=0.04) 	# les x_i, ils sont vus comme fixÈs.
  beta0<--1 		# y_i = -1 + x_i + Normal(0,5)
  beta1<-1
  sd.w<-10
  N<-length(X)
  R<-10000 		# le nombre d'itÈrations pour les simulations


  #                 ^            ^                   ^
  # Calcul de Var(beta_1), de Var(beta_0), et de (sigma_w)≤, en thÈorie et par simulation
  # =====================================================================================
  beta.hat.1<-numeric(R)
  beta.hat.0<-numeric(R)
  var.hat.w<-numeric(R)
  Y.hat<-array(NA,c(R,N))
  x.hat<-mean(X)
  S.xx<-sum((X-x.hat)^2)
  for (r in 1:R){
    Y<-beta0+beta1*X+rnorm(N,sd=sd.w)
    y.hat<-mean(Y)
    S.xy<-sum((X-x.hat)*Y)

    beta.hat.1[r]<-S.xy/S.xx
    beta.hat.0[r]<-y.hat-beta.hat.1[r]*x.hat

    Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*X
    var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(N-2)
  }

  # RÈsultat pour beta_1
  var.beta.hat.1<-(sd.w^2)/S.xx
  print(paste("Theoretical var beta1=", var.beta.hat.1, "; Observed =",
              var(beta.hat.1) ))
  hist(beta.hat.1, main=paste("Distribution of beta.hat.1: beta1=", beta1))

  # RÈsultat pour beta_0
  var.beta.hat.0<-(sd.w^2)*(1/N+(x.hat^2)/S.xx)
  print(paste("Theoretical var beta0=", var.beta.hat.0, "; Observed =",
              var(beta.hat.0) ))
  hist(beta.hat.0,main=paste("Distribution of beta.hat.0: beta0=", beta0))

  # RÈsultat pour sigma_w
  hist(var.hat.w,main=paste("Distribution of var.hat.w: var w=", sd.w^2))



  #                                       ^
  # Affichage des diffÈrentes prÈdictions y (droites)
  #===================================================

  plot(X,Y.hat[1,],type="l",
       main=paste("Variance of the prediction:",R, " repetitions"),
       ylim=c(-20,20))
  for (r in 2:R){
    lines(X,Y.hat[r,])
  }



  #                                       ^
  # Illustration empirique du thÈorËme: E[y(x)] = E[y(x)]  pour tout x
  # ==================================================================

  #                                    ^
  # Affichage d'une droite estimant  E[y(x)] (par une moyenne empirique)
  lines(X,apply(Y.hat,2,mean),col="red")

  # Affichage de la droite de rÈgression E[y(x)]= beta0 + beta1 x
  lines(X,beta0+beta1*X,col="green")


  #                                         ^            2             -
  # Illustration empirique du thÈorËme: Var[y(x)] = sigma  [1/N + (x - x) / Sxx]
  # ============================================================================

  #					    ^   ~
  # Etude de la variance des prÈdictions (Var[y | x]), en thÈorie et en pratique.
  for (i in 1:N){
    var.y.hat<-var(Y.hat[,i])

    th.var.y.hat<-sd.w^2*(1/N+((X[i]-x.hat)^2)/S.xx)
    print(paste("Theoretical var predic=",th.var.y.hat, "; Observed =",
                var.y.hat ))
  }
}
