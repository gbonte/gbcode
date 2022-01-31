######################################################
# movie.R
# Estimation de la "cote" d'un film, cette cote ?tant d?finie
# comme un entier entre 1 et 10 (voir cours).
# Cette cote, theta, est vu comme la moyenne inconnue d'une distribution
# normale d'?cart-type 2.
# La distribution a priori de theta est donn?e par:
# {0, 0, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1, 0, 0}
# et l'?chantillon DN est {6,7,8,9}.
# Le script calcule et affiche les distributions a priori
# et a posteriori.
######################################################

Movie<-function(){

  # Initialisations diverses.
  # =========================
  rm(list=ls())
  par(ask=TRUE)
  n <- 10
  pr.theta <- c(0, 0.0, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1, 0, 0)  # Distribution a priori.
  po.theta <- rep(0,n)  # Distribution a posteriori.
  DN <- c(6,7,8,9) # Echantillon.
  var <- 4 # Variance connue.
  lik <- function(m,D,var) {   # Fonction de vraisemblance empirique (empirical likelihood
    N<- length(D)	     # function), i.e. le produit de la densit? de probabilit?
    Lik<-1		     # d'une variable N(m,var) aux diff?rents points d'un
    for (i in 1:N)      # ?chantillon D.
      Lik<-Lik*dnorm(D[i],m,sqrt(var))
    Lik
  }



  # Calcul de la distribution a posteriori.
  # =======================================
  marg<-0
  for (theta in 1:n){
    marg<-marg+lik(theta,DN,var)*pr.theta[theta]
  }

  for (theta in 1:n){
    po.theta[theta]<-(lik(theta,DN,var)*pr.theta[theta])/marg;
  }




  # Affichage des distributions a priori et a posteriori.
  # =====================================================
  plot(1:n,pr.theta,type="b",ylim=c(0,0.4),main='Distribution of theta',col="red")
  lines(1:n,po.theta,col="blue",type="b")
  legend(2,.4,c("Prior","Posterior"),col=c("red","blue"),lty=1)

}
