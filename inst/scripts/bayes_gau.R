######################################################
# bayes_gau.R
# Estimation param?trique Bayesienne de la moyenne d'une
# variable normale de variance conue, avec une estimation
# a priori normale de cette moyenne.
######################################################




par(ask=TRUE)
sigma <- 2   # L'?cart-type (connu) de la variable normale dont nous voulons
# estimer la moyenne (theta) est de 2.
theta0 <- 13 # La distribution A PRIORI de theta est une normale de moyenne 13
sigma0 <- 1  # et d'?cart-type 1.
Nmax <- 100
DN<-rnorm(Nmax,0,sigma) # G?n?ration d'un ?chantillon al?atoire gaussien de
# taille 100. L'?cart-type est de 2 comme annonc?
# pr?c?demment. La moyenne (le param?tre que nous
# cherchons ici ? estimer) est quant ? elle fix?e ? 0.



# Calcul de la distribution A POSTERIORI de theta, connaissant un ?chantillon
# ===========================================================================
# al?atoire de taille N allant de 3 ? 100.
# ========================================
for (N in 3:Nmax)
{
  
  # Affichage de l'estimation A PRIORI de theta.
  # ============================================
  I<- seq(-20,20,by=.1)
  plot(I,dnorm(I,theta0,sigma0),type='l',ylim=c(0,1),col="red",main=paste("# observations=",N))
  
  
  # Affichage de l'?chantillon observ?.
  # ===================================
  zeros<-array(0,dim=c(N,1))
  points(DN[1:N],zeros,col='black')
  
  
  # Calcul et affichage de la distribution A POSTERIORI.
  # ===================================================
  mu.hat<-mean(DN[1:N])
  sigma1<-sqrt(((sigma^2)/N)*((sigma0^2)/(sigma0^2+((sigma^2)/N))))
  theta1<-(sigma1^2)*(theta0/(sigma0^2)+N*mu.hat/(sigma^2))
  lines(I,dnorm(I,theta1,sigma1),col='blue')
  legend(12,.8,c("Prior","Posterior"),col=c("red","blue"),lty=1)
}


