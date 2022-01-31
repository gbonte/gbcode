medical<-function(){
  # =============
  library(ISwR)
  data(thuesen)
  par(ask=TRUE)

  # Collecter les donn?es pertinentes dans 2 vecteurs (X et Y)
  # ==========================================================
  I<-! is.na(thuesen[,"short.velocity"])
  Y<-thuesen[I,"short.velocity"]
  X<-thuesen[I,"blood.glucose"]


  # Visionner les donn?es
  # =====================
  thuesen
  hist(Y)
  plot(X,Y)


  # Estimation des coefficients de r?gression par la m?thode des moindres carr?s
  # et affichage de la droite obtenue ==========================================
  # ==================================
  N<-length(Y)
  x.hat<-mean(X)
  y.hat<-mean(Y)
  S.xy<-sum((X-x.hat)*Y)
  S.xx<-sum((X-x.hat)^2)
  beta.hat.1<-S.xy/S.xx
  beta.hat.0<-y.hat-beta.hat.1*x.hat
  print(paste("beta.hat.0 = ", beta.hat.0))
  print(paste("beta.hat.1 = ", beta.hat.1))
  Y.hat<-beta.hat.0+beta.hat.1*X
  sigma.hat = sqrt( sum( (Y-Y.hat)^2 ) / (N-2) )
  print(paste("sigma.hat = ", sigma.hat))
  x<-seq(min(X),max(X),by=.1)
  lines(x,beta.hat.0+beta.hat.1*x)


  # Test de l'hypoth?se "beta1=0" par un F-test.
  # ============================================
  SS.mod <- sum((Y.hat-mean(Y))^2)
  SS.res <- sum((Y-Y.hat)^2)
  F.value<-SS.mod/(SS.res/(N-2))
  F.pr<-(1-pf(F.value,df1=1,df2=N-2))
  print(paste("R?sultat du F-test: F.value= ", F.value, "; Pr[F >= F.value]= ", F.pr))


  # Test de l'hypoth?se "beta1=0" par un t-test.
  # ============================================
  var.hat.w<-sum((Y-Y.hat)^2)/(N-2)
  beta.bar<-0
  t.value<-(beta.hat.1-beta.bar)*sqrt(S.xx)/(sqrt(var.hat.w))
  t.pr<-(1-pt(t.value,df=N-2))*2
  print(paste("R?sultat du t-test: t.value= ",t.value, "; Pr[|T| >= t.value]= ", t.pr))


  # Calcul d'un intervalle de confiance pour beta1
  # ==============================================
  alpha<-0.05
  conf.interval.min<-beta.hat.1-qt(alpha/2,df=N-2,lower.tail=FALSE)*sqrt(var.hat.w/S.xx)
  conf.interval.max<-beta.hat.1+qt(alpha/2,df=N-2,lower.tail=FALSE)*sqrt(var.hat.w/S.xx)
  print(paste("Intervalle de confiance pour beta1=(", conf.interval.min, ",", conf.interval.max, ")"))



  # Tester la commande R qui fait la r?gression automatiquement
  # ===========================================================
  summary(lm(Y~X))
}
