svm_nlinExample<-function(){
  library("quadprog")
  library("MASS")
  rm(list=ls())
  set.seed(0)
  normv<-function(x,p=2){
    sum(x^p)^(1/p)

  }
  kernel<-function(x,y,type="radial"){
    if (type=="linear")
      return(x%*%y)
    if (type=="radial")
      return(exp((-(normv(x-y))^2)/10))
  }

  hyperpl<-function(x,x1,alpha,beta0,X,Y){

    y.hat<-0

    for (i in 1:length(Y))
      y.hat<-y.hat+Y[i]*alpha[i]*kernel(X[i,],c(x1,x))

    y.hat+beta0


  }


  x2.hyperpl<-function(x,alpha,beta0,X,Y){


    ##  U<-uniroot(hyperpl,lower=6*min(X[,2])-10,
    ##             upper=6*max(X[,2])+10,x1=x,alpha=alpha,beta0=beta0,X=X,Y=Y)
    ##   U$root

    interv<-seq(min(X[,2]),max(X[,2]),by=.005)
    values<-NULL
    for (i in 1:length(interv))
      values<-c(values,hyperpl(interv[i],x,alpha=alpha,beta0=beta0,X=X,Y=Y))

    interv[which.min(abs(values))]

  }


  separable<-T

  if (!separable){
    gam<-0.05
  } else {
    gam<-Inf
  }
  eps<-0.001

  for ( rep in 1:10){
    N<-15  #number of samples per class
    x1<-cbind(rnorm(N),rnorm(N))
    y1<-numeric(N)+1

    x2<-cbind(rnorm(N,2),rnorm(N,2))
    y2<-numeric(N)-1

    X<-rbind(x1,x2)
    Y<-c(y1,y2)


    ## PLOT Training set
    plot(-2:6, -2:6, type = "n",xlab="x1",ylab="x2")
    points(X[1:N,1],X[1:N,2],col="red")
    points(X[(N+1):(2*N),1],X[(N+1):(2*N),2],col="blue")
    par(ask=TRUE)


    ##########################################
    ########## SVM parametric identification

    Dmat<-array(NA,c(2*N,2*N))
    for (i in 1:(2*N)){
      for (j in 1:(2*N)){
        Dmat[i,j]<-Y[i]*Y[j]*kernel(X[i,],X[j,])
      }
    }

    Dmat<-Dmat+1e-3*diag(2*N)
    d<-array(1,c(2*N,1))

    A<-cbind(Y,diag(2*N))
    b<-numeric(2*N+1)



    if (! separable){
      A<-cbind(A,-diag(2*N))
      b<-c(b,numeric(2*N))
      b[(2*N+2):(4*N+1)]<--gam


      ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
      ## b-> alpha [2N,1]
      ## 1st constraint sum_i y_i*alpha_i=0
      ## 2:(2N+1) constraint alpha_i >=0
      ## (2N+2):(4N+1) constraint -alpha_i>=-gam
    }



    S<-solve.QP(Dmat,dvec=d,Amat=A,meq=1,bvec=b)
    ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
    ## b-> alpha [2N,1]
    ## 1st contraint sum_i y_i*alpha_i=0
    ## 2:(2N+1) constraint alpha_i >=0


    alpha<-S$solution
    alpha[alpha<eps]<-0
    ind.j<-which(alpha>eps & alpha<gam-eps)
    if (all(alpha<=gam+eps) & length(ind.j)>0){
      cat("min value=",S$value,"\n")
      cat("min value2=",-t(d)%*%alpha+(1/2*t(alpha)%*%Dmat%*%alpha),"\n")
      cat("sum_i y_i*alpha_i=0:",alpha%*%Y,"\n")


      beta<-numeric(2)
      for ( i in 1:(2*N))
        beta<-beta+alpha[i]*Y[i]*X[i,]

      ind1<-which(alpha[1:N]>eps)
      ind2<-which(alpha[(N+1):(2*N)]>eps)

      ## PLOT Support Vector
      points(X[ind1,1],X[ind1,2],col="black")
      points(X[(N+ind2),1],X[(N+ind2),2],col="black")


      if (separable){
        beta0<--0.5*(kernel(beta,X[ind1[1],])+kernel(beta,X[N+ind2[1],]))
        marg<-1/normv(beta)

      } else {
        L<-0
        for (i in 1:(2*N)){
          for (j in 1:(2*N)){
            L=L+Y[i]*Y[j]*alpha[i]*alpha[j]*(X[i,]%*%X[j,])
          }
        }


        beta0<-0

        beta0<-(1-Y[ind.j[1]]*beta%*%X[ind.j[1],])/Y[ind.j[1]]

        marg<-1/sqrt(L)

        ## points whose slack variable is positive
        ind3<-which(abs(alpha[1:N]-gam)<eps)
        ind4<-which(abs(alpha[(N+1):(2*N)]-gam)<eps)
        points(X[ind3,1],X[ind3,2],col="yellow") ## red->yellow
        points(X[(N+ind4),1],X[(N+ind4),2],col="green")






      }
      cat("beta=",beta,"\n")
      theta<-atan(-beta[1]/beta[2])
      cat("theta=",theta,"\n")

      x1.ind<-seq(min(X[,1]),max(X[,1]),by=.1)
      x2.ind<-NULL
      x2.ind.l<-NULL
      x2.ind.u<-NULL
      for ( i in 1:length(x1.ind)){
        x2.ind<-c(x2.ind,x2.hyperpl(x1.ind[i],alpha,beta0,X,Y))

        x2.ind.l<-c(x2.ind.l,x2.hyperpl(x1.ind[i],alpha,beta0-1,X,Y))
        x2.ind.u<-c(x2.ind.u,x2.hyperpl(x1.ind[i],alpha,beta0+1,X,Y))
      }
      lines(x1.ind,x2.ind,col="black")
      ## PLOT Separating Hyperplane
      ## abline(b=-beta[1]/beta[2],a=-beta0/beta[2])

      ## PLOT Margin
      lines(x1.ind,x2.ind.l,col="cyan")
      ## abline(b=-beta[1]/beta[2],
      ##      a=-beta0/beta[2]+ marg/(cos(pi-theta)))

      lines(x1.ind,x2.ind.u,col="orange")
      ## abline(b=-beta[1]/beta[2],
      ##       a=-beta0/beta[2]- marg/(cos(pi-theta)))

      title(paste("margin=",marg, ", C=",gam))
      print(marg)
      par(ask=TRUE)
      plot(alpha)
      title("Alpha values")
    } else
      title("Missing solution")
  }
}
