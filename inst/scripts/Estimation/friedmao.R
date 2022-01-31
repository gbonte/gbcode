friedmao<-function(y,conf.level=0.95,post=FALSE){
  k<-ncol(y) # number of algorithms
  n<-nrow(y) # number of test cases
  C<-0
  r<-t(apply(y, 1, rank,na.last=FALSE))
  A<-sum(as.vector(r)^2)
  R<-apply(r, 2, sum)
  alpha<-1-conf.level
  TIES<-tapply(r, row(r), table)
  STATISTIC<-((12 * sum((R - n * (k + 1) / 2)^2)) /
              (n * k * (k + 1)
               - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                  (k - 1))))
  PARAMETER<-k-1
  PVAL<-pchisq(STATISTIC, PARAMETER, lower = FALSE)
  o<-order(R)
  if (!is.nan(PVAL) && (PVAL<alpha) && post){
    # the pre-test has shown significance: let's go for the post-tests 
    
    C<-diag(k)
    colnames(C)<-colnames(y)[o]

    t<-qt(1-alpha/2,(n-1)*(k-1))*(2*(n*A-sum(R^2))/((n-1)*(k-1)))^(1/2)

    for (i in 1:(k-1))
      for (j in (i+1):k) 
        if (abs(R[o[j]]-R[o[i]])>t) 
          break
        else
          C[i,j]<-1
  }
  list(C=C,order=o)
}

