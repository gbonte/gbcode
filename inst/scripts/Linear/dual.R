## Numerical check of the push-through identity
Delta=0
for (r in 1:1000){
  N=100
  n=20
  lambda=0.1
  X=array(rnorm(N*n),c(N,n))
  Delta=Delta+
    abs(sum(solve(t(X)%*%X+lambda*diag(n))%*%t(X)-t(X)%*%solve(X%*%t(X)+lambda*diag(N))))
  print(Delta )
}