data{
  real Y[10];
  
  }
  
parameters{
  real mu;
  //real sigma;
  }
  
  model {
  for (i in 1:10){
    Y[i]~normal(mu,1);
  }
  mu~normal(1.5,0.1);
  //sigma~1;
}
