library(bnlearn)

bn = model2network("[Z1][Z4|Z2:Z3][Z5|Z3][Z3|Z1][Z2|Z1][Z6|Z4]")
plot(bn)

n=6
nodes=paste("Z",1:n,sep="")

n3=nodes[c(6,2,4)]

ds=dsep(bn, n3[1], n3[2], n3[3])
if (ds){
  print(paste(n3[1],"is d-separated from", n3[2],"|",n3[3] ))
}else{
  print(paste(n3[1],"is d-connected to", n3[2],"|",n3[3] ))
}


n3=nodes[c(2,3,4)]

ds=dsep(bn, n3[1], n3[2], n3[3])
if (ds){
  print(paste(n3[1],"is d-separated from", n3[2],"|",n3[3] ))
}else{
  print(paste(n3[1],"is d-connected to", n3[2],"|",n3[3] ))
}

for (r in 1:10){
  n3=sample(nodes,3)
  ds=dsep(bn, n3[1], n3[2], n3[3])
  if (ds)
    print(paste(n3[1],"is d-separated from", n3[2],"|",n3[3] ))
  else
    print(paste(n3[1],"is d-connected to", n3[2],"|",n3[3] ))
}





