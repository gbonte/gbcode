rm(list=ls())

muz=2
sdz=1
N=100

S=10000
B=10000

muhat2.1=NULL
muhat2.2=NULL
muhat2.3=NULL

for (s in 1:S){
  DN=rnorm(N,muz,sd=sdz)
  muhat2.1=c(muhat2.1,mean(DN)^2)
  muhat2.2=c(muhat2.2,sum(DN^2)/N)
  muhat2.3=c(muhat2.3,sum(DN)^2/N)
}

print(mean(muhat2.1))
print(mean(muhat2.2))
print(mean(muhat2.3))

muz^2+sdz^2/N  ## (muz^2+(sdz^2)+(N-1)*muz^2)/N=(N muz^2 + (sdz^2))/N
(muz^2+(sdz^2))
B1=muz^2+sdz^2/N-muz^2

B2=(muz^2+(sdz^2))-muz^2

B3=(muz^2+(sdz^2)+(N-1)*muz^2)-muz^2



muhat2=mean(DN)^2
muhatb=NULL
muhatb2=NULL
muhatb3=NULL
for (b in 1:B){
  Ib=sample(N,rep=TRUE)
  Db=DN[Ib]
  muhatb=c(muhatb,(mean(Db)^2))
  muhatb2=c(muhatb2,sum(Db^2)/N)
  muhatb3=c(muhatb3,sum(Db)^2/N)
}

bB1=mean(muhatb)-muhat2
bB2=mean(muhatb2)-muhat2
bB3=mean(muhatb3)-muhat2

cat("B1=",B1, "B2=", B2, "B3=", B3, "\n",
    "mcB1=", mean(muhat2.1)-muz^2,
    "mcB2=", mean(muhat2.2)-muz^2,
    "mcB3=", mean(muhat2.3)-muz^2,
    "\n bB1=",bB1, "bB2=", bB2, "bB3=", bB3, "\n")