
rm(list=ls())

P=0
while(any(P==0)){
  P=round(runif(8),1)
  P=P/sum(P)
}

P=c(0.4,0.07,0.08,0.1,0.09,0.11,0.03,0.12)

## probabilistic weights

print(P)
if (abs(sum(P)-1)>0.01)
  stop("Error in P")

vec=1:length(P)
AllSets <- unlist(lapply(vec,    # Get all combinations
                         combinat::combn, 
                         x = vec,
                         simplify = FALSE), 
                  recursive = FALSE)


Nindep=0
for (i in 1:(length(AllSets)-1))
  for (j in setdiff(i:(length(AllSets)-1),i)){
    Event1=AllSets[[i]]
    Event2=AllSets[[j]]
    Inters=intersect(Event1,Event2)
    if (length(Inters)>0){
      P1=sum(P[Event1])
      P2=sum(P[Event2])
      PI=sum(P[Inters])
      
      if (PI==P1*P2){
        
        
        cat("E1=",Event1,"E2=",Event2,
            "P1=",P1,"P2=",P2,"PI=",PI,"\n")
        Nindep=Nindep+1
      }
    }
    
    
  }
print(P)
cat("The number of independent events is ", Nindep,"\n")