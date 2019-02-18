## Monty Hall problem
## Simulation of the two strategies gains
rm(list=ls())
set.seed(0)

R=20000
win=0
win2=0
for (r in 1:R){
  
  Car=sample(1:3,1)
  Pick=sample(1:3,1)
  
  Open=sample(setdiff(1:3,unique(c(Pick,Car))),1)
  
  if (length(setdiff(1:3,unique(c(Pick,Car))))==1)
    Open=setdiff(1:3,unique(c(Pick,Car)))
  
  
  
  Pick2=sample(setdiff(1:3,c(Pick,Open)),1)
  
  if (length(setdiff(1:3,c(Pick,Open)))==1)
    Pick2=setdiff(1:3,c(Pick,Open))
  
  
  if (Pick==Car)
    win=win+1
  
  if (Pick2==Car)
    win2=win2+1
  
  if (Pick==Pick2)
    stop("Error")
  cat("Car door=",Car,"Pick=",Pick, "Open=",Open,"Pick2=",Pick2," : Change: % wins=",win2/r, "No Change: % wins=",win/r,"\n")
 # invisible(readline(prompt="Press [enter] to continue"))
 
}