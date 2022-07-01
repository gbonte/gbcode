

Code from the handbook and book
"Statistical foundations of machine learning"
https://leanpub.com/statisticalfoundationsofmachinelearning



To use it in your R code

library(devtools); install_github("gbonte/gbcode"); require(gbcode)


To change the directory to the one containing scripts 

- setwd(find.package("gbcode"))

To go to the directory of the Forecasting scripts 
- setwd(paste(find.package("gbcode"),"scripts/Forecasting",sep="/"))

To list the Forecasting demos:
- dir(paste(find.package("gbcode"),"scripts/Forecasting/",sep="/"),patt="demo*")
