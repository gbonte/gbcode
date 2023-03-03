
Code from the handbook and book
"Statistical foundations of machine learning"
https://leanpub.com/statisticalfoundationsofmachinelearning



To use it in your R code 

> library(devtools); install_github("gbonte/gbcode"); require(gbcode)

Once installed, all the scripts will be available in the root directory of the package. In order to retrieve the directory containing the gbcode package you should type

> system.file(package = "gbcode")

To change the directory to the one containing scripts

> setwd(paste(find.package("gbcode"),"/scripts",sep=""))

If you wish to run a script mentioned in the main text (e.g. the script "Probability/freq.R") without changing the local directory you should run


> source(system.file("scripts","Probability/freq.R",package = "gbcode"))

If you wish to edit a script mentioned in the main text (e.g. the script "Probability/freq.R") without changing the local directory you should run

> edit(file=system.file("scripts","Probability/freq.R",package ="gbcode"))

If you wish to execute a Shiny dashboard (e.g. "leastsquares.R") you should run

> library(shiny) 
> setwd(paste(find.package("gbcode"),"shiny",sep="/"))
> runApp("leastsquares.R")


To go to the directory of the Forecasting scripts 
> setwd(paste(find.package("gbcode"),"scripts/Forecasting",sep="/"))

To list the Forecasting demos:
> dir(paste(find.package("gbcode"),"scripts/Forecasting/",sep="/"),patt="demo*")
