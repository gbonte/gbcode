rm(list=ls())
library(tidyverse)
#library(lubridate)
#library(timetk)
#library(parsnip)
#library(rsample)
library(tidymodels)
library(modeltime)
library(modeltime.gluonts)


gluonpred<-function(TS,H=10){
  N=length(TS)
  DatesX=as.Date(1:(N+H),origin="2000/1/2")
  
  X<-data.frame(DatesX[1:N],
                rep("id",N),
                TS)
  
  colnames(X)=c("date","id","value")
  
  model_fit <- deep_ar(
    id                    = "id",
    freq                  = "D",
    prediction_length     = H,
    lookback_length       = 4,
    epochs                = 2
  ) %>%
    set_engine("gluonts_deepar")
  
  model_fit <- nbeats(
    id                    = "id",
    freq                  = "D",
    prediction_length     = H,
    lookback_length       = 4,
    epochs                = 2
  ) %>%
    set_engine("gluonts_nbeats")
 
  invisible(capture.output(model_fit_deepar<-model_fit%>%
    fit(value ~ date+id, X)))
  
  # ---- FORECAST ----
  new_data <- tibble(
    id   = factor("id"),
    date = DatesX[(N+1):(N+H)]
  )
  
  P<-unlist(predict(model_fit_deepar, new_data)%>%
              dplyr::select(".pred"))
  return(P)
}

N=100
TS=sin(2*pi*(1:N)/20)
P<-gluonpred(TS,H=10)

plot(c(TS,P),type="l")
lines(c(NA*TS,P),col="red")




