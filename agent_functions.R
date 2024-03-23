#LIBRARIES
library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(writexl)
library(readxl)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)

#DETERMINE HORIZON OF VECTORS FOR EACH TIME STEP
horizons<-function(ts, M){
hor<-list() #h is a list/array/family of all possible horizons vectors corresponding to each maximum possible horizon

#2 cases for horizons depending on input
if(M>=max(ts)){
#Max of investment horizon can only look as far back as data you have (same as ts), 0 day horizon doesn't have meaning since horizons include today, 1 day horizon is today only etc
for (t in 1:(max(ts))) {
hor[[t]]<-seq(from = 1, to = ts[t], by =1) #assign i's sequentially up to max possible allowed by data history
  }
}

if(M<max(ts)){
#for data with less than or equal to M days of history cap it at ts
for (t in 1:M) {
hor[[t]]<-seq(from = 1, to = ts[t], by =1) #for the first M horizons assign max i allowed by data same as before
  }
#For data with more than M days of history cap max i it at M
for (t in (M+1):(max(ts))) {
hor[[t]]<-seq(from = 1, to = M, by =1) #max i is capped at M
  }
}
return(hor)
}

#CALCULATE GAMMA BASED ON POWER LAW DECAY FOR EACH i
gamma<-function(i) { 
  return(i^e) 
}

#Rprime using Rnet (Rn) to be equal to Rt, Use Rweight (Rw) for the calculation
wareturn<-function(ts,Rt,hor,gam,k){
Rp<-Rt
for (t in 1:(max(ts))) {
  Rn<-Rt[t]
  Rw<-Rt[t]
  i<-2
  while (i <= max(hor[[t]])) {

    for(j in 1:(i-1)) {
      Rn<-(Rn + Rt[t-j])
    }
    Rw<-Rw + (gam[[t]][i])*Rn
    Rn<-Rt[t]
    i<-i+1
  }
  Rp[t]<-k*Rw 
 }
 return(Rp)
}

