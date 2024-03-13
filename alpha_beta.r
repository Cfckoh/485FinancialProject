library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(writexl)
library(readxl)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)

#How will we change the process if the data is big data
#INITIALIZATION, using the one year tie horizon as avg or 250 trading days/year
market_data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/Index_Data.csv") %>%
slice(-1:-5) %>% setNames(unname(.[1,])) %>% slice(-1) %>% 
rename(Y = PX_LAST) %>% mutate(Y = as.numeric(Y)) %>% 
rename(V = PX_VOLUME) %>% mutate(V = as.numeric(V)) %>%
mutate(Date = as_date(Date, format = "%m/%d/%Y")) %>% mutate(Date_Num = as.numeric(Date)) %>%
arrange(Date_Num) %>% mutate(ts = (row_number() - 1)) %>% select(sort(names(.))) %>%
mutate(lnY = log(Y)) %>% mutate(R = log(Y/lag(Y))) %>% mutate(Rm = mean(R, na.rm=T)) %>% mutate(Rsq = R*R) %>% 
mutate(Rsqm = mean(Rsq, na.rm=T)) %>% mutate(Std = sqrt(Rsqm - Rm*Rm)) %>% mutate(r = (R - Rm)/Std) %>% 
mutate(Rabs = abs(R))


#PARAMETERS for generating Agent Matrix
#try to generate agents as matrix as well as Rprime vector

#time step currently in days can get more granular depending on data input, can't use ts = 0
ts<-market_data$ts[-c(1)]
#Max investment horizon, can only look as far back as data you have, 0 day horizon doesn't have meaning since horizons include today, 1 day horizon is today only etc
M<-ts

#weight of investment horizon, i (more weight to recent horizons) looks at net Return over past days
h<-list() #h is a list/array/family of all possible horizons corresponding to each maximum possible horizon
eta<--1.12
gamma<-function(i) {
  return(i^eta)
}

#initialize horizons which include today
for (t in 1:(max(ts))) {
h[[t]]<-seq(from = 1, to = M[t], by =1)
}

#gamma 0 is not defined can't have a 
g <- map(h, gamma)
#proportionality constant
k<-1

#test make sure we have all investment horizons
h[[2]]
g[[3]]

#calculation of Rprime
R<-market_data$R[-c(1)]
#Initialize Rprime and Rnet to be equal to R
Rprime<-R


for (t in 1:(max(ts))) {
  Rnet<-R[t]
  Rw<-R[t]
  i<-2
  while (i <= max(h[[t]])) {

    for(j in 1:(i-1)) {
      Rnet<-(Rnet + R[t-j])
    }
    Rw<-Rw + (g[[t]][i])*Rnet
    Rnet<-R[t]
    i<-i+1
  }
  Rprime[t]<-k*Rw 
}

Rprime<-as.data.frame(Rprime) %>% rbind(NA,.)

#add Rprime back to market_data and create bull/bear flag
market_data<-data.frame(market_data,Rprime) %>% 
  mutate(BB = case_when(Rprime > 0 ~ "BULL",
                        Rprime < 0 ~ "BEAR",
                        TRUE ~ "NOTBB"))


Vbb<-market_data %>% group_by(BB) %>% summarize(Vm = mean(V), Vs = sum(V))

Vplus<-Vbb$Vs[2]
Vminus<-Vbb$Vs[1]
#system of equations
ab_ratio<-Vplus/Vminus
beta<-2/(ab_ratio +1)
alpha<-2-beta
#try to generate agents as matrix eventually with randomization may need to update with Rprime and as we go
#probability of Pbuy = Psell
p<-.0154

market_data<-market_data %>% mutate(Ptrade = case_when(Rprime > 0 ~ 2*p*alpha,
                                                       Rprime < 0 ~ 2*p*beta,
                                                       TRUE ~ 2*p))

Ptrade<-market_data$Ptrade[-c(251)]
market_data<-market_data %>% select(-Ptrade)
Ptrade<-as.data.frame(Ptrade) %>% rbind(NA,.)
#put Ptrade back into market data to adjust/ shift to Pt+1 
market_data<-data.frame(market_data,Ptrade)

#ToDo
#following similar methods DETERMINE delta r 
#create graphs
#generate randomized agent matrix, and produce time series of returns python

#graph of Rprime
gg1 <- market_data %>% ggplot(aes(ts, color = Rprime >=0)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Weighted Return Rprime vs t") + labs(x="t (trade day step)", y="Rprime") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=Rprime)) + scale_color_manual(values = c("red","green"), labels = c("Bear","Bull")) + geom_line(aes(y=Rprime), color = "black", linewidth = .3) 
gg1

coord_cartesian(xlim=c(.5,1))
