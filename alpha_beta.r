
#FUNDAMENTAL PARAMETERS 
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/Index_Data.csv") #mark_data gets updated as we go along
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/2008_SPX_Recession_Prices.csv")


#mark_data gets updated as we go along
e<--1.12 #eta weight of investment horizon, i (more weight to recent horizons) looks at net Return over past days
k<-1 #proportional coefficient affecting Rprime, let it be 1 for now but maybe it must be a function of e and M that makes Rp_mx = N?
M<-150 #enter a Max Investment horizon greater than 0
p<-.0154 #probability of Pbuy = Psell
N<-100000 #Initialize number of agents for now, see if it can be determined from index later

#parameter check function ranges and possibly user interface
if(M<=0){
  print("invalid horizon")
}

#CALCULATIONS & DERIVED PARAMETERS 
mark_data<-data %>%
slice(-1:-5) %>% setNames(unname(.[1,])) %>% slice(-1) %>% 
rename(SP = PX_LAST) %>% mutate(SP = as.numeric(SP)) %>% 
rename(TV = PX_VOLUME) %>% mutate(TV = as.numeric(TV)) %>%
rename(DT = Date) %>% mutate(DT = as_date(DT, format = "%m/%d/%y")) %>% mutate(DN = as.numeric(DT)) %>%
arrange(DN) %>% mutate(ts = (row_number() - 1)) %>% 
mutate(ln_SP = log(SP)) %>% mutate(Rt = log(SP/lag(SP))) %>% mutate(Rt_mn = mean(Rt, na.rm=T)) %>% mutate(Rt_sq = Rt*Rt) %>% 
mutate(Rt_sq_mn = mean(Rt_sq, na.rm=T)) %>% mutate(sd = sqrt(Rt_sq_mn - Rt_mn*Rt_mn)) %>% mutate(rt = (Rt - Rt_mn)/sd) %>% 
mutate(rt_av = abs(rt)) %>% mutate(Rt_av = abs(Rt)) %>% mutate(Vr_av = TV*rt_av) %>% mutate(VR_av = TV*Rt_av) 

n<-length(mark_data$ts) #number of rows of mark_data
ts<-mark_data$ts[-c(1)] #extract timestep vector and slice off first row
hor<-horizons(ts,M) #initialize horizons, see horizons function for more details
gam <- map(hor, gamma) #take each horizon i in each vector and raise it to eta, see gamma function for more details


#test make sure we have all investment horizons
hor[[200]]
hor[[100]]
gam[[3]]

#CALCULATION OF RPRIME (Rp)
Rt<-mark_data$Rt[-c(1)] #extract Rt from mark data and slice off first row
Rt_av<-mark_data$Rt_av[-c(1)]
Rp<-wareturn(ts,Rt,hor,gam,k) #calculate weighted average return Rp
Rp_mx<-wareturn(ts,Rt_av,hor,gam,k) #test Rp max to determine N number of agents (abs forces all agents to buy or sell)
Rp<-as.data.frame(Rp) %>% rbind(NA,.) #convert to data frame and add the first row as NA to prepare to add back to mark_data
Rp_mx<-as.data.frame(Rp_mx) %>% rbind(NA,.)
  
#add Rp and Rp_mx back to mark_data and create bull/bear flag
mark_data<-data.frame(mark_data,Rp, Rp_mx) %>% 
  mutate(BB = case_when(Rp > 0 ~ "BULL",
                        Rp < 0 ~ "BEAR",
                        TRUE ~ "NOTBB")) %>%
  mutate(bb = case_when(rt > 0 ~ "bull",
                        rt < 0 ~ "bear",
                        TRUE ~ "notbb")) %>%
  mutate(Rp_av = abs(Rp)) %>% mutate(VRp = TV*Rp_av)



#CALCULATE VOLUME Ratio and alpha and beta
TVBB<-mark_data %>% group_by(BB) %>% summarize(TV = sum(TV))
V_p<-TVBB$TV[2]
V_m<-TVBB$TV[1]
#system of equations for alpha and beta
a_b<-V_p/V_m
b<-2/(a_b +1)
a<-2-b

#CALCULATE Ptrade (Pt) to mark_data 
mark_data<-mark_data %>% mutate(Pt = case_when(Rp > 0 ~ 2*p*a,
                                                       Rp < 0 ~ 2*p*b,
                                                       TRUE ~ 2*p))

#extract Pt vector to shift it down one row to resemble t + 1
Pt<-mark_data$Pt[-c(n)] #remove last row since its not part of the data
mark_data<-mark_data %>% select(-Pt) #delete old Pt from market data
Pt<-as.data.frame(Pt) %>% rbind(NA,.) #add NA for first row
#put Pt back into mark data to adjust/ shift to Pt+1 
mark_data<-data.frame(mark_data,Pt)

#AYSMMETRIC HERDING CALUCLATIONS
mark_data<-mark_data %>% 
          mutate(nA_mn = abs(Rp))
nA_mn<-mark_data$nA_mn[-c(n)]
mark_data<-mark_data %>% select(-nA_mn)
nA_mn<-as.data.frame(nA_mn) %>% rbind(NA,.)
mark_data<-data.frame(mark_data,nA_mn)

#Step 1 calculate dbull and dbear #why delta little r doesn't depend on rprime to determine bull/bear like Big R does?
hdbb<-mark_data %>% group_by(bb) %>% summarize(hd = sum(Vr_av)/sum(TV))
d_p<-hdbb$hd[2]
d_m<-hdbb$hd[1]
d_r<-(d_m - d_p)/2

mark_data<-mark_data %>% mutate(rp = rt + d_r)

#test with R
HDBB<-mark_data %>% group_by(BB) %>% summarize(HD = sum(VR_av)/sum(TV))
d_P<-HDBB$HD[2]
d_M<-HDBB$HD[1]
d_R<-(d_M - d_P)/2

#test with Rp
HDBB<-mark_data %>% group_by(BB) %>% summarize(HD = sum(VRp)/sum(TV))
d_P<-HDBB$HD[2]
d_M<-HDBB$HD[1]
d_R<-(d_M - d_P)/2

D_R<-d_r/d_R


mdata<-mark_data %>% filter(ts>=750 & ts <=1100)
#graph of Rp
gg1 <- mark_data %>% ggplot(aes(ts, color = Rp >=0)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("2008 SP500 Weighted Return Rprime vs t") + labs(x="t (trade day step)", y="Rprime") +
  geom_point(aes(y=Rp)) + scale_color_manual(values = c("red","green"), labels = c("Bear","Bull")) + geom_line(aes(y=Rp), color = "black", linewidth = .3)
                gg1 + coord_cartesian(xlim = c(700,1150))

gg2 <- mark_data %>% ggplot(aes(ts)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Ptrade vs t") + labs(x="t (trade day step)", y="Rprime") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=Pt)) + geom_line(aes(y=Pt), color = "black", linewidth = .3) 
gg2

mdata <-mark_data %>% na.omit()
mpiv<-pivot_longer(mdata, -1:25, names_to = "Measure", values_to = "val")
