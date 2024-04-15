
#FUNDAMENTAL PARAMETERS 
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/Index_Data.csv") #mark_data gets updated as we go along
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/2008_SPX_Recession_Prices.csv")

#ALL TIME DATA
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_SPX.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_Shanghai.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_FTSE.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_Nikkei.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_Hangseng.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_DAX.csv")
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_VIX.csv")

#SPX
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_SPX.csv")

#Shanghai
data<-read.csv("/Users/Personal/Documents/GitHub/485FinancialProject/data/ALLTIME_Shanghai.csv")


#mark_data gets updated as we go along
e<--1.12 #eta weight of investment horizon, i (more weight to recent horizons) looks at net Return over past days
k<-1 #proportional coefficient affecting Rprime, let it be 1 for now but maybe it must be a function of e and M that makes Rp_max = N? (1/∑∑γ) 
M<-150 #enter a Max Investment horizon greater than 0
p<-.0154 #probability of Pbuy = Psell
N<-10000 #Initialize number of agents for now, see if it can be determined from index later

#parameter check function ranges and possibly user interface
if(M<=0){
  print("invalid horizon")
}

#CALCULATIONS & DERIVED PARAMETERS 
mark_data<-data %>%
  slice(-1:-5) %>% setNames(unname(.[1,])) %>% slice(-1) %>% 
  #filter(apply(.,1,function(row) any(grepl("N/A", row)))) %>%
  rename(SP = PX_LAST) %>% mutate(SP = as.numeric(SP)) %>% 
  rename(TV = PX_VOLUME) %>% mutate(TV = as.numeric(TV)) %>%
  rename(DT = Date) %>% mutate(DT = as_date(DT)) %>% mutate(DN = as.numeric(DT)) %>%
  arrange(DN) %>% mutate(ts = (row_number() - 1)) %>% 
  mutate(ln_SP = log(SP)) %>% mutate(Rt = log(SP/lag(SP))) %>% mutate(Rt_avg = mean(Rt, na.rm=T)) %>% mutate(Rt_sq = Rt*Rt) %>% 
  mutate(Rt_sq_avg = mean(Rt_sq, na.rm=T)) %>% mutate(sd = sqrt(Rt_sq_avg - Rt_avg*Rt_avg)) %>% mutate(rt = (Rt - Rt_avg)/sd) %>% 
  mutate(rt_abs = abs(rt)) %>% mutate(Rt_abs = abs(Rt)) %>% mutate(rt_absTV = TV*rt_abs) %>% mutate(Rt_absTV = TV*Rt_abs) 


n<-length(mark_data$ts) #number of rows of mark_data
ts<-mark_data$ts[-c(1)] #extract timestep vector and slice off first row
hor<-horizons(ts,M) #initialize horizons, see horizons function for more details
gam <- map(hor, gamma) #take each horizon i in each vector and raise it to eta, see gamma function for more details

#CALCULATION OF RPRIME (Rp)
Rt<-mark_data$Rt[-c(1)] #extract Rt from mark data and slice off first row
Rt_abs<-mark_data$Rt_abs[-c(1)]
Rp<-wareturn(ts,Rt,hor,gam,k) #calculate weighted average return Rp
Rp_max<-wareturn(ts,Rt_abs,hor,gam,k) #test Rp max to determine N number of agents (abs forces all agents to buy or sell)
Rp<-as.data.frame(Rp) %>% rbind(NA,.) #convert to data frame and add the first row as NA to prepare to add back to mark_data
Rp_max<-as.data.frame(Rp_max) %>% rbind(NA,.)
  
#add Rp and Rp_max back to mark_data and create bull/bear flag, Rp_max might not be needed
mark_data<-data.frame(mark_data,Rp, Rp_max) %>% 
  mutate(BB = case_when(Rp > 0 ~ "BULL",
                        Rp < 0 ~ "BEAR",
                        TRUE ~ "NOTBB")) %>%
  mutate(bb = case_when(rt > 0 ~ "bull",
                        rt < 0 ~ "bear",
                        TRUE ~ "notbb")) %>%
  mutate(Rp_abs = abs(Rp)) %>% mutate(Rp_absTV = TV*Rp_abs)

#graph of Rp
gg1 <- mark_data %>% ggplot(aes(ts, color = Rp >=0)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Weighted Return Rprime vs t") + labs(x="t (trade day step)", y="Rprime") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=Rp)) + scale_color_manual(values = c("red","green"), labels = c("Bear","Bull")) + geom_line(aes(y=Rp), color = "black", linewidth = .3) 
gg1

--------------------------------------------------------------------------------
#AYSMMETRIC TRADING CALCULATIONS
#CALCULATE VOLUME Ratio and alpha and beta
TVBB<-mark_data %>% group_by(BB) %>% summarize(TV_sum = sum(TV))
V_p<-TVBB$TV_sum[2]
V_m<-TVBB$TV_sum[1]
#system of equations for alpha and beta
a_b<-V_p/V_m
b<-2/(a_b +1)
a<-2-b

#Ptrade
mark_data<-mark_data %>% mutate(Pt = case_when(Rp > 0 ~ 2*p*a,
                                                Rp < 0 ~ 2*p*b,
                                                 TRUE ~ 2*p))

#extract Pt vector to shift it down one row to resemble t + 1
Pt<-mark_data$Pt[-c(n)] #remove last row since its not part of the data
mark_data<-mark_data %>% select(-Pt) #delete old Pt from market data
Pt<-as.data.frame(Pt) %>% rbind(NA,.) #add NA for first row
#put Pt back into mark data to adjust/ shift to Pt+1 
mark_data<-data.frame(mark_data,Pt)



--------------------------------------------------------------------------------
#AYSMMETRIC HERDING CALUCLATIONS
#CALCULATE little delta R and Big Delta R and finally the slope ie the DELTA R WE WANT
mark_data<-mark_data %>% 
          mutate(nA_avg = abs(Rp))
nA_avg<-mark_data$nA_avg[-c(n)]
mark_data<-mark_data %>% select(-nA_avg)
nA_avg<-as.data.frame(nA_avg) %>% rbind(NA,.)
mark_data<-data.frame(mark_data,nA_avg)

#Step 1 calculate dbull and dbear #why delta little r doesn't depend on rprime to determine bull/bear like Big R does?
hdbb<-mark_data %>% group_by(bb) %>% summarize(hd = sum(rt_absTV)/sum(TV))
d_p<-hdbb$hd[2]
d_m<-hdbb$hd[1]
d_r<-(d_m - d_p)/2

mark_data<-mark_data %>% mutate(rp = rt + d_r)

#test with R
HDBB<-mark_data %>% group_by(BB) %>% summarize(HD = sum(Rt_absTV)/sum(TV))
d_P<-HDBB$HD[2]
d_M<-HDBB$HD[1]
d_R<-(d_M - d_P)/2

#test with Rp
HDBB<-mark_data %>% group_by(BB) %>% summarize(HD = sum(Rp_absTV)/sum(TV))
d_P<-HDBB$HD[2]
d_M<-HDBB$HD[1]
d_R<-(d_M - d_P)/2

#found evidence that it is rounded up for S&P and other indices and down for Shanghai
#find the slope also called Delta R
D_R<-d_r/d_R

#round up to ceiling for all indexes
if(D_R>0){
D_R<-ceiling(D_R)
}

if (D_R<0) {
D_R<-(-1)*ceiling(abs(D_R))}

D_R

---------------------------------------------------------------------
  #SIMULATION N = 10000
---------------------------------------------------------------------
#SPX 2008 Data 

#Return volatility correlation Function Lt (tau is cumulative)
ts<-mark_data$ts[-c(1)]
ret<-mark_data$rt[-c(1)]
vol<-mark_data$Rt_abs[-c(1)]
tp<-1 #t prime its a delta t or step size
tw<-60 #time window
ws<-950 #window start time

Lt<-leverage(ts,ret,vol,tp,tw,ws)

gg2 <- Lt %>% ggplot(aes(tau, color = rvc >=0)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Mid Oct - Dec 2008 Return Volatility Correlation Lt vs tau") + labs(x="tau (cumulative time)", y="Lt (return-vol-corr)") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=rvc)) + scale_color_manual(values = c("orange","purple"), labels = c("anti-leverage","leverage")) + geom_line(aes(y=rvc), color = "black", linewidth = .3) +
scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.10), limits = c(-0.5,0.5)) 
gg2

mark_data %>% filter(ts == 4430) #figure out date from ts
#adjust scale of graph accordingly
-----------------------------------------------------
#S&P All-time 2008
 #Long Term Lt (tau is cumulative)
ts<-mark_data$ts[-c(1)]
ret<-mark_data$rt[-c(1)]
vol<-mark_data$Rt_abs[-c(1)]
tp<-floor(180/60) #t prime its a delta t or step size
tw<-30 #time window
ws<-20320 #window start time

Lt<-leverage(ts,ret,vol,tp,tw,ws)
Lt1<-Lt
gg <- Lt1 %>% ggplot(aes(tau)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("2008 Return Volatility Correlation Lt vs tau") + labs(x="tau (cumulative time)", y="Lt (return-vol-corr)") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=rvc), color = "red") + geom_line(aes(y=rvc), color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.10), limits = c(-1,1)) 
gg

mark_data %>% filter(ts == 20320) #figure out date from ts
#adjust scale of graph accordingly

------------------------------------------------------
#ALL TIME All timeLong Term Lt (tau is cumulative)
ts<-mark_data$ts[-c(1)]
ret<-mark_data$rt[-c(1)]
vol<-mark_data$Rt_abs[-c(1)]
tp<-floor(24000/60) #t prime its a delta t or step size
tw<-24000 #time window
ws<-1 #window start time

Lt<-leverage(ts,ret,vol,tp,tw,ws)

gg <- Lt %>% ggplot(aes(tau)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Oct-Dec 2008 Return Volatility Correlation Lt vs tau") + labs(x="tau (cumulative time)", y="Lt (return-vol-corr)") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=rvc), color = "red") + geom_line(aes(y=rvc), color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.10), limits = c(-0.5,0.5)) 
gg

mark_data %>% filter(ts == 20120) #figure out date from ts
#adjust scale of graph accordingly
--------------------------------------------------------------------

#SHANGHAI
  #S&P All-time 2008
  #Long Term Lt (tau is cumulative)
  ts<-mark_data$ts[-c(1)]
ret<-mark_data$rt[-c(1)]
vol<-mark_data$Rt_abs[-c(1)]
tp<-floor(8000/60) #t prime its a delta t or step size
tw<-8000 #time window
ws<-1 #window start time

Lt<-leverage(ts,ret,vol,tp,tw,ws)

gg <- Lt %>% ggplot(aes(tau)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("1990 to Present Shaghai Lt vs tau") + labs(x="tau (cumulative time)", y="Lt (return-vol-corr)") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=rvc), color = "red") + geom_line(aes(y=rvc), color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-0.8, 0.8, by = 0.10), limits = c(-0.8,0.8)) 
gg

---------------------------------------------------------------------
 ts<-mark_data$ts[-c(1)]
ret<-mark_data$rt[-c(1)]
vol<-mark_data$Rt_abs[-c(1)]
tp<-floor(260/60) #t prime its a delta t or step size
tw<-260 #time window
ws<-4200 #window start time

Lt<-leverage(ts,ret,vol,tp,tw,ws)

gg <- Lt %>% ggplot(aes(tau)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("2008 to Shaghai Anti-Leverage Lt vs tau") + labs(x="tau (cumulative time)", y="Lt (return-vol-corr)") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=rvc), color = "red") + geom_line(aes(y=rvc), color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.10), limits = c(-1,1)) 
gg


n

mark_data %>% filter(ts == 4200) #figure out date from ts
#adjust scale of graph accordingly





#Simulation
#graph of Ptrade
gg <- mark_data %>% ggplot(aes(ts)) + 
  theme_minimal() + geom_hline(yintercept=0) + geom_vline(xintercept = 0) + 
  ggtitle("Ptrade vs t") + labs(x="t (trade day step)", y="Rprime") +
  #geom_point(aes(y=V), color = "blue") + geom_line(aes(y=V), color = "blue", linewidth= .3) +
  geom_point(aes(y=Pt)) + geom_line(aes(y=Pt), color = "black", linewidth = .3) 
gg

