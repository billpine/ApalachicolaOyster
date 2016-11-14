#Full Correlations for all years, these are correlations presented in tables 
#Set working directory to your data folder
setwd("Insert name here")

WATERDATA.ALL.YEARS.1958.2013 <- read.delim("WATERDATA ALL YEARS 1958-2013.txt", comment.char="#")
water.allyrs<-WATERDATA.ALL.YEARS.1958.2013
colnames(water.allyrs)<-c("year","mmean","mmax","mmin","X1.day","X7.day","X30.day","X60.day","X90.day","X120.day","max60.day","lt_9000","lt_12000","lt_16000", "gt_16000","gt_18000","gt_20000","gt_30000","gt_50000","Winter","Summer","Fall" )


TotalApalachicoladata <- read.delim("TotalApalachicoladata.txt")
Newdata<-TotalApalachicoladata
colnames(Newdata)<-c("year","landings","adj_landings","reg_oysterman","CPUE")

library("Hmisc") #for cor matrices

cor.mat<-cbind(log10(water.allyrs$mmin),log10(water.allyrs$mmean),log10(water.allyrs$mmax),log10(water.allyrs$X1.day),log10(water.allyrs$X7.day),log10(water.allyrs$X30.day),log10(water.allyrs$X60.day),log10(water.allyrs$X90.day),log10(water.allyrs$X120.day),log10(water.allyrs$max60.day),water.allyrs$lt_9000,water.allyrs$lt_12000,water.allyrs$lt_16000, water.allyrs$gt_16000, water.allyrs$gt_18000, water.allyrs$gt_20000, water.allyrs$gt_30000, water.allyrs$gt_50000, log10(water.allyrs$Winter), log10(water.allyrs$Summer), log10(water.allyrs$Fall))
colnames(cor.mat)<-c("mmin","mmean","mmax","X1.day","X7.day","X30.day","X60.day","X90.day","X120.day","max60.day","lt_9000","lt_12000","lt_16000", "gt_16000","gt_18000","gt_20000","gt_30000","gt_50000","Winter","Summer","Fall" )
head(cor.mat)

#FLOW CORRELATIONS USING ALL DATA
#Output matrix contains correlations, sample size, and p-value. 1st row or 1st column are values of interest
Newdata$CPUE[29]=NA  #Omitting Year 1986
Total.cor.0<-rcorr(Newdata$CPUE,cor.mat,type="pearson")
Total.cor.1<-rcorr(Newdata$CPUE[2:56],cor.mat[1:55,],type="pearson")
Total.cor.2<-rcorr(Newdata$CPUE[3:56],cor.mat[1:54,],type="pearson")
Total.cor.3<-rcorr(Newdata$CPUE[4:56],cor.mat[1:53,],type="pearson")

#CORRELATIONS FROM 1958-1984
Wilber.cor.0<-rcorr(Newdata$CPUE[1:27],cor.mat[1:27,],type="pearson")
Wilber.cor.1<-rcorr(Newdata$CPUE[2:27],cor.mat[1:26,],type="pearson")
Wilber.cor.2<-rcorr(Newdata$CPUE[3:27],cor.mat[1:25,],type="pearson")
Wilber.cor.3<-rcorr(Newdata$CPUE[4:27],cor.mat[1:24,],type="pearson")

#CORRELATIONS FROM 1987-2013
Recent.cor.0<-rcorr(Newdata$CPUE[29:56],cor.mat[29:56,],type="pearson")
Recent.cor.1<-rcorr(Newdata$CPUE[29:56],cor.mat[28:55,],type="pearson")
Recent.cor.2<-rcorr(Newdata$CPUE[30:56],cor.mat[28:54,],type="pearson")
Recent.cor.3<-rcorr(Newdata$CPUE[31:56],cor.mat[28:53,],type="pearson")

#Normalized CPUE ALL Years
Normalized_CPUE <- read.delim("Normalized_CPUE.txt")

Norm.cor.0<-rcorr(Normalized_CPUE$Norm.CPUE,cor.mat[1:56,],type="pearson")
Norm.cor.1<-rcorr(Normalized_CPUE$Norm.CPUE[2:56],cor.mat[1:55,],type="pearson")
Norm.cor.2<-rcorr(Normalized_CPUE$Norm.CPUE[3:56],cor.mat[1:54,],type="pearson")
Norm.cor.3<-rcorr(Normalized_CPUE$Norm.CPUE[4:56],cor.mat[1:53,],type="pearson")

#Checking for a relationship between standard deviation of flow and CPUE, does not seem to be one
#Must run portion of data extraction code(Extract_all_waterdata_code) that obtains sumstats,lf.flow.yr.stats, at the beginning 
#All yrs, need zoo pckg
sal.cor.0<-rcorr(Newdata$CPUE[3:56],coredata(lf.flow.yr.stats$Sd[4:57]),type="pearson")
sal.cor.1<-rcorr(Newdata$CPUE[3:56],coredata(lf.flow.yr.stats$Sd[3:56]),type="pearson")
sal.cor.2<-rcorr(Newdata$CPUE[3:56],coredata(lf.flow.yr.stats$Sd[2:55]),type="pearson")
sal.cor.3<-rcorr(Newdata$CPUE[4:56],coredata(lf.flow.yr.stats$Sd[2:54]),type="pearson")
#First epoch
s.cor.0<-rcorr(Newdata$CPUE[3:27],coredata(lf.flow.yr.stats$Sd[4:28]),type="pearson")
s.cor.1<-rcorr(Newdata$CPUE[3:27],coredata(lf.flow.yr.stats$Sd[3:27]),type="pearson")
s.cor.2<-rcorr(Newdata$CPUE[3:27],coredata(lf.flow.yr.stats$Sd[2:26]),type="pearson")
s.cor.3<-rcorr(Newdata$CPUE[4:27],coredata(lf.flow.yr.stats$Sd[2:25]),type="pearson")
#Second epoch
scor0<-rcorr(Newdata$CPUE[30:56],coredata(lf.flow.yr.stats$Sd[31:57]),type="pearson")
scor1<-rcorr(Newdata$CPUE[30:56],coredata(lf.flow.yr.stats$Sd[30:56]),type="pearson")
scor2<-rcorr(Newdata$CPUE[30:56],coredata(lf.flow.yr.stats$Sd[29:55]),type="pearson")
scor3<-rcorr(Newdata$CPUE[31:56],coredata(lf.flow.yr.stats$Sd[29:54]),type="pearson")

#MODEL 1.1
#Predicting Cpue from 1960-1981 using the minimum flow each year (logged) with a 2 year time lag from 1958-1979
#Main Wilber Model, bottom page 185
#Kg conversion lbs*0.453592=kg
lm.lag.X1<-lm(Newdata$CPUE[3:24]*0.453592~cor.mat[1:22,4],na.action="na.omit")    
#model Wilber used to predict 82-84
summary(lm.lag.X1)
########################################
#Predicting CPUE for Years 1982-1984
########################################

#Using parameters from model 1.1
Predicted_CPUE<-coef(lm.lag.X1)[1]+(cor.mat[1:25,4]*coef(lm.lag.X1)[2])
Predicted_CPUE
plot(Newdata$year[1:27],Newdata$CPUE[1:27]*0.453592,type="l")
lines(Newdata$year[3:27],Predicted_CPUE,col="red")

#Getting residuals and percent error for 1982-1984 CPUE
Resid<-(Newdata$CPUE[3:27]*0.453592)-Predicted_CPUE
Residabs<-abs(Resid)

PercentErr<-Residabs/(Newdata$CPUE[3:27]*0.453592)             
tail(PercentErr)
#Percent errors of 37% in 1982, 7.6% in 1983, and 6.5 % in 1984, 
#very close to Wilber values, page 185 of Wilber

#MODEL 2.1
#Model predicting CPUE from 1960-1984 using the number of days flow was less than 12,000 cfs
#with a 2 year time lag from 1958-1982
lm.lt_12000<-lm(Newdata$CPUE[3:27]*0.453592~cor.mat[1:25,12],na.action="na.exclude") 
summary(lm.lt_12000)

Pred.CPUE.Nickmodel1<-coef(lm.lt_12000)[1]+(cor.mat[1:25,12]*coef(lm.lt_12000)[2])
lines(Newdata$year[3:27],Pred.CPUE.Nickmodel1, col="blue")

#Predicting CPUE from 1987-2013 using model 1.1
Pred.CPUE.Wilbermodel<-coef(lm.lag.X1)[1]+(cor.mat[28:54,4]*coef(lm.lag.X1)[2])
Pred.CPUE.Wilbermodel

plot(Newdata$year,Newdata$CPUE*0.453592,type="l",ylim=c(0,7000),ylab="CPUE",xlab="Year",main="CPUE in Apalachicola Bay")    
lines(Newdata$year[30:56],Pred.CPUE.Wilbermodel,col="red")
legend("topleft",c("Predicted CPUE","Observed CPUE"),lty=c(1,1),lwd=c(1,1),col=c("red","black"))

#Characterizing error for Model 1.1 when used to predict CPUE from 1987-2013
Resid_Wilber_model<-(Newdata$CPUE[30:56]*0.453592)-Pred.CPUE.Wilbermodel
PercentErr_Wilber_model<-abs(Resid_Wilber_model)/(Newdata$CPUE[30:56]*0.453592) 
plot(PercentErr_Wilber_model,xlab="Year",ylab= "Percent Error")
AvgPE<-mean(PercentErr_Wilber_model,na.rm=T)                               
AvgPE
#Wilber has average error of 119% 

#Model 2.1, using number of days flow was less than 12000 cfs
Pred.CPUE.Nickmodel<-coef(lm.lt_12000)[1]+(cor.mat[28:54,12]*coef(lm.lt_12000)[2])

#Characterizing error of Model 2.1 in predicting CPUE from 1987-2013
Resid_Nick_model<-(Newdata$CPUE[30:56]*0.453592)-Pred.CPUE.Nickmodel
PercentErr_Nick_model<-abs(Resid_Nick_model)/(Newdata$CPUE[30:56]*0.453592) 
AvgPENick<-mean(PercentErr_Nick_model,na.rm=T)   
AvgPENick
#Nicks model (2.1) has average error of 136%

#MODEL 2.2, Predicted CPUE from 1987-2013 using the number of days flow was less than 12,000 cfs with a 2 year
#time lag from 1985-2011
lm.lt_12000.recent.revised.w86<-lm(Newdata$CPUE[30:56]*0.453592~cor.mat[28:54,12])
summary(lm.lt_12000.recent.revised.w86)

#MODEL 1.2, Predicted CPUE from 1987-2013 using the log of the minimum flow each year from 1985-2011
lm.X1.day.recent.revised.w86<-lm(Newdata$CPUE[30:56]*0.453592~cor.mat[28:54,4]) 
summary(lm.X1.day.recent.revised.w86)

##################
#####Simple AIC###
##################
lm.Nick.AIC2<-extractAIC(lm.lt_12000.recent.revised.w86)
lm.Wilber.AIC2<-extractAIC(lm.X1.day.recent.revised.w86)

AIC.table3<-matrix(c(lm.Nick.AIC2[2],lm.Wilber.AIC2[2]),ncol=2)
colnames(AIC.table3) <- c("Nick","Wilber")
rownames(AIC.table3) <- c("AIC")
AIC.table3 <- as.table(AIC.table3)
AIC.table3

min(AIC.table3)

#Pred CPUE's for both model fitted to recent epoch data
Pred_CPUE_X1_recent<-coef(lm.X1.day.recent.revised.w86)[1]+(coef(lm.X1.day.recent.revised.w86)[2]*cor.mat[28:54,4])
Pred_CPUE_recent<-coef(lm.lt_12000.recent.revised.w86)[1]+(coef(lm.lt_12000.recent.revised.w86)[2]*cor.mat[28:54,12])

#Linear Regressions Full dataset
#MODEL 2.3, using the number of days flow was less than 12,000 cfs with a 2 year time lag to predict CPUE from 1960-2013
lm.Nick<-lm(Newdata$CPUE[3:56]*0.453592~cor.mat[1:54,12])
summary(lm.Nick)
Pred.lm.Nick<-coef(lm.Nick)[1]+(coef(lm.Nick)[2]*cor.mat[1:54,12])

#MODEL 1.3, using the log of the minimum annual flow with a 2 year time lag to predict CPUE from 1960-2013
lm.Wilber<-lm(Newdata$CPUE[3:56]*0.453592~cor.mat[1:54,4])
summary(lm.Wilber)
Pred.lm.Wilber<-coef(lm.Wilber)[1]+(cor.mat[1:54,4]*coef(lm.Wilber)[2])

##################
#####Simple AIC###
##################
lm.Nick.AIC<-extractAIC(lm.Nick)
lm.Wilber.AIC<-extractAIC(lm.Wilber)

AIC.table2<-matrix(c(lm.Nick.AIC[2],lm.Wilber.AIC[2]),ncol=2)
colnames(AIC.table2) <- c("Nick","Wilber")
rownames(AIC.table2) <- c("AIC")
AIC.table2 <- as.table(AIC.table2)
AIC.table2

min(AIC.table2)

#CPUE PLOT, Figure 1, Lower panel
CatchEffort <- read.delim("ABtrip_vs_fishers_CPUE.txt")
Trips <- read.delim("Trips.txt")

CPUE.trips<-Newdata$adj_landings[30:56]/Trips$Trips[2:28] #CPUE by trip
S<-rep(NA,29)  #To fill in NA's for first epoch

CPUE.t<-c(S,CPUE.trips)  #Full time period, (1960-1986)-NA 
CPUE.o<-Newdata$CPUE
CPUE.o[29]=NA #omitting year 86 

#Spearman rank CORRELATIONS FROM 1987-2013
Spear.Recent.cor.0<-rcorr(Newdata$CPUE[29:56],cor.mat[29:56,],type="spearman")
Spear.Recent.cor.1<-rcorr(Newdata$CPUE[29:56],cor.mat[28:55,],type="spearman")
Spear.Recent.cor.2<-rcorr(Newdata$CPUE[30:56],cor.mat[28:54,],type="spearman")
Spear.Recent.cor.3<-rcorr(Newdata$CPUE[31:56],cor.mat[28:53,],type="spearman")

#Spearman rank FLOW CORRELATIONS USING ALL DATA
spear.Total.cor.0<-rcorr(Newdata$CPUE[1:56],cor.mat,type="pearson")
spear.Total.cor.1<-rcorr(Newdata$CPUE[2:56],cor.mat[1:55,],type="spearman")
spear.Total.cor.2<-rcorr(Newdata$CPUE[3:56],cor.mat[1:54,],type="spearman")
spear.Total.cor.3<-rcorr(Newdata$CPUE[4:56],cor.mat[1:53,],type="spearman")

#Spearman CORRELATIONS FROM 1958-1984
spear.Wilber.cor.0<-rcorr(Newdata$CPUE[1:27],cor.mat[1:27,],type="spearman")
spear.Wilber.cor.1<-rcorr(Newdata$CPUE[2:27],cor.mat[1:26,],type="spearman")
spear.Wilber.cor.2<-rcorr(Newdata$CPUE[3:27],cor.mat[1:25,],type="spearman")
spear.Wilber.cor.3<-rcorr(Newdata$CPUE[4:27],cor.mat[1:24,],type="spearman")

####################################################
#Salinity Relationships
####################################################

#Salinity data was obtained from the National Estuarine Research Reserve System Centralized Data Management Office

#Reading in Salinity
Salinity.Compiled <- read.delim("Salinity Compiled.txt")
#Column names
colnames(Salinity.Compiled)<-c("Station_Code", "Date", "PSU", "Flag_Code")

aggregate(Salinity.Compiled$PSU, by=Salinity.Compiled["Flag_Code"], FUN=.sumstats)

#Detaching time from date variable
Salinity.Compiled$Date <- sapply(strsplit(as.character(Salinity.Compiled$Date), " "), "[", 1)
#Converting to R date format
Salinity.Compiled$Date<-as.Date(Salinity.Compiled$Date, "%m/%d/%Y")

#Creating summary stats function to obtain stats by day, month, etc 
stats = function(x) c(Mean=mean(x,na.rm=T),Sd=sd(x,na.rm=T), Var=var(x,na.rm=T), Nobs=length(na.omit(x)))
#Converting to zoo objects
sal.valdb <- zoo(Salinity.Compiled[Salinity.Compiled$Station_Code=="Dry Bar", 3],Salinity.Compiled[Salinity.Compiled$Station_Code=="Dry Bar", 2] )
sal.valcp <- zoo(Salinity.Compiled[Salinity.Compiled$Station_Code=="Cat Point", 3],Salinity.Compiled[Salinity.Compiled$Station_Code=="Cat Point", 2] )

#Converting into Daily salinity means
Salinity.stats.DB = aggregate(sal.valdb,by=format(index(sal.valdb),"%Y-%m-%d"),FUN=stats)
Salinity.stats.CP = aggregate(sal.valcp,by=format(index(sal.valcp),"%Y-%m-%d"),FUN=stats)

#Converting from daily means to monthly means
Salinity.monthly.db<-aggregate(Salinity.stats.DB$Mean, by=as.yearmon(index(Salinity.stats.DB$Mean)), FUN=stats)
Salinity.monthly.cp<-aggregate(Salinity.stats.CP$Mean, by=as.yearmon(index(Salinity.stats.CP$Mean)), FUN=stats)

#Looking at relationship between salinity of two bars 
cor.test(Salinity.monthly.cp$Mean, Salinity.monthly.db$Mean)

#Getting flow data from lf.flow.z in data extraction code
flow.stats  = window(lf.flow.z,start='2002-01-01',end='2013-12-31')  #subset of data from 2002 - 2013
#Turning this flow data into monthly means
monthlyflow.stats = aggregate(flow.stats,by=as.yearmon(index(flow.stats)),FUN=stats)

#Relationships between each bar and flow 
cor.test(Salinity.monthly.db$Mean, monthlyflow.stats$Mean)
cor.test(Salinity.monthly.cp$Mean, monthlyflow.stats$Mean)

#Correlation between CPUE calculated using two different effort metrics from 1987-2013, r=0.88
cor.test((CPUE.o[3:56]*0.453592)[28:54],(CPUE.t[3:56]*0.453592)[28:54])
