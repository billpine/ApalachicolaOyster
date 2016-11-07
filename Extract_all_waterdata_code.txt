#Extracting all discharge variables for All years 

#setup  
library(waterData)  #Must have waterData installed
library(hydroTSM)  #Must have hydroTSM installed
#Set working directory to your data folder
setwd("Insert name here")

#function to calculate summary statistics
.sumstats = function(x) c(Sum=sum(x,na.rm=T),Mean=mean(x,na.rm=T),Max=max(x,na.rm=T),Min=min(x,na.rm=T), Sd=sd(x,na.rm=T), 
                          Var=var(x,na.rm=T), Nobs=length(na.omit(x)),
                          CV    = sd(x,na.rm=T)/mean(x,na.rm=T),
                          se    = sd(x,na.rm=T)/sqrt(length(na.omit(x))),
                          L95se = mean(x,na.rm=T)-1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))),
                          U95se = mean(x,na.rm=T)+1.96*(sd(x,na.rm=T)/sqrt(length(na.omit(x)))))


################################################################################
#
#         Blountstown gauge 02358700
#
################################################################################

stnum = '02358700'
Title = 'Apalachicola River at Blountstown'

#get flow from USGS
lf.flow = importDVs(staid=stnum,code='00060',stat='00003',edate='2014-12-31')

#make time series objects
lf.flow.z   = zoo(lf.flow$val,lf.flow$dates)

lf.flowE.z  = window(lf.flow.z,start='1958-01-01',end='2013-12-31')  #subset of data from 1957 to 2013
head(lf.flowE.z)
#Missing values from 1994-07-08 to 1994-07-13
lf.flowE.z<-na.approx(lf.flowE.z)      #Linearly interpolating to fill the NA values

#summary stats
#calculate summary statistics - annual
lf.flow.yr.stats = aggregate(lf.flow.z,by=format(index(lf.flow.z),"%Y"),FUN=.sumstats)
#calculate summary statistics - monthly
lf.flow.mo.stats = aggregate(lf.flowE.z,by=as.yearmon(index(lf.flowE.z)),FUN=.sumstats)

write.table(lf.flow.mo.stats,file="Annual_Monthly_avgs_adj.csv", col.names=NA,row.names=TRUE,sep=",") #Writes it into Excel file

L.9000 = function(x) {sum(x<9000,na.rm=TRUE)}  #function to calculate number of days less than 9000 cubic feet of flow
L.12000= function(x) {sum(x<12000,na.rm=TRUE)}  #function to calculate number of days less than 12000 cubic feet of flow
L.16000= function(x) {sum(x<16000,na.rm=TRUE)}   #function to calculate number of days less than 16000 cubic feet of flow

G.16000= function(x) {sum(x>16000)} #function to calculate number of days over 16000 cubic feet of flow
G.18000= function(x) {sum(x>18000)} #function to calculate number of days over 18000 cubic feet of flow
G.20000= function(x) {sum(x>20000)} #function to calculate number of days over 20000 cubic feet of flow
G.30000= function(x) {sum(x>30000)} #function to calculate number of days over 30000 cubic feet of flow
G.50000= function(x) {sum(x>50000)} #function to calculate number of days over 50000 cubic feet of flow

#Calculating the above functions per year
lf.flow.yr.stats.L9000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=L.9000) 
lf.flow.yr.stats.L12000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=L.12000)  
lf.flow.yr.stats.L16000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=L.16000)  

lf.flow.yr.stats.G16000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=G.16000)  
lf.flow.yr.stats.G18000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=G.18000)  
lf.flow.yr.stats.G20000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=G.20000)  
lf.flow.yr.stats.G30000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=G.30000)  
lf.flow.yr.stats.G50000 = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=G.50000)  

#Writing each into excel file
write.table(lf.flow.yr.stats.L9000,file="less_9000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.L12000,file="less_12000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.L16000,file="less_16000.csv", col.names=NA,row.names=TRUE,sep=",")

write.table(lf.flow.yr.stats.G16000,file="Greater_16000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.G18000,file="Greater_18000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.G20000,file="Greater_20000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.G30000,file="Greater_30000.csv", col.names=NA,row.names=TRUE,sep=",")
write.table(lf.flow.yr.stats.G50000,file="Greater_50000.csv", col.names=NA,row.names=TRUE,sep=",")

#Obtaining lowest mean flow for seven consecutive days of each year
Min7<-rollapply(lf.flowE.z,7, FUN=function(x) {Sum=(sum(x)/7)})
lf.flow.yr.stats.Min7<-aggregate(Min7,by=format(index(Min7),"%Y"),FUN=function(x) {min(x)})
write.table(lf.flow.yr.stats.Min7,file="Minimum7_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

Min30<-rollapply(lf.flowE.z,30,FUN=function(x) {Sum=(sum(x)/30)})
lf.flow.yr.stats.Min30<-aggregate(Min30,by=format(index(Min30),"%Y"),FUN=function(x) {min(x)})
write.table(lf.flow.yr.stats.Min30,file="Minimum30_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

Min60<-rollapply(lf.flowE.z,60, FUN=function(x) {Sum=(sum(x)/60)})
lf.flow.yr.stats.Min60<-aggregate(Min60,by=format(index(Min60),"%Y"),FUN=function(x) {min(x)})
write.table(lf.flow.yr.stats.Min60,file="Minimum60_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

Min90<-rollapply(lf.flowE.z,90, FUN=function(x) {Sum=(sum(x)/90)})
lf.flow.yr.stats.Min90<-aggregate(Min90,by=format(index(Min90),"%Y"),FUN=function(x) {min(x)})
write.table(lf.flow.yr.stats.Min90,file="Minimum90_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

Min120<-rollapply(lf.flowE.z,120, FUN=function(x) {Sum=(sum(x)/120)})
lf.flow.yr.stats.Min120<-aggregate(Min120,by=format(index(Min120),"%Y"),FUN=function(x) {min(x)})
write.table(lf.flow.yr.stats.Min120,file="Minimum120_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

#Max 60 day flow
Max60<-rollapply(lf.flowE.z,60, FUN=function(x) {Sum=(sum(x)/60)})
lf.flow.yr.stats.Max60<-aggregate(Max60,by=format(index(Max60),"%Y"),FUN=function(x) {max(x)})
write.table(lf.flow.yr.stats.Max60,file="Maximum60_DayAverages.csv", col.names=NA,row.names=TRUE,sep=",")

#1-day min
lf.flow.yr.stats.new = aggregate(lf.flowE.z,by=format(index(lf.flowE.z),"%Y"),FUN=.sumstats)  
lf.flow.yr.min.new<-lf.flow.yr.stats.new$Min

#Monthly Means Max and Mins
#read in Annual_Monthly_avgs.adj.txt
Annual_Monthly_avgs_adj <- read.delim("Annual_Monthly_avgs_adj.txt")
View(Annual_Monthly_avgs_adj)

head(Annual_Monthly_avgs_adj)
write.table(rollapply(Annual_Monthly_avgs_adj$Mean,12, by=12, FUN=mean), file="Annual_Monthly_Means.csv", col.names=NA, row.names=T, sep=",")
write.table(rollapply(Annual_Monthly_avgs_adj$Max, 12, by=12, FUN=mean), file="Annual_Monthly_Max.csv", col.names=F, row.names=T, sep=",")
write.table(rollapply(Annual_Monthly_avgs_adj$Min, 12, by=12, FUN=mean), file="Annual_Monthly_Min.csv", col.names=F, row.names=T, sep=",")

#Seasonal flows
#Winter
MonthlyFlow<-Annual_Monthly_avgs_adj$Mean*Annual_Monthly_avgs_adj$Nobs
Winternoleap<-rollapply(MonthlyFlow, 3, by=12, FUN=function(x){Winter=(sum(x)/90)})
Winterts<-zoo(Winternoleap, seq(1958,2013,1))

Winter.adj.forleap<-ifelse(index(Winterts) %in% seq(1960,2012,4), Winterts*(90/91), Winterts)
write.table(Winter.adj.forleap, file="Winter_avg.csv", col.names=F, row.names=T, sep=",")

#Summer
Summer<-rollapply(MonthlyFlow[7:672], 2,by=12, FUN=function(x){summer=(sum(x)/62)})
write.table(Summer, file="Summer_avg.csv", col.names=F, row.names=T, sep=",")

#Fall
Fall<-rollapply(MonthlyFlow[11:672],2,by=12, FUN=function(x){fall=(sum(x)/61)})
write.table(Fall, file="Fall_avg.csv", col.names=F, row.names=T, sep=",")

#ALL DATA FILES WERE COMBINED INTO MULTIPLE EXCEL DATA FILES REFERENCED IN ANALYSIS CODE
