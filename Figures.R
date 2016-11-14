#Have to run data analysis code prior to running all this figure stuff
setwd("Insert name here")

#############################
#Figure 1
#############################
tiff(filename="Figure1.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)
##  Split the screen into two rows and one column, defining screens 1 and 2.
split.screen( figs = c( 2, 1 ) )
##  Split screen 1 into one row and three columns, defining screens 3, 4, and 5.
split.screen( figs = c( 1, 2 ), screen = 1 )
##  Split screen 2 into one row and two columns, defining screens 6 and 7.
split.screen( figs = c( 1, 1 ), screen = 2 )
par(oma = c(1,1,-0.1,0) + 0.1, mar = c(1,1,1,1) + 0.1, xpd=NA)
##  The first plot is located in screen 3:
screen( 3 )
#insert first plot here
par(oma=c(4,1,1,1)+0.1,mar = c(4,4,1,2) + 0.1)
plot(Newdata$year[1:27],((Newdata$adj_landings[1:27]*0.453592)/1000000),xlim=c(1960,2015),ylim=c(0,3),las=1,type="l",ylab="Landings (in millions of kg)",  xlab="Year",lwd=2)
lines(Newdata$year[30:56],((Newdata$adj_landings[30:56]*0.453592)/1000000),lwd=2)
text(1962,2.9,"A",cex=1.25)

##  The second plot is located in screen 4:
screen( 4 )
#insert second plot here
#Figure 6
par(oma=c(4,1,1,1)+0.1,mar = c(4,4,1,1) + 0.1)
plot(lf.flow.yr.stats$Mean,type='p', pch=18, xlab="Year", ylab="",las=1, ylim=c(0,40000))
mtext("Mean Annual Discharge (cfs)", side=2, line=4, cex=1)
abline(lm(lf.flow.yr.stats$Mean~seq(1957,2014,1)))
text(1960,39000,"B", cex=1.25)

##  The third plot is located in screen 5:
screen( 5 )
par(oma = c(4,4,0,4) + 0.1, mar = c(1,1,1,1) + 0.1)
#insert third plot here
#CPUE PLOT, Figure 1, Lower panel
plot(Newdata$year[3:56],CPUE.o[3:56]*0.453592, type="b",axes=F,pch=16,xlab="",ylim=c(0,5000),ylab="")
axis(2,ylim=c(0,4000),las=1)
mtext("Catch Per Oysterman (kg)", side=2,line=3.5)
box()
par(new=T)
plot(Newdata$year[3:56],CPUE.t[3:56]*0.453592,type="b",pch=17,xlab="",ylab="",ylim=c(0,50),axes=F)
mtext("Catch Per Trip (kg)",side=4,line=3.5)
axis(4,ylim=c(0,100),las=1)
axis(1,at=c('1960','1965','1970','1975','1980','1985','1990','1995','2000','2005','2010','2015'))
mtext("Year",side=1,col="black",line=2.5) 

legend("topright",legend=c("Catch Per Oysterman","Catch Per Trip"),cex=.75,pch=c(16,17),bty="n")
text(1960,50,"C", cex=1.25)

#  Close all screens.
close.screen( all = TRUE )

dev.off()


######################################################################################################
#Figure 4
######################################################################################################
tiff(filename="Figure4.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)

par(mfrow=c(3,1), oma = c(5,4,0,0.5) + 0.1, mar = c(1,1,1,1) + 0.1, xpd=NA)
plot(Newdata$year[30:56],Newdata$CPUE[30:56]*0.453592,pch=18,type="p",ylim=c(0,4000),las=1,xlim=c(1985,2015),lwd=2, xlab="",ylab="", cex.axis=1.2)
lines(Newdata$year[30:56],Pred.CPUE.Nickmodel,lty=1,lwd=2)
lines(Newdata$year[30:56],Pred.CPUE.Wilbermodel,lty=6,lwd=2)
legend("topright",c("Model 1.1","Model 2.1","Observed CPUE"),lty=c(1,6,NA),lwd=c(2,2,NA),pch=c(NA,NA,18),bty="n",cex=1.1)
text(1990,350, expression(italic(paste("Model 1.1 ","CPUE = 3555.8x - 11075.5"))),cex=1.1)
text(1989.5,90, expression(italic(paste("Model 2.1 ","CPUE = -5.4x + 3449.7"))),cex=1.1)
text(1985,3900,"A", cex=1.3)
plot(Newdata$year[30:56],Newdata$CPUE[30:56]*0.453592,xlim=c(1985,2015),las=1,xlab="",ylab="", type="p",lwd=2,pch=18, ylim=c(0,4000),cex.axis=1.2)
lines(seq(1987,2013,1),Pred_CPUE_recent,lty=1,lwd=2)
lines(seq(1987,2013,1),Pred_CPUE_X1_recent,lty=6,lwd=2)
legend("topright",c("Model 1.2","Model 2.2","Observed CPUE"),lty=c(1,6,NA),lwd=c(2,2,NA),pch=c(NA,NA,18),bty="n",cex=1.1)
text(1990,350, expression(italic(paste("Model 1.2 ","CPUE = 1844.4x - 5718.9"))),cex=1.1)
text(1989.65,90, expression(italic(paste("Model 2.2 ","CPUE = -1.0x + 1465.1"))),cex=1.1)
text(1985,3900,"B", cex=1.3)
plot(Newdata$year, Newdata$CPUE*0.453592,lwd=2,ylim=c(0,4000),xlab="",type="p",las=1,pch=18,ylab="",cex.axis=1.2)
lines(Newdata$year[3:56],Pred.lm.Nick,lty=1,lwd=2)
lines(Newdata$year[3:56],Pred.lm.Wilber,lty=6,lwd=2)
legend("topright",c("Model 1.3","Model 2.3","Observed CPUE"),lty=c(1,6,NA),lwd=c(2,2,NA),pch=c(NA,NA,18),bty="n",cex=1.1)
text(1967.5,350, expression(italic(paste("Model 1.3 ","CPUE = 6256.0x - 22236.1"))),cex=1.1)
text(1966.6,90, expression(italic(paste("Model 2.3 ","CPUE = -6.9x + 2895.7"))),cex=1.1)
text(1958,3900,"C", cex=1.3)
mtext("CPUE", side=2, line=2.5, outer=T, cex=0.9)
mtext("Year", side=1, line=2, outer=T, cex=0.9)

dev.off()

#######################################################################################################
#Figures 2-3 & 5-6
#######################################################################################################
tiff(filename="Figure2.tiff", height = 19, width = 17.3, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(3,3), oma = c(3,4,0,0) + 0.1, mar = c(0.5,1,1,1) + 0.1)

#Multipanel plot for minimum daily flow per year
#O year lag
plot(cor.mat[3:27,4],Newdata$CPUE[3:27], ylim=c(0,10500), xlim=c(3.5,4.2),pch=16, main="1960-1984",xaxt='n',las=1,cex.main=1.3, cex.axis=1.1)
plot(cor.mat[29:56,4],Newdata$CPUE[29:56],ylim=c(0,10500), xlim=c(3.5,4.2), main="1987-2013",xaxt='n',yaxt="n",cex.main=1.3)
plot(cor.mat[,4],Newdata$CPUE[1:56],ylim=c(0,10500), xlim=c(3.5,4.2), main="1960-2013",xaxt='n', yaxt="n",cex.main=1.3)
points(cor.mat[3:27,4],Newdata$CPUE[3:27], ylim=c(0,10500), xlim=c(3.5,4.2),pch=16)

#2 year lag
plot(cor.mat[1:25,4],Newdata$CPUE[3:27], ylim=c(0,10500),pch=16, xlim=c(3.5,4.2),xaxt="n",las=1, cex.axis=1.1)
plot(cor.mat[28:54,4],Newdata$CPUE[30:56],ylim=c(0,10500), xlim=c(3.5,4.2),xaxt='n',yaxt="n")
plot(cor.mat[1:54,4],Newdata$CPUE[3:56],ylim=c(0,10500), xlim=c(3.5,4.2),xaxt='n',yaxt="n")
points(cor.mat[1:25,4],Newdata$CPUE[3:27], ylim=c(0,10500),pch=16, xlim=c(3.5,4.2))

#3 year lag
plot(cor.mat[1:24,4],Newdata$CPUE[4:27], ylim=c(0,10500),pch=16, xlim=c(3.5,4.2),las=1, cex.axis=1.1)
plot(cor.mat[27:53,4],Newdata$CPUE[30:56],ylim=c(0,10500), xlim=c(3.5,4.2),yaxt="n", cex.axis=1.1)
plot(cor.mat[1:53,4],Newdata$CPUE[4:56],ylim=c(0,10500), xlim=c(3.5,4.2),yaxt="n", cex.axis=1.1)
points(cor.mat[1:24,4],Newdata$CPUE[4:27], ylim=c(0,10500),pch=16, xlim=c(3.5,4.2))

mtext("O Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.88)
mtext("2 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.50)
mtext("3 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.15)
mtext("CPUE", side=2, line=2, outer=T, cex=0.75)
mtext("Annual Minimum Average Daily Discharge (cfs-log transformed)", side=1, line=2, outer=T, cex=0.75)

dev.off()

#Multipanel plot for number of days less that 12,000 cfs per year
#O year lag
tiff(filename="Figure3.tiff", height = 19, width = 17.3, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(3,3), oma = c(3,4,0,0) + 0.1, mar = c(0.5,1,1,1) + 0.1)

plot(cor.mat[3:27,12],Newdata$CPUE[3:27], main="1960-1984",pch=16, ylim=c(0,10000), xlim=c(0,350), xaxt="n",las=1,cex.main=1.3, cex.axis=1.1)
plot(cor.mat[29:56,12], Newdata$CPUE[29:56],main="1987-2013",ylim=c(0,10000), xlim=c(0,350), xaxt="n", yaxt='n',cex.main=1.3)
plot(cor.mat[,12],Newdata$CPUE[1:56], main="1960-2013",ylim=c(0,10000), xlim=c(0,350), xaxt="n", yaxt='n',cex.main=1.3)
points(cor.mat[3:27,12],Newdata$CPUE[3:27], pch=16, ylim=c(0,10000), xlim=c(0,350))

#2 year lag
plot(cor.mat[1:25,12],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(0,350),xaxt="n",las=1, cex.axis=1.1)
plot(cor.mat[28:54,12],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(0,350),xaxt='n',yaxt="n")
plot(cor.mat[1:54,12],Newdata$CPUE[3:56],ylim=c(0,10000), xlim=c(0,350),xaxt='n',yaxt="n")
points(cor.mat[1:25,12],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(0,350))

#3 year lag
plot(cor.mat[1:24,12],Newdata$CPUE[4:27],  ylim=c(0,10000), pch=16, xlim=c(0,350),las=1, cex.axis=1.1)
plot(cor.mat[27:53,12],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(0,350),yaxt="n", cex.axis=1.1)
plot(cor.mat[1:53,12],Newdata$CPUE[4:56], ylim=c(0,10000), xlim=c(0,350),yaxt="n", cex.axis=1.1)
points(cor.mat[1:24,12],Newdata$CPUE[4:27],  ylim=c(0,10000), pch=16, xlim=c(0,350))

mtext("O Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.88)
mtext("2 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.50)
mtext("3 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.15)
mtext("CPUE", side=2, line=2, outer=T, cex=0.75)
mtext("Annual Number of Days Discharge was Below 12,000 cfs", side=1, line=2, outer=T, cex=0.75)

dev.off()

#Multipanel plot for 30-day minimum per year
#O year lag
tiff(filename="Figure5.tiff", height = 19, width = 17.3, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(3,3), oma = c(3,4,0,0) + 0.1, mar = c(0.5,1,1,1) + 0.1)

plot(cor.mat[3:27,6],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4), ylab="0 Time Lag", main="1960-1984",las=1,xaxt='n', cex.axis=1.1, cex.main=1.3)
plot(cor.mat[29:56,6],Newdata$CPUE[29:56],ylim=c(0,10000), xlim=c(3.5,4.4), main="1987-2013",xaxt='n',yaxt="n",cex.main=1.3)
plot(cor.mat[,6],Newdata$CPUE[1:56],ylim=c(0,10000), xlim=c(3.5,4.4), main="1960-2013",xaxt='n', yaxt="n",cex.main=1.3)
points(cor.mat[3:27,6],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

#2 year lag
plot(cor.mat[1:25,6],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4),xaxt="n",las=1, cex.axis=1.1)
plot(cor.mat[28:54,6],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(3.5,4.4),xaxt='n',yaxt="n")
plot(cor.mat[1:54,6],Newdata$CPUE[3:56],ylim=c(0,10000), xlim=c(3.5,4.4),xaxt='n',yaxt="n")
points(cor.mat[1:25,6],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

#3 year lag
plot(cor.mat[1:24,6],Newdata$CPUE[4:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4), ylab="3 Year Lag",las=1, cex.axis=1.1)
plot(cor.mat[27:53,6],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(3.5,4.4),yaxt="n", cex.axis=1.1)
plot(cor.mat[1:53,6],Newdata$CPUE[4:56],ylim=c(0,10000), xlim=c(3.5,4.4),yaxt="n", cex.axis=1.1)
points(cor.mat[1:24,6],Newdata$CPUE[4:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

mtext("O Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.88)
mtext("2 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.50)
mtext("3 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.15)
mtext("CPUE", side=2, line=2, outer=T, cex=0.75)
mtext("Lowest 30-Consecutive Day Daily Average Discharge (cfs-log transformed)", side=1, line=2, outer=T, cex=0.75)

dev.off()

#Multipanel plot for 90-day minimum per year
#O year lag
tiff(filename="Figure6.tiff", height = 19, width = 17.3, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(3,3), oma = c(3,4,0,0) + 0.1, mar = c(0.5,1,1,1) + 0.1)

plot(cor.mat[3:27,8],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4),las=1, ylab="0 Time Lag", main="1960-1984",xaxt='n',cex.axis=1.1,cex.main=1.3)
plot(cor.mat[29:56,8],Newdata$CPUE[29:56],ylim=c(0,10000), xlim=c(3.5,4.4), main="1987-2013",xaxt='n',yaxt="n",cex.main=1.3)
plot(cor.mat[,8],Newdata$CPUE[1:56],ylim=c(0,10000), xlim=c(3.5,4.4), main="1960-2013",xaxt='n', yaxt="n",cex.main=1.3)
points(cor.mat[3:27,8],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

#2 year lag
plot(cor.mat[1:25,8],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4),xaxt="n",las=1, cex.axis=1.1)
plot(cor.mat[28:54,8],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(3.5,4.4),xaxt='n',yaxt="n")
plot(cor.mat[1:54,8],Newdata$CPUE[3:56],ylim=c(0,10000), xlim=c(3.5,4.4),xaxt='n',yaxt="n")
points(cor.mat[1:25,8],Newdata$CPUE[3:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

#3 year lag
plot(cor.mat[1:24,8],Newdata$CPUE[4:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4),las=1, ylab="3 Year Lag", cex.axis=1.1)
plot(cor.mat[27:53,8],Newdata$CPUE[30:56],ylim=c(0,10000), xlim=c(3.5,4.4),yaxt="n", cex.axis=1.1)
plot(cor.mat[1:53,8],Newdata$CPUE[4:56], ylim=c(0,10000), xlim=c(3.5,4.4),yaxt="n", cex.axis=1.1)
points(cor.mat[1:24,8], Newdata$CPUE[4:27], ylim=c(0,10000),pch=16, xlim=c(3.5,4.4))

mtext("O Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.88)
mtext("2 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.50)
mtext("3 Year Lag", side=2, line=3, outer=TRUE, cex=.8,adj=0.15)
mtext("CPUE", side=2, line=2, outer=T, cex=0.75)
mtext("Lowest 90-Consecutive Day Daily Average Discharge (cfs-log transformed)", side=1, line=2, outer=T, cex=0.75)


dev.off()

#####################################################################################################
#Figure 7
#####################################################################################################

tiff(filename="Figure7.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)

#Plots of above correlations

par(mfrow=c(2,1), oma = c(4,3,0,0) + 0.1, mar = c(2,1,2,1) + 0.1)

plot(monthlyflow.stats$Mean,Salinity.monthly.db$Mean,ylim=c(0,35),xlim=c(0,78000),pch=2,las=1, xlab="Apalachicola River Discharge (monthly mean cfs)", ylab="Salinity (monthly mean)")
points(monthlyflow.stats$Mean, Salinity.monthly.cp$Mean, pch=20)
legend("topright", legend=c("Dry Bar", "Cat Point"), pch=c(2,20), bty="n")
mtext("Monthly Mean Salinity",side=2,col="black",line=2.5) 
mtext("Monthly Mean Apalachicola River Discharge (cfs)",side=1,col="black",line=2.5) 
text(150,35,"A",cex=1.25)

plot(Salinity.monthly.cp$Mean, Salinity.monthly.db$Mean,ylim=c(0,35),xlim=c(0,35), las=1,xlab="Cat Point Salinity", ylab="Dry Bar Salinity")
mtext("Cat Point Monthly Mean Salinity",side=1,col="black",line=2.5) 
mtext("Dry Bar Monthly Mean Salinity", side=2, col="black", line=2.5)
text(0.25,35,"B",cex=1.25)

dev.off()


