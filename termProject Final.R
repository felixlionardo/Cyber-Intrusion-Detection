getwd()
library(readr)
library(ggplot2)
library(lubridate)
library(tibble)
library(depmixS4)
library(pracma)
library(scales)
library(grid)
library(ggbiplot)

data <- read.table("TermProjectData.txt",TRUE,sep=",")
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
 
data$Date<-as.POSIXct(data$Date, format ="%d/%m/%Y")
data$Time<-(period_to_seconds(hms(data$Time)))/60

timeWindowDayData<-subset(data,data$Time>1140& data$Time<=1320)

trainData<-subset(timeWindowDayData, timeWindowDayData$Date<as.POSIXct("1/1/2009",format ="%d/%m/%Y"))

traindf<-trainWeekdaysData[,c(3,4,5,6,7,8,9)]

pcadataTrainWeekdays <- prcomp(trainweekdaysdf,scale=TRUE)
ggbiplot(pcadataTrainWeekdays,choices = c(1,2), scale=1,alpha=0.4)
summary(pcadataTrainWeekdays)
pcadataTrainWeekdays #pc1 global intensity and active power

trainData_globalIntensity_mean <-tapply(data$Global_intensity, INDEX = data$Time, FUN = mean)
plot((0:1439)/60, trainData_globalIntensity_mean, xlab ="Time in hours", ylab="Average Global Intensity")

trainData_globalActivePower_mean <-tapply(data$Global_active_power, INDEX = data$Time, FUN = mean)
plot((0:1439)/60, trainData_globalActivePower_mean, xlab ="Time in hours", ylab="Average Global Active Power")

#mv 
mod4day <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainData,nstates=4,family=list(gaussian(),gaussian()))
fmodmulti4 <- fit(mod4day)

mod8day <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainData,nstates=8,family=list(gaussian(),gaussian()))
fmodmulti8 <- fit(mod8day)

mod12day <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainData,nstates=12,family=list(gaussian(),gaussian()))
fmodmulti12 <- fit(mod12day)

mod16day <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainData,nstates=16,family=list(gaussian(),gaussian()))
fmodmulti16 <- fit(mod16day)

mod20day <- depmix(list(Global_active_power~1, Global_intensity~1),data=trainData,nstates=20,family=list(gaussian(),gaussian()))
fmodmulti20 <- fit(mod20day)

plot(c(4,8,12,16,20),c(BIC(fmodmulti4),BIC(fmodmulti8),BIC(fmodmulti12),BIC(fmodmulti16), BIC(fmodmulti20)),ty="b",xlab ="number of state", ylab="BIC Weekdays")

plot(c(4,8,12,16,20),c(logLik(fmodmulti4),logLik(fmodmulti8),logLik(fmodmulti12),logLik(fmodmulti16), logLik(fmodmulti20)),ty="b",xlab ="number of state", ylab="log-likelihood WeekDays")

#1 
Testdata1 <- read.table("Data1(WithAnomalies).txt",TRUE,sep=",")


Testdata1$Date<-as.POSIXct(Testdata1$Date, format ="%d/%m/%Y")
Testdata1$Time<-(period_to_seconds(hms(Testdata1$Time)))/60

Testdata1<-subset(Testdata1,Testdata1$Time>1140& Testdata1$Time<=1320)

modTest1<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1,nstates=20,family=list(gaussian(),gaussian()))
modTest1<-setpars(modTest1,getpars(fmodmulti20))

#monthly
Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/12/2009",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/1/2010",format ="%d/%m/%Y"))
modTestmonthly1<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly1<-setpars(modTestmonthly1,getpars(fmodmulti20))
fbstatm1 <- forwardbackward(modTestmonthly1)
fbstatm1$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/1/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/2/2010",format ="%d/%m/%Y"))
modTestmonthly2<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly2<-setpars(modTestmonthly2,getpars(fmodmulti20))
fbstatm2 <- forwardbackward(modTestmonthly2)
fbstatm2$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/2/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/3/2010",format ="%d/%m/%Y"))
modTestmonthly3<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly3<-setpars(modTestmonthly3,getpars(fmodmulti20))
fbstatm3 <- forwardbackward(modTestmonthly3)
fbstatm3$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/3/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/4/2010",format ="%d/%m/%Y"))
modTestmonthly4<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly4<-setpars(modTestmonthly4,getpars(fmodmulti20))
fbstatm4 <- forwardbackward(modTestmonthly4)
fbstatm4$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/4/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/5/2010",format ="%d/%m/%Y"))
modTestmonthly5<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly5<-setpars(modTestmonthly5,getpars(fmodmulti20))
fbstatm5 <- forwardbackward(modTestmonthly5)
fbstatm5$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/5/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/6/2010",format ="%d/%m/%Y"))
modTestmonthly6<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly6<-setpars(modTestmonthly6,getpars(fmodmulti20))
fbstatm6 <- forwardbackward(modTestmonthly6)
fbstatm6$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/6/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/7/2010",format ="%d/%m/%Y"))
modTestmonthly7<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly7<-setpars(modTestmonthly7,getpars(fmodmulti20))
fbstatm7 <- forwardbackward(modTestmonthly7)
fbstatm7$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/7/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/8/2010",format ="%d/%m/%Y"))
modTestmonthly8<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly8<-setpars(modTestmonthly8,getpars(fmodmulti20))
fbstatm8 <- forwardbackward(modTestmonthly8)
fbstatm8$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/8/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/9/2010",format ="%d/%m/%Y"))
modTestmonthly9<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly9<-setpars(modTestmonthly9,getpars(fmodmulti20))
fbstatm9 <- forwardbackward(modTestmonthly9)
fbstatm9$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/9/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/10/2010",format ="%d/%m/%Y"))
modTestmonthly10<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly10<-setpars(modTestmonthly10,getpars(fmodmulti20))
fbstatm10 <- forwardbackward(modTestmonthly10)
fbstatm10$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/10/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/11/2010",format ="%d/%m/%Y"))
modTestmonthly11<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly11<-setpars(modTestmonthly11,getpars(fmodmulti20))
fbstatm11 <- forwardbackward(modTestmonthly11)
fbstatm11$logLike

Testdata1month<-subset(Testdata1, Testdata1$Date>=as.POSIXct("1/11/2010",format ="%d/%m/%Y") & Testdata1$Date<as.POSIXct("1/12/2010",format ="%d/%m/%Y"))
modTestmonthly12<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata1month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly12<-setpars(modTestmonthly12,getpars(fmodmulti20))
fbstatm12 <- forwardbackward(modTestmonthly12)
fbstatm12$logLike

fbstat1 <- forwardbackward(modTest1)
fbstat1$logLike

plot(c(1:12),
     c(fbstatm1$logLike,fbstatm2$logLike,fbstatm3$logLike,fbstatm4$logLike,fbstatm5$logLike,fbstatm6$logLike,
      fbstatm7$logLike,fbstatm8$logLike,fbstatm9$logLike,fbstatm10$logLike,fbstatm11$logLike,fbstatm12$logLike),type = "b",
     main = "Test data 1", xlab = "Month (December 2009 - November 2010)", ylab = "Log-likelihood")

#2
Testdata2 <- read.table("Data2(WithAnomalies).txt",TRUE,sep=",")


Testdata2$Date<-as.POSIXct(Testdata2$Date, format ="%d/%m/%Y")
Testdata2$Time<-(period_to_seconds(hms(Testdata2$Time)))/60

Testdata2<-subset(Testdata2,Testdata2$Time>1140& Testdata2$Time<=1320)

modTest2<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata2,nstates=20,family=list(gaussian(),gaussian()))
modTest2<-setpars(modTest2,getpars(fmodmulti20))

fbstat2 <- forwardbackward(modTest2)
fbstat2$logLike

#3
Testdata3 <- read.table("Data3(WithAnomalies).txt",TRUE,sep=",")


Testdata3$Date<-as.POSIXct(Testdata3$Date, format ="%d/%m/%Y")
Testdata3$Time<-(period_to_seconds(hms(Testdata3$Time)))/60

Testdata3<-subset(Testdata3,Testdata3$Time>1140& Testdata3$Time<=1320)

modTest3<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3,nstates=20,family=list(gaussian(),gaussian()))
modTest3<-setpars(modTest3,getpars(fmodmulti20))

fbstat3 <- forwardbackward(modTest3)
fbstat3$logLike

#monthly
Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/12/2009",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/1/2010",format ="%d/%m/%Y"))
modTestmonthly1<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly1<-setpars(modTestmonthly1,getpars(fmodmulti20))
fbstatm1 <- forwardbackward(modTestmonthly1)
fbstatm1$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/1/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/2/2010",format ="%d/%m/%Y"))
modTestmonthly2<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly2<-setpars(modTestmonthly2,getpars(fmodmulti20))
fbstatm2 <- forwardbackward(modTestmonthly2)
fbstatm2$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/2/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/3/2010",format ="%d/%m/%Y"))
modTestmonthly3<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly3<-setpars(modTestmonthly3,getpars(fmodmulti20))
fbstatm3 <- forwardbackward(modTestmonthly3)
fbstatm3$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/3/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/4/2010",format ="%d/%m/%Y"))
modTestmonthly4<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly4<-setpars(modTestmonthly4,getpars(fmodmulti20))
fbstatm4 <- forwardbackward(modTestmonthly4)
fbstatm4$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/4/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/5/2010",format ="%d/%m/%Y"))
modTestmonthly5<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly5<-setpars(modTestmonthly5,getpars(fmodmulti20))
fbstatm5 <- forwardbackward(modTestmonthly5)
fbstatm5$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/5/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/6/2010",format ="%d/%m/%Y"))
modTestmonthly6<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly6<-setpars(modTestmonthly6,getpars(fmodmulti20))
fbstatm6 <- forwardbackward(modTestmonthly6)
fbstatm6$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/6/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/7/2010",format ="%d/%m/%Y"))
modTestmonthly7<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly7<-setpars(modTestmonthly7,getpars(fmodmulti20))
fbstatm7 <- forwardbackward(modTestmonthly7)
fbstatm7$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/7/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/8/2010",format ="%d/%m/%Y"))
modTestmonthly8<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly8<-setpars(modTestmonthly8,getpars(fmodmulti20))
fbstatm8 <- forwardbackward(modTestmonthly8)
fbstatm8$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/8/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/9/2010",format ="%d/%m/%Y"))
modTestmonthly9<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly9<-setpars(modTestmonthly9,getpars(fmodmulti20))
fbstatm9 <- forwardbackward(modTestmonthly9)
fbstatm9$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/9/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/10/2010",format ="%d/%m/%Y"))
modTestmonthly10<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly10<-setpars(modTestmonthly10,getpars(fmodmulti20))
fbstatm10 <- forwardbackward(modTestmonthly10)
fbstatm10$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/10/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/11/2010",format ="%d/%m/%Y"))
modTestmonthly11<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly11<-setpars(modTestmonthly11,getpars(fmodmulti20))
fbstatm11 <- forwardbackward(modTestmonthly11)
fbstatm11$logLike

Testdata3month<-subset(Testdata3, Testdata3$Date>=as.POSIXct("1/11/2010",format ="%d/%m/%Y") & Testdata3$Date<as.POSIXct("1/12/2010",format ="%d/%m/%Y"))
modTestmonthly12<-depmix(list(Global_active_power~1, Global_intensity~1),data=Testdata3month,nstates=20,family=list(gaussian(),gaussian()))
modTestmonthly12<-setpars(modTestmonthly12,getpars(fmodmulti20))
fbstatm12 <- forwardbackward(modTestmonthly12)
fbstatm12$logLike

plot(c(1:12),
     c(fbstatm1$logLike,fbstatm2$logLike,fbstatm3$logLike,fbstatm4$logLike,fbstatm5$logLike,fbstatm6$logLike,
       fbstatm7$logLike,fbstatm8$logLike,fbstatm9$logLike,fbstatm10$logLike,fbstatm11$logLike,fbstatm12$logLike),type = "b",
     main = "Test data 3", xlab = "Month (December 2009 - November 2010)", ylab = "Log-likelihood")

plot(c(1:12),
     c(fbstatm1$logLike,fbstatm2$logLike,fbstatm3$logLike,fbstatm4$logLike,fbstatm5$logLike,fbstatm6$logLike,
       fbstatm7$logLike,fbstatm8$logLike,fbstatm9$logLike,fbstatm10$logLike,fbstatm11$logLike,fbstatm12$logLike),type = "b",
     main = "Test data 1 and 3", xlab = "Month (December 2009 - November 2010)", ylab = "Log-likelihood")

abline(h = -15700, col = "red", lty="dashed" )
abline(h = -13700, col = "purple",lty="dashed" )
abline(h = -14700, col = "orange",lty="dashed" )
abline(h = -16700, col = "green",lty="dashed" )
abline(h = -17700, col = "blue1",lty="dashed" )
legend("topleft",c("line 1","line 2", "line 3", "line 4", "line 5"), col = c("purple", "orange","red", "green", "blue"), lty= 2)
