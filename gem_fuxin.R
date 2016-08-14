library(mgcv)
library(gam)
data=read.table("gam_fuxin.txt",header=TRUE,sep=',')
data_1 = data[data$City!='????━?????━??1????━?2',]

data=read.table("gam_fuxin_1.txt",header=TRUE,sep=',')
data_1 = data[data$City!='????━?????━??1????━?2',]

data_1$City <- relevel(as.factor(data_1$City), ref = "1")
data_1$Gender <- relevel(as.factor(data_1$Gender), ref = "1")

lm1=lm(Morbidity~City+Gender+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1)


lm1=lm(Mortality~City+Gender+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1)

ct1<-mgcv::gam(Morbidity~City+Gender+GDP+s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=poisson(link = "log"),data=data_1)

ct1<-gam.fit(Morbidity~City+Gender+GDP+CarOwnership+s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=poisson(link = "log"),data=data_1)


ct1<-mgcv::gam(Morbidity~s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=poisson(link = "log"),data=data_1)

ct1<-mgcv::gam(Morbidity~s(AQI_Spring),,family=poisson(link = "log"),data=data_1)
ct1<-mgcv::gam(Mortality~City+Gender+GDP+CarOwnership+s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=poisson(link = "log"),data=data_1)


data_2=as.matrix(data_1)
ct1<-mgcv::gam(Morbidity~s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=Gamma(link=log),data=data_2)

ct1<-mgcv::gam(Morbidity~s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=Gamma(link=log),data=data_1)

ct1<-mgcv::gam(Morbidity~City+Gender+GDP+CarOwnership+s(AQI_Spring)+s(AQI_Summer)+s(AQI_Autumn)+s(AQI_Winter),family=Gamma(link=log),data=data_1)

ct1<-mgcv::gam(Morbidity~s(AQI_Spring),family=poss,data=data_1)


lm1 = lm(Morbidity~AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter,data=data_1[data_1$Gender==1&data_1$City=='????′????━??━????′????━??━??',])

lm1 = lm(Morbidity~AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter,data=data_1[data_1$Gender==1,])

ct1<-gam(Morbidity~s(AQI_Spring),family=Gamma(link=log),data=data_1[data_1$Gender==1,])

data.frame(gam.data)
gam(y ~ s(x) + z, ,family=Gamma(link=log),data=gam.data)


summary(lm(Morbidity~City+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1[data_1$Gender==1,]))


lm1=lm(Morbidity~City+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1[data_1$Gender==1,])
step(lm1)
summary(lm(Morbidity ~ AQI_Spring + AQI_Summer + AQI_Winter + GDP + CarOwnership,data=data_1[data_1$Gender==1,]))

lm2=lm(Mortality ~ City+Year + AQI_Spring + AQI_Summer + AQI_Winter + GDP + CarOwnership,data=data_1[data_1$Gender==1,])
step(lm2)
summary(lm(Mortality ~ AQI_Spring + AQI_Winter + CarOwnership,data=data_1[data_1$Gender==1,]))

lm1=lm(Morbidity~City+Year+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1[data_1$Gender==2,])
step(lm1)
summary(lm(Morbidity ~  AQI_Spring + AQI_Summer + AQI_Autumn + AQI_Winter + GDP + CarOwnership,data=data_1[data_1$Gender==2,]))

lm2=lm(Mortality ~ City+Year + AQI_Spring + AQI_Summer + AQI_Winter + GDP + CarOwnership,data=data_1[data_1$Gender==2,])
step(lm2)
summary(lm(Mortality ~ AQI_Spring + AQI_Winter + CarOwnership,data=data_1[data_1$Gender==2,]))




summary(lm(Morbidity~Year+Gender+AQI_Spring+AQI_Summer+AQI_Autumn+AQI_Winter+GDP+CarOwnership,data=data_1[data_1$Gender==1&data_1$City=='????′????━??━????′????━??━??',]))


lm1 = lm(Morbidity~AQI_Spring+GDP+CarOwnership,data=data_1[data_1$Gender==1&data_1$City=='????′????━??━????′????━??━??',])
lm1 = lm(Morbidity~AQI_Spring+GDP+CarOwnership,data=data_1[data_1$Gender==2&data_1$City=='????′????━??━????′????━??━??',])
summary(lm(Morbidity~AQI_Spring+GDP+CarOwnership,data=data_1[data_1$Gender==1&data_1$City=='????━?|?o????′?o',]))
summary(lm(Morbidity~AQI_Spring+GDP+CarOwnership,data=data_1[data_1$Gender==2&data_1$City=='????━?|?o????′?o',]))

data(trees)
ct1 = gam(Volume ~ s(Height) + s(Girth),family=Gamma(link=log),data=trees)


par(mfrow=c(2,2))

plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam

==================
data = read.csv("beijing-aqi",header=FALSE)
tiff(file = "fuxin-figure-shanghai.tif", res =300, width = 5000, height = 2400) 
plot(data$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim = c(60,190))
axis(1,c(1,13,25,37,48,61,73),c('03/01/01','04/01/01','05/01/01','06/01/01','07/01/01','08/01/01','09/01/01'),cex.axis=2)
dev.off()
data = read.csv("shanghai-aqi",header=FALSE)
tiff(file = "fuxin-figure-beijing.tif", res =300, width = 5000, height = 2400) 
plot(data$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim=c(60,190))
axis(1,c(1,13,25,37,48,61,73),c('03/01/01','04/01/01','05/01/01','06/01/01','07/01/01','08/01/01','09/01/01'),cex.axis=2)
dev.off()

data = read.csv("dalian-aqi",header=FALSE)
tiff(file = "fuxin-figure-dalian.tif", res =300, width = 5000, height = 2400) 
plot(data$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim=c(60,190))
axis(1,c(1,13,25,37,48,61,73),c('03/01/01','04/01/01','05/01/01','06/01/01','07/01/01','08/01/01','09/01/01'),cex.axis=2)
dev.off()

data = read.csv("dalian-aqi",header=FALSE)
tiff(file = "fuxin-figure-dalian.tif", res =300, width = 5000, height = 2400) 
plot(data$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim=c(60,190))
axis(1,c(1,13,25,37,48,61,73),c('03/01/01','04/01/01','05/01/01','06/01/01','07/01/01','08/01/01','09/01/01'),cex.axis=2)
dev.off()

data = read.csv("guangzhou-aqi",header=FALSE)
tiff(file = "fuxin-figure-guangzhou.tif", res =300, width = 5000, height = 2400) 
plot(data$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim=c(60,190))
axis(1,c(1,13,25,37,48,61,73),c('03/01/01','04/01/01','05/01/01','06/01/01','07/01/01','08/01/01','09/01/01'),cex.axis=2)
dev.off()





aqi = read.table("aqi_avg.txt",header=FALSE,sep='\t')

baqi = matrix(as.numeric(aqi[,1]), nrow = 7, byrow = TRUE)
saqi = matrix(as.numeric(aqi[,2]), nrow = 7, byrow = TRUE)
daqi = matrix(as.numeric(aqi[,4]), nrow = 7, byrow = TRUE)
gaqi = matrix(as.numeric(aqi[,3]), nrow = 7, byrow = TRUE)

aqi = read.table("aqi_city.txt",head=TRUE,sep="\t")
dat = rbind(baqi,saqi,gaqi,daqi)
dat = rbind(dat,dat)
dat = cbind(aqi,dat)
dat= read.table('aqi_data._1txt.txt',header=TRUE,sep='\t')

ct1<-gam( Mortality~City + Gerder + AvgGDP+s(m1)+s(m12),data=dat)
dat = as.data.frame(scale(dat))
ct1<-gam( Mortality~City + Gender + s(AvgGDP)+m1+ m9 + m12,data=dat)
ct1<-gam( Mortality~City + Gender + s(AvgGDP)+s(m1)+ s(m11) + s(m12),data=dat)


plot(da$V1,type='b',lwd='3',xaxt='n',cex.axis=2,ylim=c(60,190))

