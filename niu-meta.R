#niu meta 4
library("ggplot2")
 library("ggplot2")
data=read.table("niu-meta5.txt",header=TRUE,sep=",")
data=read.table("FP1-1.txt",header=TRUE,sep=",")

data$Sqrt.height=sqrt(data$Height)

data$Sqrt.height=sqrt(data$DH)

data$Frequecny=as.factor(data$Frequecny)

data$Frequecny_1=data$Frequecny



lm_1 = lm(mean~Sqrt.height,data=data[c(data$Frequecny==1),])

lm_2 = lm(mean~Sqrt.height,data=data[c(data$Frequecny==2),])

lm_3 = lm(mean~Sqrt.height,data=data[c(data$Frequecny==3),])

ggplot(data, aes(Sqrt.height, mean))+ geom_point(aes(shape=Frequecny)) + geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=Frequecny_1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,10))+theme_classic()+theme(legend.position="bottom")

ggplot(data, aes(DH, mean))+ geom_point(aes(shape=Frequecny)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black",aes(linetype=Frequecny_1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,120))+theme_classic()+theme(legend.position="bottom")



lm_1 = lm(mean.pvgrf~Sqrt.height,data=data[c(data$SampleFrequency==1),])
summary(lm_1)
lm_2 = lm(mean.pvgrf~Sqrt.height,data=data[c(data$SampleFrequency==2),])
summary(lm_2)
lm_3 = lm(mean.pvgrf~Sqrt.height,data=data[c(data$SampleFrequency==3),])
summary(lm_3)

lm_1 = lm(mean.pvgrf~Sqrt.height,data=data[c(data$Sex==1),])
summary(lm_1)


lm_1 = lm(mean.pvgrf~Sqrt.height,data=data[c(data$Sex==2),])
summary(lm_1)hean


data_1=data[data$SampleFrequency==1,]
lm_1 = lm(mean.pvgrf~sqrt(Height),data=data_1)
summary(lm_1)
data_2=data[data$SampleFrequency==2,]
lm_2 = lm(mean.pvgrf~sqrt(Height),data=data_2)
summary(lm_2)
data_3=data[data$SampleFrequency==3,]
lm_3 = lm(mean.pvgrf~sqrt(Height),data=data_3)
summary(lm_3)


