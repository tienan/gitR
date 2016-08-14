library("ggplot2")
data = read.table(file=".txt",header=TRUE)

data[data$LOS<57.35&data$LOS>4,]
ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS, TC))+ geom_point() + geom_smooth(method='lm',se=TRUE,size=1,color="black")

ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS, TC,shape=AGE))+ geom_point() + geom_smooth(method='lm',se=TRUE,size=1,color="black")

ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS,TC))+ geom_point(aes(color=factor(AGE))) + geom_smooth(method='lm',se=TRUE,color="black")

ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS,LC))+ geom_point(aes(color=factor(AGE))) + geom_smooth(method='lm',se=TRUE,color="black")

ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS,WD))+ geom_point(aes(color=factor(AGE))) + geom_smooth(method='lm',se=TRUE,color="black")

ggplot(data[data$LOS<57.35&data$LOS>4,], aes(LOS,CC))+ geom_point(aes(color=factor(AGE))) + geom_smooth(method='lm',se=TRUE,color="black")

%% this proce

library("ggplot2")
dataYibao = read.table(file="YibaoData.txt",head=TRUE)
> cor.test(dataYibao$LOS,dataYibao$TC-dataYibao$OC, alternative="two.side",method="pearson",conf.level=0.95)

        Pearson's product-moment correlation

data:  dataYibao$LOS and dataYibao$TC - dataYibao$OC
t = 105.4722, df = 9614, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.7230002 0.7415361
sample estimates:
      cor 
0.7324038 

> cor.test(dataYibao$LOS,dataYibao$TC, alternative="two.side",method="pearson",conf.level=0.95)

        Pearson's product-moment correlation

data:  dataYibao$LOS and dataYibao$TC
t = 102.9971, df = 9614, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.7146417 0.7336504
sample estimates:
      cor 
0.7242836 




qplot(LOS,dataYibao$TC,data=dataYibao)+geom_smooth(method="lm")

ggplot(temData, aes(LOS,TC-OC))+ geom_point(aes(color=factor(Age))) + geom_smooth(method='lm',se=TRUE,color="black")
ggplot(temData, aes(MC,TC-OC))+ geom_point(aes(color=factor(Age))) + geom_smooth(method='lm',se=TRUE,color="black")
ggplot(temData, aes(LOS,MC))+ geom_point(aes(color=factor(Age))) + geom_smooth(method='lm',se=TRUE,color="black")



cor.test(dataYibao$LOS,dataYibao$TC, alternative="two.side",method="pearson",conf.level=0.95)


names = levels(dataYibao$JCD)
qplot(LOS,TC,data=temData)+geom_smooth(method="lm")
scale = c(0,40,100)
for(j in 1:length(names)){
for (i in 1:2){
temData = dataYibao[dataYibao$JCD==names[j]&dataYibao$Age<scale[i+1]&dataYibao$Age>=scale[i],]
if(nrow(temData)>2) {
lm.sol = lm(formula = temData$TC-temData$OC ~ temData$LOS)
ex=data.frame(temData$LOS)
p_result=predict(lm.sol,ex,interval="prediction",level=0.95)
index = temData$TC-p_result[,3]
(temData$index = index)
write.table(temData,file="predicting_result",quote = FALSE,append=TRUE,col.name=FALSE)
}
}
}

for (i in 1:10){
temData = dataYibao[dataYibao$JCD==names[1]&dataYibao$Age<scale[i+1]&dataYibao$Age>=scale[i],]
if(nrow(temData)>1) print(i)
}

lm.sol = lm(formula = temData$TC-temData$OC ~ temData$MC)
summary(lm.sol)

boxplot(TC-OC~JCD,data=dataYibao,ylim = c(0, 50000))



cor.test(dataYibao$LOS,dataYibao$TC-dataYibao$OC, alternative="two.side",method="pearson",conf.level=0.95)test
=====the generation of Yibao data
data = read.ta



