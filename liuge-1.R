library("ggplot2")
 library("ggplot2")
liu_data=read.table(file="liuge-1.txt",header=TRUE)

liu_data=read.table(file="vGRF.txt",header=TRUE)
vGRF=read.table(file="vGRF-1.txt",header=TRUE)

liu_data=read.table(file="vGRF-1nozero.txt",header=TRUE)

liu_data=read.table(file="vGRFmd.txt",header=TRUE)
qplot(Sqrt.High, vGRF, data = liu_data, colour = factor(kinds))+geom_smooth(method='lm',se=FALSE)
qplot(Sqrt.High, vGRF, data = liu_data, shape = factor(kinds))+ geom_smooth(method='lm',se=FALSE)

data_1 = liu_data[liu_data$both==0,]
data_2 = liu_data[c(liu_data$both==0,liu_data$distance_plant==1),]
data_3 = aggregate(data_2[,2], list(provID=data_2[,1]), mean)


ggplot( liu_data, aes(Sqrt.height, mmeanpvgrf),aes(group = SampleFrequency)) + geom_smooth(method='lm',se=TRUE)
ggplot(liu_data, aes(Sqrt.height,  mmeanpvgrf))+ geom_point(aes(shape=SampleFrequency)) + geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=Frequency1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,10))+theme_classic()+theme(legend.position="bottom")



data_1 = liu_data[c(liu_data$both==0,liu_data$gender==1),]
qplot(High, vGRF, data = liu_data, facets = .~Frequent+footwear)+geom_smooth(method='lm',se=TRUE)
qplot(High, vGRF, data = liu_data, colour = factor(kinds))+geom_smooth(method='lm',se=TRUE)



aggregate(presidents, nfrequency = 1,


FUN = weighted.mean, w = c(1, 1, 0.5, 1))

aggregate(list(vGRF=liu_data$vGRF), list(High=liu_data$High),weighted.mean)


vRGFbox <- ggplot(liu_data, aes(High, vGRF)) + geom_smooth()

vRGFbox + geom_line( colour = "#3366FF")

ggplot(liu_data, aes(High, vGRF)) + geom_smooth()+geom_boxplot(aes(group = High),)


ggplot(liu_data, aes(Sqrt.High, vGRF))  + geom_smooth(method='lm',se=TRUE)+geom_point()

 qplot(Sqrt.High, vGRF, data = liu_data, facets = .~Frequent+footwear+distance+surface+gender)+geom_smooth(method='lm',se=TRUE)

 qplot(Sqrt.High, vGRF, data = liu_data, facets = .~Frequent)+geom_smooth(method='lm',se=TRUE)

 qplot(Sqrt.High, vGRF, data = liu_data, colour=factor(Frequent))+geom_smooth(method='lm',se=TRUE)

ggplot(vGRF, aes(Sqrt.High, vGRF,shape=Frequency,linetype=Frequency))+ geom_point() + geom_smooth(method='lm',se=TRUE,size=1,color="black")+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+scale_x_continuous(limits = c(0, 9))
ggplot(vGRF, aes(DH, PvGRF))+ geom_point(aes(shape=Frequency)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black")+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,101))+theme(legend.position=c(0.15,0.74))


tiff(file = "fig-2.tiff", res = 900, width = 3800, height = 3400) 
ggplot(vGRF, aes(DH, PvGRF))+ geom_point(aes(shape=Frequency)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black",aes(linetype=Frequency1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,101))+theme_classic()+theme(legend.position="bottom") 
dev.off()

tiff(file = "fig-1.tiff", res = 900, width = 3800, height = 3400) 
ggplot(vGRF, aes(DH, PvGRF))+ geom_point(aes(shape=Frequency)) + geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=Frequency_1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,10))+theme_classic()+theme(legend.position="bottom")
dev.off()

ggplot(vGRF, aes(Sqrt.High, PvGRF))+ geom_point(aes(shape=Frequency)) + geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=Frequency_1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,10))+theme_classic()+theme(legend.position="bottom")


text(0, 0, "0. 'blank'"   ,  adj=c(0,.5))
text(0, 1, "1. 'solid'"   ,  adj=c(0,.5))
text(0, 2, "2. 'dashed'"  ,  adj=c(0,.5))
text(0, 3, "3. 'dotted'"  ,  adj=c(0,.5))
text(0, 4, "4. 'dotdash'" ,  adj=c(0,.5))
text(0, 5, "5. 'longdash'",  adj=c(0,.5))
text(0, 6, "6. 'twodash'" ,  adj=c(0,.5))

vGRF=read.table(file="vGRF-1.txt",header=TRUE)

g2=ggplot(vGRF, aes(DH, PvGRF,shape=Frequency,linetype=Frequency)+geom_smooth(method='lm', formula=y~I(x*x))+geom_point(data=df, aes(x=x,y=y, colour=colour), alpha=.9)



ggplot(vGRF, aes(DH, PvGRF,shape=Frequency,linetype=Frequency))+ geom_point() + geom_smooth(method='lm',se=TRUE,size=1,color="black")+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 8),xlim=c(0,9))+opts(legend.position=c(0.15,0.7))


ggplot( vGRF, aes(Sqrt.High, vGRF),aes(group = frequency)) + geom_smooth(method='lm',se=TRUE)
===========================================================


scale_area("Cook¡¯s distance")



 lm_3 = lm(Sqrt.High~ vGRF,data=liu_data[c(liu_data$Frequent==3,liu_data$footwear),])

 lm_3 = lm(vGRF~Sqrt.High,data=liu_data[c(liu_data$Frequent==1),])
 summary(lm_3)
 lm_3 = lm(vGRF~Sqrt.High,data=liu_data[c(liu_data$Frequent==2),])
 summary(lm_3)
 lm_3 = lm(vGRF~Sqrt.High~ vGRF,data=liu_data[c(liu_data$Frequent==3),])
 summary(lm_3)

lm_3 = lm(vGRF~Sqrt.High,data=liu_data)
 summary(lm_3)

 lm_3 = lm(Sqrt.High~ vGRF,data=liu_data[c(liu_data$Frequent==3,liu_data$footwear),])


a=c(1,1,1,0,2)


lm_3 = lm(Sqrt.High~vGRF,data=liu_data[c(liu_data$Frequent==1&liu_data$footwear==1&liu_data$distance==1&liu_data$surface==0&liu_data$gender==2),])

lm_3 = lm(Sqrt.High~vGRF,data=liu_data)
summary(lm_3)

qplot(Sqrt.High, vGRF, data = liu_data)+geom_smooth(method='lm',se=TRUE)




lm_3 = lm(Sqrt.High~vGRF,data=liu_data[c(liu_data$Frequent==1&liu_data$footwear==1&liu_data$distance==1&liu_data$surface==0&liu_data$gender==2),])
summary(lm_3)


lm_3 = lm(Sqrt.High~Frequent+footwear+distance+surface+gender,data=liu_data)

lm_3 = lm(Sqrt.High~Frequent+footwear+distance+surface+gender,data=liu_data[c(liu_data$Frequent==1,liu_data$footwear==1,liu_data$distance==1,liu_data$surface==0,liu_data$gender==2),])




b <- matrix(c(1,1,1,0,2,
2,1,1,1,1,
2,1,1,1,2,
2,1,1,1,3,
2,3,1,1,1,
2,3,1,1,2,
2,4,1,1,1,
3,2,1,1,1,
3,4,1,1,1), ncol=5, byrow=T)

for (i in 1:9){
lm = lm(vGRF~Sqrt.High,data=liu_data[c(liu_data$Frequent==b[i,1]&liu_data$footwear==b[i,2]&liu_data$distance==b[i,3]&liu_data$surface==b[i,4]&liu_data$gender==b[i,5]),])
summary(lm)

jpeg(paste("ÄâºÏm",i,".jpeg"))
qplot(Sqrt.High, vGRF, data = liu_data[c(liu_data$Frequent==b[i,1]&liu_data$footwear==b[i,2]&liu_data$distance==b[i,3]&liu_data$surface==b[i,4]&liu_data$gender==b[i,5]),])+geom_smooth(method='lm',se=TRUE)
dev.off()

}
lm = lm(Sqrt.High~vGRF,data=liu_data[c(liu_data$Frequent==b[i,1]&liu_data$footwear==b[i,2]&liu_data$distance==b[i,3]&liu_data$surface==b[i,4]&liu_data$gender==b[i,5]),])
summary(lm)
jpeg(paste("ÄâºÏ",i,".jpeg"))
qplot(Sqrt.High, vGRF, data = liu_data[c(liu_data$Frequent==b[i,1]&liu_data$footwear==b[i,2]&liu_data$distance==b[i,3]&liu_data$surface==b[i,4]&liu_data$gender==b[i,5]),])+geom_smooth(method='lm',se=TRUE)
dev.off()

lm_3 = lm(Sqrt.High~vGRF,data=liu_data[c(liu_data$Frequent==1&liu_data$footwear==1&liu_data$distance==1&liu_data$surface==0&liu_data$gender==2),])
summary(lm_3)


redata=read.table(file="regressOne.txt",header=TRUE)
row.names(redata) = c("11102","24111","32111","21113","21112","21111","23112","23111","34111")



df = data.frame(



qplot( High,Reciprocal.Time, data = TPGRF, colour = factor(Frequent))+geom_smooth(method='lm',se=TRUE)

qplot( Reciprocal.High,Time, data = TPGRF, colour = factor(Frequent))+geom_smooth(method='lm',se=TRUE)

qplot( Reciprocal.High,Time, data = TPGRF)+geom_smooth(method='lm',se=TRUE)
lm = lm(High~Reciprocal.Time,data=TPGRF[TPGRF$Frequent==1,])
summary(lm)

lm = lm(Reciprocal.High~Time,data=TPGRF[TPGRF$Frequent==2,])
summary(lm)

lm = lm(High~Reciprocal.Time,data=TPGRF[TPGRF$Frequent==3,])
summary(lm)
====================================================================
TPGRF=read.table(file="TPGRF.txt",header=TRUE)





lm = lm(Time~High,data=TPGRF)
summary(lm)

qplot( Reciprocal.High,Time, data = TPGRF)+geom_smooth(method='lm',se=TRUE)

lm = lm(Time~Reciprocal.High,data=TPGRF)
summary(lm)

qplot(Reciprocal.High,Time, data =  TPGRF, facets = .~Frequency)+geom_smooth(method='lm',se=TRUE)


lm_3 = lm(Time~Reciprocal.High,data= TPGRF[c(TPGRF$Frequency==1),])
summary(lm_3)

lm_3 = lm(Time~Reciprocal.High,data= TPGRF[c(TPGRF$Frequency==2),])
summary(lm_3)

lm_3 = lm(Time~Reciprocal.High,data= TPGRF[c(TPGRF$Frequency==3),])
summary(lm_3)
============================================
PL=read.table(file="p1-1.txt",header=TRUE)


tiff(file = "fig-4.tiff", res = 900, width = 4800, height = 3800) 
ggplot(PL, aes(DH, PvGRF1,linetype=Frequency))+ geom_point(aes(shape=Frequency1)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black")+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 6),xlim=c(0,105))+theme_classic()+opts(legend.position="bottom")
dev.off()

tiff(file = "fig-3.tiff", res = 900, width = 4800, height = 3800) 
ggplot(PL, aes(DH, PvGRF1,linetype=Frequency))+ geom_point(aes(shape=Frequency1)) + geom_smooth(method='lm',se=TRUE,size=1,color="black")+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 6),xlim=c(0,11))+theme_classic()+opts(legend.position="bottom")
dev.off()


qplot( Sqrt.High,High, data =PL)+geom_smooth(method='lm',se=TRUE)

lm_3 = lm(P1~Sqrt.High,data=PL)
summary(lm_3)

lm_3 = lm(P1~Square.High,data=PL)
summary(lm_3)


 lm_3 = lm(P1~Sqrt.High ,data=PL[PL$Frequent==1,])
summary(lm_3)

 lm_3 = lm(P1~Sqrt.High ,data=PL[PL$Frequent==2,])
summary(lm_3)

 lm_3 = lm(P1~Sqrt.High ,data=PL[PL$Frequent==3,])
summary(lm_3)

qplot(Sqrt.High,P1, data =  PL, facets = .~Frequent)+geom_smooth(method='lm',se=TRUE)

qplot(Square.High,P1, data =  PL, facets = .~Frequent)+geom_smooth(method='lm',se=TRUE)
===================================================
TPF1=read.table(file="TPF1.txt",header=TRUE)

lm_3 = lm(Time~Reciprocal.High,data=TPF1)
summary(lm_3)

qplot(Reciprocal.High,Time, data =TPF1)+geom_smooth(method='lm',se=TRUE)

ROL=read.table(file="ROL.txt",header=TRUE)

lm_3 = lm(ROL ~ X1.5_order.High+Sqrt.High,data=ROL[ROL$High==32,])
summary(lm_3)

qplot( Reciprocal.High,Time, data = TPGRF, colour = factor(Frequent))+geom_smooth(method='lm',se=TRUE)

kruskal.test(ROL~High,data=ROL)
boxplot(ROL~High,data=ROL,col = "lightgray",pch="1", cex=2,xlab = "High", ylab="ROL")
boxplot(ROL~High , data = data, col = "lightgray",pch="1", cex=2,xlab = "Time Point", ylab = name_col[3])

APGRF=read.table(file="APGRF.txt",header=TRUE)
lm_3 = lm(APGRF ~Horizontal_distance,data= APGRF)
summary(lm_3)

qplot( Horizontal_distance,APGRF, data = APGRF, colour = factor(Frequent))+geom_smooth(method='lm',se=TRUE)
qplot( Horizontal_distance,APGRF, data = APGRF)+geom_smooth(method='lm',se=TRUE)
kruskal.test(APGRF~Horizontal_distance,data=APGRF)
qplot( Horizontal_distance,APGRF, data = APGRF,)+boxplot()
qplot( Horizontal_distance,APGRF, data = APGRF,colour = factor(gender))+geom_smooth(method='lm',se=TRUE)

ggplot(APGRF, aes(Horizontal_distance,APGRF)) + geom_smooth(method='lm',se=TRUE)+geom_boxplot(aes(group = Horizontal_distance),)





clust = read.table("cluster.txt",header=TRUE)
clust
row.names(clust)=c(11102,24111,32111,21113,21112,21111,23112,23111,34111)
dist.r = dist(clust) 
hc.r = hclust(dist.r) 

tiff(file = "fig11.tiff", res = 900, width = 3800, height = 3400) 
plot(hc.r)
dev.off()

===============================
vGRF=read.table("niu-meta5.txt",header=TRUE,sep=',')
vGRF$Sqrt.Height=sqrt(vGRF$Height)
vGRF$SampleFrequency1=vGRF$SampleFrequency
vGRF$SampleFrequency=as.factor(vGRF$SampleFrequency)
vGRF$SampleFrequency1=as.factor(vGRF$SampleFrequency1)
tiff(file = "figure-1.tiff", res = 900, width = 4800, height = 3800) 
ggplot(vGRF, aes(Sqrt.Height, PvGRF))+ geom_point(aes(shape=SampleFrequency)) + geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=SampleFrequency1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 9),xlim=c(-1,12.5))+theme_classic()+theme(legend.position="bottom")
dev.off()

tiff(file = "figure-b.tiff", res = 900, width = 4800, height = 3800) 
ggplot(vGRF, aes(Height, PvGRF))+ geom_point(aes(shape=SampleFrequency)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black",aes(linetype=SampleFrequency1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 9),xlim=c(-1,140))+theme_classic()+theme(legend.position="bottom")
dev.off()

tiff(file = "figure-3.tiff", res = 900, width = 4800, height = 3800) 
ggplot(vGRF[vGRF$SampleFrequency==2,], aes(Height, PvGRF))+ geom_point(aes(shape=SampleFrequency)) + geom_smooth(method='lm',formula=y~I(sqrt(x)),se=TRUE,size=1,color="black",aes(linetype=SampleFrequency1))+scale_linetype_manual(values=c("solid","twodash", "dotted"))+theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+coord_cartesian(ylim = c(0, 9),xlim=c(0,150))+theme_classic()+theme(legend.position="bottom")
dev.off()
