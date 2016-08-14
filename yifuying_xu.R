library("ggplot2")
library(splines)
library(grid)

dat = read.table("xu_1.txt",header=TRUE,sep="\t")
qplot(Week,Single, data = dat, geom = c("point", "smooth"),method = "lm")


qplot( geom = c("point", "smooth"),method = "lm", formula = y ~ ns(x, 7))

ggplot(dat, aes(Week,Single))+
geom_point(aes(shape=as.factor(group)))+
geom_smooth(aes(color=as.factor(group)),method = "lm", formula = y ~ ns(x, 7), se= F,size = 2)+
theme_classic()+scale_x_continuous(breaks = c(13:27))+
theme(axis.text.x=element_text(size=15,hjust=0),axis.text.y=element_text(size=15))     
 
qplot( geom = c("point", "smooth"),method = "lm", formula = y ~ ns(x, 7))


tiff(file = "xu_fig-1.1.tiff", res =300, width = 4000, height = 3400) 
ggplot(dat, aes(Week,TSH))+
#geom_point(aes(shape=as.factor(Percentage)))+
scale_linetype_manual(values=c(1,1,1,2,2,2))+
geom_smooth(aes(linetype=as.factor(Percentage)),method = "lm", formula = y ~ ns(x, 7), se= F,size = 1.5)+
theme_classic()+scale_x_continuous(breaks = c(13:27))+scale_y_continuous(breaks = c(1:5))+
theme(axis.text.x=element_text(size=20,hjust=0),axis.text.y=element_text(size=20))+
theme(legend.text = element_text(size=20),legend.key.height=unit(3,"line"))
  
dev.off()

dat = read.table("xu_2.txt",header=TRUE,sep="\t")
tiff(file = "xu_fig-2.tiff", res =300, width = 4000, height = 3400) 
ggplot(dat, aes(Weeks,FT4))+
#geom_point(aes(shape=as.factor(Percentage)))+
#scale_linetype_manual(values=c(1,1,1,2,2,2))+
geom_smooth(aes(linetype=as.factor(Twins)),method = "lm", formula = y ~ ns(x, 7), se= F,size = 1.5)+
theme_classic()+scale_x_continuous(breaks = c(13:27))+scale_y_continuous(limits=c(0,2))+
theme(axis.text.x=element_text(size=20,hjust=0),axis.text.y=element_text(size=20))+
theme(legend.text = element_text(size=20),legend.key.height=unit(3,"line"))
 dev.off() 
 

dat = read.table("xu_3.txt",header=TRUE,sep="\t")
tiff(file = "xu_fig-3.tiff", res =300, width = 4000, height = 3400) 
ggplot(dat, aes(Weeks,TSH))+
#geom_point(aes(shape=as.factor(Percentage)))+
scale_linetype_manual(values=c(1,1,2,2))+
geom_smooth(aes(linetype=as.factor(Percentage)),method = "lm", formula = y ~ ns(x, 7), se= F,size = 1.5)+
theme_classic()+scale_x_continuous(breaks = c(13:27))+scale_y_continuous(limits=c(0,2))+
theme(axis.text.x=element_text(size=20,hjust=0),axis.text.y=element_text(size=20))+
theme(legend.text = element_text(size=20),legend.key.height=unit(3,"line"))
dev.off() 

dat = read.table("xu_4.txt",header=TRUE,sep="\t")
ggplot(dat, aes( TPOAb, TSH))+
geom_point()+
#scale_linetype_manual(values=c(1,1,2,2))+
geom_smooth(method = "lm",  se= T,size = 1.5)+
theme_classic()+
#scale_x_continuous(limits = c(0,3500))+
#scale_x_continuous(breaks = c(13:27))+scale_y_continuous(limits=c(0,2))+
theme(axis.text.x=element_text(size=20,hjust=0),axis.text.y=element_text(size=20))+
theme(legend.text = element_text(size=20),legend.key.height=unit(3,"line"))


dat = read.table("xu_4.txt",header=TRUE,sep="\t")
ggplot(dat, aes( TPOAb,FT4))+
geom_point()+
#scale_linetype_manual(values=c(1,1,2,2))+
geom_smooth(method = "lm",  se= T,size = 1.5)+
theme_classic()+
#scale_x_continuous(limits = c(0,3500))+
#scale_x_continuous(breaks = c(13:27))+scale_y_continuous(limits=c(0,2))+
theme(axis.text.x=element_text(size=20,hjust=0),axis.text.y=element_text(size=20))+
theme(legend.text = element_text(size=20),legend.key.height=unit(3,"line"))



a=sort(data);
l=a[0.05*length(a)];
u=a[0.95*length(a)];
result=a[a>l&a<u]	a = sort(df[,i],index.return=TRUE)
	l=floor(0.025*length(a$ix));
	u=ceiling(0.975*length(a$ix));
	logic = a$ix>l&a$ix<u	
	index = a$ix
	df=df[index,];;

   
