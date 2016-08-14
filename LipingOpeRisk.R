dat =  read.table("LipingOpeRisk.txt",header=F)


ggplot(vGRF, aes(Sqrt.Height, PvGRF))+ 
geom_point(aes(shape=SampleFrequency)) + 
geom_smooth(method='lm',se=TRUE,size=1,color="black",aes(linetype=SampleFrequency1))+
scale_linetype_manual(values=c("solid","twodash", "dotted"))+
theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))