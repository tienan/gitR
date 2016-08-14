HIV=read.table(file="LiuHIV.txt",header=TRUE,seq=)

HIV=read.csv(file="LiuHIV.txt", header = TRUE, sep = ",")

kruskal.test( CD4_min~Gender, data=HIV)
boxplot(CD4_min ~ Classification, data=HIV)

ggplot(HIV, aes(Gender,CD4_min)) +geom_boxplot(aes(group =Gender),)


dat = read.table("./liubaochi/liubaochi-risk-2015-8-26.txt",sep="\t",header=TRUE)

cutoff = c(1:16)/2+6

f1=c()
f2=c()

for (i in 1:length(cutoff)){
	f1[i]=nrow(dat[dat$Sepsis==1&dat$Sum<cutoff[i],])
	f2[i]=nrow(dat[dat$Sepsis==2&dat$Sum>=cutoff[i],])
}

r = cbind(f1,f2,cutoff)

write.table(r,file="Prediction.txt",quote = FALSE, sep = "\t",row.names = FALSE,col.names = FALSE)