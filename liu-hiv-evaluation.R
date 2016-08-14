library(ROCR)
library(pROC)
dat = read.table("liu-hiv-evaluation.txt",head = TRUE, sep="\t")
h1 = na.omit(dat[dat$Hospital==1,])
h2 = na.omit(dat[dat$Hospital==2,])

fm <- glm(Sepsis ~ C1+C2+C3+C4+C5, family = binomial(link = logit),data=dat[dat$Hospital==1,]) 
pre<-predict(fm, na.omit(h1[,c(2:6)]))
p<-exp(pre)/(1+exp(pre))

roc2 <- roc(h2$Sepsis,p,
plot=TRUE, add=TRUE, percent=roc1$percent)



m=prediction(p,h1$Sepsis)
plot(performance(m,'tpr','fpr'))
abline(0,1, lty = 8, col = "grey")

performance(m, "auc")
performance(m,'tpr','fpr')


pre<-predict(fm, na.omit(h2[,c(2:6)]))
p<-exp(pre)/(1+exp(pre))
m=prediction(p,h2$Sepsis)
plot(performance(m,'tpr','fpr'))
abline(0,1, lty = 8, col = "grey")

perf <- performance(m, "tpr")

plot(perf, avg= "vertical", 
     spread.estimate="boxplot", 
print.cutoffs.at=seq(0,1,by=0.1), 
text.adj=c(1.2,1.2), avg="threshold", lwd=3,
     show.spread.at= seq(0.1, 0.9, by=0.1))

perf <- performance(m,'ppv','npv')
perf <- performance(m,'sens','spec')
perf <- performance(m,'acc')
plot(perf, colorize=T,,xaxt="n", print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(1.2,1.2), avg="threshold", lwd=3)

rocobj <- roc(h2$Sepsis,p)
ci(rocobj)

ci.se.obj <- ci(rocobj, of="se", boot.n=500)
plot(ci.se.obj)

tiff(file = "liu_fig-2.tiff", res =300, width = 4500, height = 3000,compression ="lzw") 
plot.roc(rocobj, add=FALSE, reuse.auc=TRUE,
axes=TRUE, legacy.axes=TRUE,  cex.axis = 2,cex.lab=0.2,
xaxs="i"
)


dev.off()

