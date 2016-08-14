par(mfrow=c(5,7),oma=c(2,2,2,1), mar=rep(1, 4))
for (i in 1:35){
boxplot(V1 ~ V2, data = pdata[((i-1)*200+1):(i*200),], col = "lightgray",pch="1", cex=0.6,xlab = "Years", ylab = "RPN SCORE",ylim = c(0, 120))
}

boxplot(V1 ~ V2, data = t1, col = "lightgray",pch="1", cex=0.6,xlab = "Years", ylab = "RPN SCORE")


par(mfrow=c(2,2),oma=c(2,2,2,1), mar=rep(1, 4))
m=c(1:4)
for (i in 1:4){
boxplot(V1 ~ V2, data = pdata,
        main = "2008~2011 POSSUM SCORE",
		
        xlab = "Years",
		col = "yellow",
        ylab = "POSSUM SCORE")
}

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length")