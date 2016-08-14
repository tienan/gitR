#Cd$
color_set
s1 = c(61,81,269,341,512)
s2 = c(80,120,366,519,541)
s3 = c(130,244,411,612,636)
s4 = c(265,289,999)
s5 = c(141,235,326)
time = c("Before operation","1 month after operation", "3 month after operation", "12 month after operation","24 month after operation" )
matplot(c(0.5,5.5),c(0,1000),type="n",xlab="Time point",ylab="CD4+T(cell/ul)",axes=TRUE)



scores1 = c(61,81,269,341,512)
scores2 = c(80,120,366,519,541)
times = c(1,2,3,4,5)


matplot(c(0.5,5.5),type="n",xlab="Time point",ylab="Scores",main = "Accuracy of the diagnosis")


plot(time, scores1,
type="n",
bty="o",
xlim=c(-0.5, 5.5), ylim = c(0, 1000),
axes=FALSE,
ann=TRUE,
frame.plot=TRUE
)
time = c(1,2,3,4,5)
barplot(rbind(nr.prof))


lines(time,scores1, lty=1, lwd=1.5,type="b",pch=2)
axis(1, labels=c("200mg", "250mg", "300mg", "350mg"), at=c(200, 250, 300, 350))



plot(TC1, TC2-TC1,
type="n",
bty="o",
xlim=c(200, 350), ylim = c(-100, 50),
axes=FALSE,
ann=FALSE,
frame.plot=FALSE
)