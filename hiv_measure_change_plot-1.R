s1 = c(61,81,269,341,512)
s2 = c(80,120,366,519,541)
s3 = c(130,244,411,612,636)
s4 = c(265,289,999)
s5 = c(141,235,326)
time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="CD4+T(cell/ul)",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(0, 1000),
axes=FALSE,
cex=fontsize
)

title(xlab="Time point",
ylab="CD4+T(cell/ul)",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("100cell/ul","200cell/ul","300cell/ul","400cell/ul","500cell/ul","600cell/ul","700cell/ul","800cell/ul","900cell/ul","1000cell/ul"), at=c(100,200,300,400,500,600,700,800,900,1000), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(1, 995, legend=c("P1", "P2","P3","P4","P5"),col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)

##=====================
s1 =c(228,318,368,512,825)
s2 =c(529,621,686,648,796)
s3 =c(256,362,418,478,639)
s4 =c(324,649,1886)
s5 =c(312,462,542)
time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="CD8+ T(cell/ul)",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(0, 2000),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="CD8+T(cell/ul)",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("100cell/ul","200cell/ul","300cell/ul","400cell/ul","500cell/ul","600cell/ul","700cell/ul","800cell/ul","900cell/ul","1000cell/ul","1100cell/ul","1200cell/ul","1300cell/ul","1400cell/ul","1500cell/ul","1600cell/ul","1700cell/ul","1800cell/ul","1900cell/ul","2000cell/ul"), at=c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(1,1995, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)


##======================

s1=c(17,15.2,14,14.2,13.6)
s2=c(19.1,14.8,13.6,14,14.1)
s3=c(17.9,16,14.4,13.8,14.2)
s4=c(18.3,14.1,14.6)
s5=c(22.1,17.9,16.2)
time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="Change of clot enzyme(sec)",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(10, 25),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="Change of clot enzyme(sec)",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("10s","15s","20s","25s"), at=c(10,15,20,25), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,24.4, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)


##===================
s1 = c(30.8,34.1,34.6,38.6,40.1)
s2 = c(33.5,35.6,36.8,38.8,38.9)
s3 = c(27.4,38.2,37.9,38.1,38.8)
s4 = c(36.3,36.8,38.6)
s5 = c(31.3,28.6,34.2)
time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="Protein(g/L)",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(25, 42),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="Protein(g/L)",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("25g/L","30g/L","35g/L","40g/L","42g/L"), at=c(25,30,35,40,42), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,33.2, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)


##=======================

s1 =c(12.9,13.6,16.4,16.1,15.2)
s2 =c(48.2,38.6,31.6,28.4,26.4)
s3 = c(56.9,38.9,32.4,26.8,31.6)
s4 = c(16.7,18.9,18.1)
s5 = c(57.5,65.5,42.6)

time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="Bilirubin(umol/L)",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(10, 70),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="Bilirubin(umol/L)",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("10umol/L","20umol/L","30umol/L","40umol/L","50umol/L","60umol/L"), at=c(10,20,30,40,50,60), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,69.5, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)

%%=========================

s1=c(2.1,6.6,5.4,5.1,6.2)
s2=c(1.89,7.6,6.6,6.4,5.4)
s3=c(2.14,3.9,5.4,5.8,5.6)
s4=c(2,8.9,7.1)
s5=c(2.54,11.4,8.6)


time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="WBC£¨¡Á10e9/L£©",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(1, 15),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="WBC£¨¡Á10e9/L£©",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("2¡Á10e9/L","8¡Á10e9/L","15¡Á10e9/L"), at=c(2,8,15), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,14.9, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)


##===========

s1=c(36,306,286,254.6,268)
s2=c(26.7,296,326.4,284,246)
s3=c(101,412,346,264.6,289)
s4=c(56,419,427)
s5=c(28,177,286)


time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="Platelet£¨¡Á10e9/L£©",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(0, 500),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="Platelet£¨¡Á10e9/L£©",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("0¡Á10e9/L","100¡Á10e9/L","200¡Á10e9/L","300¡Á10e9/L","400¡Á10e9/L","500¡Á10e9/L"), at=c(0,100,200,300,400,500), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,220.9, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)


##===============
s1=c(107,109,114,126,132)
s2=c(88.6,102.6,129,132,112)
s3=c(63.3,89.4,121,131,134)
s4=c(55,94.2,118)
s5=c(87,109,112)
 


time_name = c("Before operation","1-month after operation", "3-month after operation", "12-month after operation","24-month after operation" )
times=c(1,2,3,4,5) 
times_1=c(1,2,3) 
fontsize=1.2
plot(times,s1,
xlab="Time point",ylab="BRP£¨g/L£©",
type="n",
bty="o",
xlim=c(0.99, 5.01), ylim = c(50, 150),
axes=FALSE,
cex=fontsize
)
title(xlab="Time point",
ylab="BRP£¨g/L£©",cex=fontsize)
lines(times,s1, lty=1, lwd=1.5, type="b",pch=1,col=1)
lines(times,s2, lty=1, lwd=1.5, type="b",pch=2,col=26)
lines(times,s3, lty=1, lwd=1.5, type="b",pch=3,col=51)
lines(times_1,s4, lty=1, lwd=1.5, type="b",pch=4,col=76)
lines(times_1,s5, lty=1, lwd=1.5, type="b",pch=5,col=101)
axis(1, labels=time_name, at=times,cex.axis=fontsize)
axis(2, labels=c("50£¨g/L£©","100£¨g/L£©","150£¨g/L£©"), at=c(50,100,150), las=3,cex.axis=fontsize)
box(which="plot", lty=1, lwd=1.5, bty="o")
legend(4,100, legend=c("P1", "P2","P3","P4","P5"), col=c(1,26,51,76,101),pch=1:5, lty=1,title="Patients",box.col = "white",bg = "white",cex=fontsize)



