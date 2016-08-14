cr = c(0.948888445,0.850767766,0.884047785,0.932627937,0.841757034,0.762777336,0.61479312,0.859132655,0.762777336,0.323549732,0.22406515,0.249264806,0.299714312,0.218040475,0.174595794,0.118034739,0.229929653,0.174595794)
rr = c(-0.041302009,-0.026420161,-0.029924682,-0.037465623,-0.025605884,-0.019982723,-0.013249649,-0.027221342,-0.019982723,-0.005429116,-0.003523427,-0.003981976,-0.004948151,-0.003416004,-0.002665029,-0.001744481,-0.003628797,-0.002665029)

x = 0:10*7.2+72
y = exp(rr[1]*x/2)
fontsize=1.2
plot(x/2,y,
xlab="Time(Hour)",ylab="Change of casualty rate",
type="n",
bty="o",
xlim=c(0,75), ylim = c(0,1),
cex=fontsize
)
box(which="plot", lty=1, lwd=1.5, bty="o")
for(i in 1:9){
y = exp(rr[i]*x/2)
lines(x-72,y, lty=1, lwd=1.5, type="b",pch=i,col=1+(i-1)*5)
end
}

names = c("Gravel structure" ,"Adobe structure" ,"Stone structure" ,"Brick structure" ,"Wooden framework (different infill walls)" ,"Wooden frame (good infill walls)" ,"Wooden frame (wood panelled walls)" ,"Low-quality reinforced concrete frame" ,"Low-quality reinforced concrete shear walls")

par(mfrow=c(3,3),oma=c(2,2,2,1))
for (i in 10:18){
y = exp(rr[i]*x/2)
plot(x-72,y, lty=1, lwd=2, type="b",pch=i,col=1,xlab="Time(Hour)",ylab="Change of casualty rate",
xlim=c(0,75), ylim = c(0.5,1),
cex=1.5,cex.axis=1,cex.lab=2,cex.main=2,main =  paste(names[i-9],"(r=",round(rr[i],3),")")
)
text(rr[i])
}

