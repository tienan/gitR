
## Accuracy of the image description
scores1 = drilling[c(1,4,8,12,16),1]
scores2 = drilling[c(19,23,27,31,35),1]
jpeg("Accuracyoftheimagedescription.jpeg")
matplot(c(0.5,5.5),c(0.3,0.9),type="n",xlab="Time point",ylab="Scores",main = "Accuracy of the image description")
lines(time,scores2,type="b",pch=1)
lines(time,scores1,type="b",pch=2)
c6 <- terrain.colors(10)[1:6]
legend(3.5,0.4,legend=c("drilling group","control group"),lty = 1,pch=1:2,box.lwd=0,
lwd = 0,box.col = "white",bg = "white")

legend(0.4,0.4,legend=c("d = Cohen's effect size","* p<0.05, **p<0.01"),box.col = "white",bg = "white")

textset=c(expression(d["2-1"]*"=0.5*"),expression(d["2-1"]*"=0.72**"),
expression(d["3-2"]*"=0.18*"),expression(d["3-2"]*"=0.67**"),
expression(d["4-3"]*"=0.72*"),expression(d["4-3"]*"=0.86**"),
expression(d["5-4"]*"=-0.07"),expression(d["5-4"]*"=-0.75**"))
j=2
k=1
for (i in 1:4){
text(time[j],scores2[j]+0.05,textset[k])
k=k+1
text(time[j],scores1[j]-0.05,textset[k])
j=j+1
k=k+1
}
dev.off()

## Norm of writing
scores1 = drilling[c(1,4,8,12,16),2]
scores2 = drilling[c(19,23,27,31,35),2]
jpeg("normofwriting.jpeg")
matplot(c(0.5,5.5),c(0.3,0.95),type="n",xlab="Time point",ylab="Scores",main = "Norm of writing")
lines(time,scores2,type="b",pch=1)
lines(time,scores1,type="b",pch=2)
c6 <- terrain.colors(10)[1:6]
legend(3.5,0.4,legend=c("drilling group","control group"),lty = 1,pch=1:2,box.lwd=0,
lwd = 0,box.col = "white",bg = "white")
legend(0.4,0.4,legend=c("d = Cohen's effect size","* p<0.05, **p<0.01"),box.col = "white",bg = "white")

textset=c(expression(d["2-1"]*"=1.43**"),expression(d["2-1"]*"=0.82**"),
expression(d["3-2"]*"=-0.25"),expression(d["3-2"]*"=0.81**"),
expression(d["4-3"]*"=0.16"),expression(d["4-3"]*"=0.70**"),
expression(d["5-4"]*"=0.16"),expression(d["5-4"]*"=-0.77**"))
j=2
k=1
for (i in 1:4){
text(time[j],scores2[j]+0.05,textset[k])
k=k+1
text(time[j],scores1[j]-0.05,textset[k])
j=j+1
k=k+1
}
dev.off()

##Accuracy of the diagnosis

scores1 = drilling[c(1,4,8,12,16),3]
scores2 = drilling[c(19,23,27,31,35),3]
jpeg("Accuracyofthediagnosis.jpeg")
matplot(c(0.5,5.5),c(-0.1,0.85),type="n",xlab="Time point",ylab="Scores",main = "Accuracy of the diagnosis")
lines(time,scores2,type="b",pch=1)
lines(time,scores1,type="b",pch=2)
c6 <- terrain.colors(10)[1:6]
legend(3.5,0.1,legend=c("drilling group","control group"),lty = 1,pch=1:2,box.lwd=0,
lwd = 0,box.col = "white",bg = "white")
legend(0.4,0.1,legend=c("d = Cohen's effect size","* p<0.05, **p<0.01"),box.col = "white",bg = "white")

textset=c(expression(d["2-1"]*"=0.11"),expression(d["2-1"]*"=2.26**"),
expression(d["3-2"]*"=1.96**"),expression(d["3-2"]*"=2.03**"),
expression(d["4-3"]*"=1.13**"),expression(d["4-3"]*"=2.02**"),
expression(d["5-4"]*"=0.94**"),expression(d["5-4"]*"=-1.14**"))

text(time[2],scores2[2]-0.05,textset[1])
text(time[2],scores1[2]+0.05,textset[2])
j=3
k=3
for (i in 2:4){
text(time[j],scores2[j]+0.05,textset[k])
k=k+1
text(time[j],scores1[j]-0.05,textset[k])
j=j+1
k=k+1
}
dev.off()

## Right subsequent treatment

scores1 = drilling[c(1,4,8,12,16),4]
scores2 = drilling[c(19,23,27,31,35),4]
jpeg("Rightsubsequenttreatment.jpeg")
matplot(c(0.5,5.5),c(0.2,0.9),type="n",xlab="Time point",ylab="Scores",main = "Right subsequent treatment")
lines(time,scores2,type="b",pch=1)
lines(time,scores1,type="b",pch=2)
c6 <- terrain.colors(10)[1:6]
legend(3.5,0.3,legend=c("drilling group","control group"),lty = 1,pch=1:2,box.lwd=0,
lwd = 0,box.col = "white",bg = "white")
legend(0.4,0.3,legend=c("d = Cohen's effect size","* p<0.05, **p<0.01"),box.col = "white",bg = "white")

textset=c(expression(d["2-1"]*"=0.75"),expression(d["2-1"]*"=1.79**"),
expression(d["3-2"]*"=1.47**"),expression(d["3-2"]*"=1.73**"),
expression(d["4-3"]*"=0.34"),expression(d["4-3"]*"=1.63**"),
expression(d["5-4"]*"=0.46"),expression(d["5-4"]*"=-1.44**"))

text(time[2],scores2[2]-0.05,textset[1])
text(time[2],scores1[2]+0.05,textset[2])
j=3
k=3
for (i in 2:4){
text(time[j],scores2[j]+0.05,textset[k])
k=k+1
text(time[j],scores1[j]-0.05,textset[k])
j=j+1
k=k+1
}
dev.off()


d["2-1"]*"=1.23"

text(2,5,expression(''^15*N), col='red')