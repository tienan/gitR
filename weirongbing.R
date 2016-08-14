library(cummeRbund)
library("ggplot2")
library("gplots")
require(graphics); 
require(grDevices)
library(pheatmap)


dat = read.table("wen_1.txt",header=TRUE)
plt <-ggplot()+theme_classic()
plt +  geom_point(aes(value_1,value_2),data=dat[dat[,2]<800&dat[,3]<800,])+
geom_smooth(aes(value_1,value_2),data=dat[dat[,2]<850&dat[,3]<850,],method='lm',size=2,color='black',
formula=y~x,se=TRUE)+
theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))

tiff(file = "wei-1.tif", res =400, width = 3500, height = 3500, compression ="lzw") 
plt +  geom_point(aes(value_1,value_2),data=dat[dat[,2]<800&dat[,3]<800,])+
geom_smooth(aes(value_1,value_2),data=dat[dat[,2]<850&dat[,3]<850,],method='lm',size=2,color='black',
formula=y~x,se=TRUE)+
theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))

dev.off()
dat = read.table("wei_2.txt",header=TRUE)
data=dat[dat[,2]<800&dat[,3]<800,]

=================cluster
tiff(file = "wei-2.tif", res =400, width = 3500, height = 4500, compression ="lzw") 
pheatmap(scale(dat[dat[,2]<850&dat[,3]<850,c(2,3)]), treeheight_row=150,cellwidth = 90, cellheight = 0.5,xlab="Sample", color = colorRampPalette(c("red", "green", "firebrick3"))(100), show_rownames=FALSE)
dev.off()

heatmap.2( scale="row",scale(dat[dat[,2]<850&dat[,3]<850,c(2,3)]), key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=2,cexCol=1)


color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), 


 ggplot(dat[dat[,2]<850&dat[,3]<850,c(2,3)], aes(value_1,value_2))+ geom_tile(aes(fill = value),

  colour ="white")+ scale_fill_gradient(low ="yellow",

 high ="red")

