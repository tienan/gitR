library(cummeRbund)
library("ggplot2")
library("gplots")
require(graphics); 
require(grDevices)

cuff <- readCufflinks('diffout/')
csDensity(genes(cuff))
csBoxplot(genes(cuff))
csScatter(genes(cuff))
csScatter(genes(cuff),c1,c2)
csScatter(genes(cuff),C1,C2)
csScatter(genes(cuff),"C1","C2")

isoform = read.table('diffout/isoform_exp.diff',header=TRUE)



t2g = read.table('Transcript2gene.txt',header=TRUE,sep='\t')
head(t2g)
fix(t2g)
result = merge(isoform,t2g)
head(result)
write.table(wenpingGene.txt)
write.table(result,wenpingGene.txt)
?write.table
write.table(result,file='wenpingGene.txt')
write.table(result,file='wenpingGene.txt',quote=FALSE,row.name=FALSE)

data=read.table('diffout/gene_exp.diff',header=TRUE)
m_data = data[data$value_1<2000,]
m_data = m_data[m_data$value_2<2000,]

data$sign=1
data[data$log2.fold_change.<0 & data$significant=="yes",]$sign=3
data[data$log2.fold_change.>0 & data$significant=="yes",]$sign=2
data$sign=as.factor(data$sign)
x = data[data$significant=="yes",c(8,9)]
x = as.matrix(x)

tiff(file = "wenping-figure-2.tif", res =300, width = 4000, height = 4400) 

heatmap.2(x, col=redgreen(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()

ggplot(m_data, aes(value_1,m_data$value_2))+ geom_point() 


ggplot(data, aes(log(value_1),log(value_2)))+ geom_point(aes(color=as.factor(sign),shape=as.factor(sign)),size=3)  

ggplot(data, aes(log(value_1),log(value_2)))+ geom_point(color="black",size=3)+
aes(shape = as.factor(sign)) +
geom_point(aes(colour = as.factor(sign)), size = 4) +
geom_point(colour="grey90", size = 1.5)

ggplot(data, aes(log(value_1),log(value_2)))+
geom_point(colour="black", size = 4.5) +
  geom_point(colour="pink", size = 4) 

tiff(file = "wenping-figure-1.tif", res =300, width = 4000, height = 4400) 

ggplot(data, aes(log(value_1/mean(value_1)),log(value_2/mean(value_2))))+ geom_point(aes(color=as.factor(sign),shape=as.factor(sign)))

dev.off()

heatmap.2(x)

data_krt=read.table('krt.txt',header=FALSE,sep="\t")

tail(krt[order(krt[,3]),],15)

data_1= tail(krt[order(krt[,4]),],15)

 p + geom_point()+geom_crossbar(limits, position=dodge, width=0.5)+theme(axis.text.x = element_text(size = 20))

myGene <- getGene(cuff, "XLOC_016386")


#===================== plot
data=read.table('diffout/gene_exp.diff',header=TRUE)
data$sign=1
data[data$log2.fold_change.<0 & data$significant=="yes",]$sign=3
data[data$log2.fold_change.>0 & data$significant=="yes",]$sign=2
data$sign=as.factor(data$sign)

plt <-ggplot()
# ggplot(data, aes(log(value_1),log(value_2)))+ geom_point(aes(color=as.factor(sign),shape=as.factor(sign)),size=1) 
colors <- c('black','red', 'green')
shape <-
for(i in seq(length(colors))){
plt <- plt +  geom_point(aes(log(value_1),log(value_2)), data=data[data$sign==i,], color=colors[i])
}
plt=plt+theme_classic()+coord_cartesian(ylim = c(-10, 15),xlim=c(-10,15))
print(plt)

ggplot(data, aes(log(value_1),log(value_2)))+ geom_point(aes(color=as.factor(sign)),size=3) +theme_classic()

krt = read.table("krt_tracing.txt",header=FALSE,sep='\t')

data=read.table("genes.fpkm_tracking.sort.genename",header=FALSE,sep='\t')

diff = read.table("gene_exp_diff.idsort.genename",head=FALSE,sep='\t')
x = diff[diff$V15=='yes',]
id = c(1:64)
 rownames = paste(id,x$V2,sep='-')
x=x[,c(9,10)]
 row.names(x)=rownames
x=as.matrix(x)
tiff(file = "wenping-figure-2.tif", res =300, width = 4000, height = 4400) 

heatmap.2(x, col=redgreen(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()

gene_diff= diffData(genes(cuff))
sig_gene_diff = subset(gene_diff,(significant=='yes'))
nrow(sig_gene_diff)
myGenes<-getGenes(cuff,'XLOC_000151')
-------------------------------------------------
library(vegan)
dat=read.table("geneName_ok.txt")
express = dat[,c(8,9)]
nrow(express)
id = c(1:11557)
 rownames = paste(id,dat$V2,sep='-')
 row.names(express)=rownames
quantile(express$V1)
quantile(express$V8)
quantile(as.numeric(express$V8))
head(express$V8)
head(express$V9)
?quantile
quantile(as.numeric(express$V8),c(0.05,0.975))
head(express)
express = dat[,c(9,10)]
 row.names(express)=rownames
history()

high_express = express[express$V9>500&express$V10>500,]

high_express=read.table("diffgene_express")
row.names(high_express)=high_express$V1
high_express=high_express[,c(2,3)]
high_express=scale(high_express)

d <- vegdist(high_express,"euclidean")
csin <- hclust(d, method="single")



plot(csin, hang=-1)
d <- vegdist(high_express)
csin <- hclust(d, methodad="single")
plot(csin, hang=-1)
rect.hclust(csin, 3)
rect.hclust(csin, 40)
rect.hclust(csin, 90)

==========================================

dat = read.table("./liushangfeng/sort_avg.txt")
dat_s = read.table("./liushangfeng/sort_name.txt")
a = 0
b = 0
r = 0
s = 0
for(i in 1:nrow(dat_s)){
	a = b + 1
	b = a + dat_s[i,1]-1
	r[i] = mean(dat[c(a:b),2])
	s[i] = mean(dat[c(a:b),3])
}
dat_1 = cbind(dat_s,r,s)
x = cbind(r,s)

row.names(x) = dat_1[,2]

dat = read.table("./liushangfeng/gene_express_full.txt")
row.names(dat) = dat[,1]
set.seed(1)
m.kmeans = kmeans(dat[,c(2,3)], 10)

#g_dat = log(g_dat[g_dat[,3]!=7,c(1,2)])

g_dat  = cbind(log(dat[,c(2,3)]),id=seq(nrow(dat)),m.kmeans$cluster)

g_dat =g_dat[g_dat[,4]!=8,]

g_dat$idsort <- g_dat$id[order(g_dat[,4])]
g_dat$idsort <- order(g_dat$idsort)



dfm <- melt(g_dat[,c(1,2,3,5)], id.vars=c("id", "idsort"))
tiff(file = "liu-cluster.tif", res =300, width = 1000, height = 1400, compression ="lzw") 

ggplot(dfm, aes(x=variable, y=idsort)) + geom_tile(aes(fill = value), colour = "transparent") +
  scale_fill_gradient(low = "green", high = "red", 
    breaks = c(1:9))
dev.off()
tiff(file = "2015-10-2liu-express.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
plt <-ggplot()+theme_classic()
plt +  geom_point(aes(V2,V3),data=g_dat)+
geom_smooth(aes(V2,V3),data=g_dat,method='lm',size=2,color='black',
formula=y~x,se=TRUE)+
theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))
dev.off()


dat_volcano =  read.table("./liushangfeng/volcano.txt",head=TRUE)

P.Value <- c(dat_volcano$p_value/10)
FC <- c(dat_volcano$log2.fold_change.)
df <- data.frame(P.Value, FC)
df$threshold = as.factor(abs(df$FC) > 1 & df$P.Value < 0.05)
 
##Construct the plot object
tiff(file = "2015-10-2liu-volcano.tif", res =300, width = 1000, height = 1400, compression ="lzw") 

g = ggplot(data=df, aes(x=FC, y=-log10(P.Value), colour=threshold)) +
 geom_point(alpha=1, size=2) +
 theme_classic() +theme(legend.position=c(.92,.9))+theme(legend.key = element_blank())+theme(legend.background = element_blank())+
  xlim(c(-5, 5)) + ylim(c(0, 5)) +
  xlab("log2 fold change") + ylab("-log10 p-value")
g

dev.off()





o<- order(g_dat[,4])
g_dat<- g_dat[o,]
dfm <- melt(g_dat, id.vars=c("id", "idsort"))

write.table(g_dat, file = "cluster", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)


dat[m.kmeans$cluster==7,]

tiff(file = "liu-d3.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( dat[m.kmeans$cluster==7,c(2,3)]))
plot(g, hang = -1,main = "Class 7",ylab = "Distance",frame.plot = FALSE, ann =  TRUE)
dev.off()

tiff(file = "liu-d8.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( dat[m.kmeans$cluster==6,c(2,3)]))
plot(g, hang = -1,main = "Class 6",ylab = "Distance",frame.plot = FALSE, ann =  TRUE)
dev.off()


tiff(file = "liu-d10.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( dat[m.kmeans$cluster==1,c(2,3)]))
plot(g, hang = -1,main = "Class 1",ylab = "Distance",frame.plot = FALSE, ann =  TRUE)
dev.off()






library(reshape2)
library(ggplot2)


library(pheatmap)

tiff(file = "liu-cluster-1.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
pheatmap(log(g_dat[g_dat[,3]!=2,c(1,2)]), xlab="Sample",  show_rownames=FALSE)
dev.off()


#heatmap.2(x, col=redgreen(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
km <- kmeans(x, 10)


pheatmap(log(m.kmeans[m.kmeans[,3]!=8,c(1,2)]))

tiff(file = "liu-d3.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( x[km$cluster==10,]))
plot(g, hang = -1,main = "Class 3",ylab = "Distance",frame.plot = FALSE, ann =  TRUE)
dev.off()"


tiff(file = "liu-d3.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( x[km$cluster==10,]))
plot(g, hang = -1,main = "Class 3",ylab = "Distance",frame.plot = FALSE, ann =  TRUE)
dev.off()"



tiff(file = "liu-d8.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( x[km$cluster==1,]))
plot(g, hang = -1,main = "Class 8")
dev.off()


tiff(file = "liu-d10.tif", res =300,width = 1000, height = 1400, compression ="lzw") 
g = hclust(dist( x[km$cluster==2,]))
plot(g, hang = -1,main = "Class 10")
dev.off()



g = hclust(dist( x[km$cluster==3,]))
plclust(g, hang = -1)

install.packages("adegenet", dep=TRUE)

library(adegenet)


g = hclust(dist( x[km$cluster==5,]))
plclust(g, hang = -1)



g = hclust(dist(x))
plclust(g, hang = -1)


g = hclust(dist( x[km$cluster==3,]))

g = hclust(dist( x[km$cluster==4,]))

s=0
for (i in 2:18){
	km <- kmeans(x,i)
	s[i-1] = 1-km$betweenss/km$totss
}

a = km$centers

b = a[km$size > 10 & km$size<100,]

abs(log(b[,1]/b[,2]))
a[c(3,8,10),]

d3 =  x[km$cluster==3,]

d8 =  x[km$cluster==8,]

d10 =  x[km$cluster==10,]


for(i in 1:length(unique(com$membership))){

subgroup[[i]] = as.character(sg[sg[, 2]== i, 1])}

rm(i)

library(igraph)

dat_i = read.table("./liushangfeng/full_interaction.txt")
dat_5=dat_i[,c(5,6)]
gg_1 = graph.data.frame(d = dat_5, directed = F)
com = walktrap.community(gg_1, steps = 1)
x_c = cbind(x,km$cluster)
x_c = as.data.frame(x[km$cluster!=1,])
x_c$names = row.names(x_c)
dat_6 = read.table("./liushangfeng/id_trans_d.txt",sep="\t",head=TRUE)

write.table(x,file='express.txt',quote=FALSE,row.name=TRUE)


merge(as.data.frame(com$names),dat_6,by.x = "com$names", by.y = "X.1")





bte=betweenness(gg_1, directed = F)

high = as.data.frame(bte[bte>21])
rownames(high)
high = cbind(rownames(high),high)
high_1 = merge(high,dat_6,by.x = "rownames(high)", by.y = "X.1")
name_x = cbind(x,rownames(x),km$cluster)

result = merge(high_1,name_x,by.x = "X", by.y = "name")


V(gg_1)$size = 8

V(gg_1)[bte>=21]$size = 15



V(gg_1)$label = NA
V(gg_1)[bte>=21]$label = 1

high = as.data.frame(bte[bte>21])
rownames(high)

name_x = cbind(x,rownames(x))



l <- layout.fruchterman.reingold(gg_1,niter=500,area=vcount(gg_1)^2.3,repulserad=vcount(gg_1)^2.8)

tiff(file = "liu-4.tif", res =600, width = 4000, height = 4400, compression ="lzw") 

plot(gg_1,layout = l,  vertex.label =as.integer(as.numeric(bte)), vertex.size = V(gg_1)$size, vertex.label = NA, edge.color = grey(0.5),

    edge.arrow.mode = "-")

dev.off()

plot(betweenness(gg_1, directed = F))


plot(gg, layout = layout.fruchterman.reingold, vertex.size = 10,

    vertex.color = V(gg)$color, vertex.label =com$names , edge.color = grey(0.5),

    edge.arrow.mode = "-")


plot(gg_1, layout = layout.fruchterman.reingold, vertex.size = 10, vertex.label = NA,

    edge.color = grey(0.5), edge.arrow.mode = "-")

tiff(file = "liu-1.tif", res =300, width = 2000, height = 2400, compression ="lzw") 
p = ggplot(dat, aes(log(V2),log(V3)))+ geom_point() + labs(x = "log(Sample_1)", y = "log(Sample_2)")  
p =  p+ theme(axis.text.y=element_text(size =18),axis.text.x=element_text(size =18), axis.title.y = element_text(size =25),axis.title.x = element_text(size =25),panel.background = element_blank())
p
dev.off()

tiff(file = "liu-3.1.tif", res =600, width = 4000, height = 4400, compression ="lzw") 

heatmap.2(m.kmeans[m.kmeans[,3]!=8,c(1,2)], xlab="Sample", cex.axis=19,col=redgreen(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()


x = cbind(log(dat$V2),log(dat$V3))


write.table(bte, file = "bte.txt", append = FALSE, quote = FALSE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = FALSE)

