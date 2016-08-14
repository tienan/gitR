 library("ggplot2")
data = read.table("masu.txt",sep="\t")
cl = kmeans(data[,c(2,3)], 3)
plot(data[,c(2,3)], col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8, cex=2)
require(plotrix)
require(grid)
draw.circle( 0, 0, .5 )
tiff(file = "modify-masu-figure-2.tif", res =300, width = 2000, height = 2400) 

#V2?a¡Á¡§?¨°

ggplot(data, aes(Expert,OR))+geom_smooth(method='lm',se=TRUE)+geom_point(shape=as.factor(cl$cluster),size=3) + theme_classic()+theme(legend.position="bottom") 
dev.off()
