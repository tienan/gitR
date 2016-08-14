# evolution
require(graphics); require(grDevices)
library("gplots")
eva_gene=read.table("evaluation_gene.txt",header=TRUE)

row.names(eva_gene)=eva_gene$Sample

x=eva_gene[,2:8]
x  <- as.matrix(x)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-1,0,length=100),  # for red
  seq(0,0.8,length=100),              # for yellow
  seq(0.8,1,length=100))              # for green


heatmap.2(x, cexRow=2,cexCol=2,densadj=0.5,
	
  cellnote = x,  # same data set for cell labels
#  main = "Correlation", # heat map title
  notecol="black",      # change font color of cell labels to black
  density.info="none",  # turns off density plot inside color legend
  trace="none",         # turns off trace lines inside the heat map
  margins =c(12,9),     # widens margins around plot
  col=my_palette,       # use on color palette defined earlier 
  breaks=col_breaks,    # enable color transition at specified limits
  ColSideColors=cc,
  #dendrogram="row"    # only draw a row dendrogram
  #Colv="NA"
  #scale="row"          # ¡À¨º?¡¥¡ä|¨¤¨ª
	
)
tiff(file = "fig-wu.tiff", res =300, width = 4000, height = 4400) 

heatmap.2(x, col=redgreen(75), scale="row",key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1.2,cexCol=1.2)
dev.off()

tiff(file = "fig-wu.tiff", res =300, width = 4000, height = 4400) 
heatmap.2(x, col=heat.colors(100), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()

dat = read.table("wu-epic.txt",header=TRUE,sep="\t")
row.names(dat)=dat$X1
dat = dat[,2:length(dat)]
x  <- as.matrix(dat)
tiff(file = "fig-wu-1.tiff", res =300, width = 4000, height = 4400) 
heatmap.2(x, col=heat.colors(100), scale="row",  key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()

dat = read.table("wu-epic-1.txt",header=TRUE,sep="\t")
row.names(dat)=dat$X1
dat = dat[,2:length(dat)]
x  <- as.matrix(dat)
tiff(file = "fig-wu-2.tiff", res =300, width = 4000, height = 4400) 
heatmap.2(x, col=heat.colors(100), scale="row",  key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=1,cexCol=1)
dev.off()



