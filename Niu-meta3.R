2#Niu-3,|?????′?????????━??2??????━????????━??y???meta??????：??????：?????
library('metafor')
library('meta')

data=read.table('niu-meta-20150309.txt',header=FALSE,sep='\t')



meta1 <- metabin(event.e, n.e, event.c, n.c,study,data=dat, sm="RR", method="I")

tiff(file = "li-fig.3.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()




##data = data[c(3:34,37:44),]
##############
dat = data[,c(1,2,2,3,4,5,6)]
#dat = data[c(1:25,27:38),c(1,2,2,3,4,5,6)]

dat = na.omit(dat)
colnames(dat) = c("Study","n.c","n.e","mean.c", "sd.c","mean.e", "sd.e")


meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="SMD")
metabias(meta1,method="mm")

tiff(file = "li-fig.3.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()

tiff(file = "fig-PF2-funnel-0309.tiff", res =300,width = 1800, height = 1400,compression="lzw") 
funnel(meta1, contour.levels=c(0.9, 0.95, 0.99))
dev.off()

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="MD")
tiff(file = "fig-PF2-forest-0309-md.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()


#metabias(meta1,method="mm")
summary(meta1)

#######################
dat = data[,c(1,2,2,7,8,9,10)]
dat = na.omit(dat)
colnames(dat) = c("Study","n.c","n.e","mean.c", "sd.c","mean.e", "sd.e")

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="SMD")

tiff(file = "fig-TPF2-forest-1222-smd.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()
tiff(file = "fig-TPF2-funnel-1222.tiff", res =300,width = 1800, height = 1400,compression="lzw") 
funnel(meta1, contour.levels=c(0.9, 0.95, 0.99))
dev.off()
metabias(meta1,method="mm")
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="MD")

tiff(file = "fig-TPF2-forest-1222-md.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()




summary(meta1)


###########################


dat = data[,c(1,2,2,11,12,13,14)]
dat = na.omit(dat)
colnames(dat) = c("Study","n.c","n.e","mean.c", "sd.c","mean.e", "sd.e")

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="SMD")

tiff(file = "fig-PF1-forest-1222-smd.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()
tiff(file = "fig-PF1-funnel-1222.tiff", res =300,width = 1800, height = 1400,compression="lzw") 

funnel(meta1, contour.levels=c(0.9, 0.95, 0.99))
dev.off()
metabias(meta1,method="mm")
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="MD")

tiff(file = "fig-PF1-forest-1222-md.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()




summary(meta1)


#############################

dat = data[,c(1,2,2,15,16,17,18)]
dat = na.omit(dat)
colnames(dat) = c("Study","n.c","n.e","mean.c", "sd.c","mean.e", "sd.e")

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="SMD")

tiff(file = "fig-TPF1-forest-1222-smd.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()
tiff(file = "fig-TPF1-funnel-1222.tiff", res =300,width = 1800, height = 1400,compression="lzw") 
funnel(meta1, contour.levels=c(0.9, 0.95, 0.99))
dev.off()
metabias(meta1,method="mm")
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=dat, sm="MD")

tiff(file = "fig-TPF1-forest-1222.tiff-md.tiff", res =300, width = 4000, height = 4400,compression="lzw") 
forest(meta1)
dev.off()



summary(meta1)


========================================

data=read.table('Niu-3txt.txt',header=TRUE,sep=',')

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='PF2'&!is.na(data$mean.c),], sm="SMD")


tiff(file = "fig-PF2-forest.tiff", res =300, width = 4000, height = 4400) 
forest(meta1)
dev.off()


tiff(file = "fig-PF2-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1, contour.levels=c(0.9, 0.95, 0.99))
dev.off()





#For the summary measure "SMD", Hedges???????━?o?????━?????━? adjusted g is utilised for pooling.
#metabias(meta1f,method="linreg")
metabias(meta1,method="mm")

#======================
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='TPF2'&!is.na(data$mean.c),], sm="SMD")


tiff(file = "fig-TPF2-forest.tiff", res =300, width = 4000, height = 4400) 
forest(meta1)
dev.off()

tiff(file = "fig-TPF2-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
#For the summary measure "SMD", Hedges???????━?o?????━?????━? adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")


#======================
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='TPF1'&!is.na(data$mean.c),], sm="SMD")


tiff(file = "fig-TPF1-forest.tiff", res =300, width = 4000, height = 4400) 
forest(meta1)
dev.off()

tiff(file = "fig-TPF1-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
#For the summary measure "SMD", Hedges???????━?o?????━?????━? adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")

#======================
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='PF1'&!is.na(data$mean.c),], sm="SMD")


tiff(file = "fig-PF1-forest.tiff", res =300, width = 4000, height = 4400) 
forest(meta1)
dev.off()

tiff(file = "fig-PF1-funnel.tiff", res =300,width = 1800, height = 1400) 
fun6nel(meta1)
dev.off()
#For the summary measure "SMD", Hedges???????━?o?????━?????━? adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")



