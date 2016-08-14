#Niu-2,|¨¬¡§2?t???meta?¡è???
library('metafor')
library('meta')
data=read.table('Niu-2.txt',header=TRUE,sep=',')

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='PVGRF'&!is.na(data$mean.c),], sm="SMD")

tiff(file = "fig-PVGRF-forest.tiff", res =300, width = 4000, height = 4400,compression ="lzw") 
forest(meta1)
dev.off()

tiff(file = "fig-PVGRF-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
#For the summary measure "SMD", Hedges??£¤ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")
#   Linear regression test of funnel plot asymmetry (methods of moment)
#data:  meta1
#t = 0.4701, df = 40, p-value = 0.6408
#alternative hypothesis: asymmetry in funnel plot
#sample estimates:
#      bias    se.bias      slope 
# 0.3739426  0.7954396 -0.3259450 
metabias(meta1,method="count")
metabias(meta1,method="peters")
#If argument method.bias is "linreg", the test statistic is based on a weighted linear regression of
#the treatment effect on its standard error (Egger et al., 1997). The test statistic follows a t distribution
#with number of studies - 2 degrees of freedom.
#==================================

meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='FZ1'&!is.na(data$mean.c),], sm="SMD")

tiff(file = "fig-FZ1-forest.tiff", res =300, width = 4000, height = 2000) 
forest(meta1)
dev.off()
tiff(file = "fig-FZ1-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="mm")
#  meta1
#t = -2.259, df = 14, p-value = 0.04036
#alternative hypothesis: asymmetry in funnel plot
#sample estimates:
#     bias   se.bias     slope 
#-8.495322  3.760652  2.956547 
#======================================
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='TPVGRF'&!is.na(data$mean.c),], sm="SMD")
tiff(file = "fig-TPVGRF-forest.tiff", res =300, width = 4000, height = 4400) 
forest(meta1)
dev.off()
tiff(file = "fig-TPVGRF-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="mm")
#   Linear regression test of funnel plot asymmetry (methods of moment)
#
#data:  meta1
#t = 5.8361, df = 31, p-value = 1.957e-06
#alternative hypothesis: asymmetry in funnel plot
#sample estimates:
#     bias   se.bias     slope
# 8.902911  1.525482 -2.087364
#==================================
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Study, data=data[data$group=='TFZ1'&!is.na(data$mean.c),], sm="SMD")
tiff(file = "fig-TFZ1-forest.tiff", res =300, width = 4000, height = 2000,compression ="lzw") 
forest(meta1)
dev.off()
tiff(file = "fig-TFZ1-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="mm")

#        Linear regression test of funnel plot asymmetry (methods of moment)
#metry (methods of moment)
#data:  meta1
#t = 0.3392, df = 11, p-value = 0.7409
#alternative hypothesis: asymmetry in funnel plot
#sample estimates:
#     bias   se.bias     slope 
#1.9769415 5.8288506 0.1953013 
#====================================================



tiff(file = "e1-1.tiff", res =300, width = 4000, height = 4400,compression ="lzw") 
forest(meta1)
dev.off()


fit.seg<-segmented(fit.glm, seg.Z=~age,psi=25)

