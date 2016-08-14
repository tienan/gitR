#wenhui-1,µÚÒ»´Îmeta·ÖÎö
library('metafor')
library('meta')
data=read.table('wenhui-high.txt',header=TRUE,sep=',')
data=read.table("wenhui-ec-0.txt",header=TRUE,sep=',')
data=read.table("wenhui-ec-1.txt",header=TRUE,sep=',')
data=read.table('wenhui-total.txt',header=TRUE,sep=',')

meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data[data$groups=='°¢ÍÐ·¥ËûÍ¡A',], sm="SMD")
meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data[data$groups=='ÐÁ·¥ËûÍ¡X',], sm="SMD")
meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data[data$groups=='ÈðÊæ·¥ËûÍ¡R',], sm="SMD")


meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-ec1-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-ec1-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="rank")



meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-ec0-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-ec0-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="rank")





meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-all-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-all-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="rank")

meta2 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD", byvar=groups)




metabias(meta1,method="linreg")

metabias(meta1,method="mm")

score", or "peters"

metabias(meta1,method="peters")

metabias(meta1,method="rank")



tiff(file = "fig-big-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-big-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()
metabias(meta1,method="mm")

#For the summary measure "SMD", Hedges¡¯ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")
 Linear regression test of funnel plot asymmetry (methods of moment)

data:  meta1
t = 4.0605, df = 15, p-value = 0.001025
alternative hypothesis: asymmetry in funnel plot
sample estimates:
     bias   se.bias     slope 
17.602874  4.335165 -3.676002 
=================================
data=read.table('wenhui-high-1.txt',header=TRUE,sep=',')
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-big-forest-1.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-big-funnel-1.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()


#For the summary measure "SMD", Hedges¡¯ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")

   Linear regression test of funnel plot asymmetry (methods of moment)

data:  meta1
t = 1.1009, df = 11, p-value = 0.2945
alternative hypothesis: asymmetry in funnel plot
sample estimates:
      bias    se.bias      slope 
 6.8685171  6.2392147 -0.8327188 
===================================
data=read.table('wenhui-low.txt',header=TRUE,sep=',')
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-small-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-small-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()


#For the summary measure "SMD", Hedges¡¯ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")
data:  meta1
t = 1.032, df = 17, p-value = 0.3165
alternative hypothesis: asymmetry in funnel plot
sample estimates:
       bias     se.bias       slope 
3.379905464 3.275125186 0.008147377 

==========================================
data=read.table('wenhui-yn.txt',header=TRUE,sep=',')
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-yn-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-yn-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()


#For the summary measure "SMD", Hedges¡¯ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")
        Linear regression test of funnel plot asymmetry (methods of moment)

data:  meta1
t = 1.0516, df = 22, p-value = 0.3044
alternative hypothesis: asymmetry in funnel plot
sample estimates:
      bias    se.bias      slope 
1.40039140 1.33169755 0.09539074 

===========================================

data=read.table('wenhui-cure.txt',header=TRUE,sep=',')
meta1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, Authors, data=data, sm="SMD")

tiff(file = "fig-cure-forest.tiff", res =300, width = 4000, height = 3000) 
forest(meta1)
dev.off()
tiff(file = "fig-cure-funnel.tiff", res =300,width = 1800, height = 1400) 
funnel(meta1)
dev.off()


#For the summary measure "SMD", Hedges¡¯ adjusted g is utilised for pooling.
#metabias(meta1,method="linreg")
metabias(meta1,method="mm")
   Linear regression test of funnel plot asymmetry (methods of moment)

data:  meta1
t = 1.1777, df = 25, p-value = 0.25
alternative hypothesis: asymmetry in funnel plot
sample estimates:
     bias   se.bias     slope 
2.8885465 2.4527689 0.4027848 


