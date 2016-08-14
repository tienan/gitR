library(rgdal)
library(sp)
library(maptools)
library(maps)
library(mapdata)
source('getColor.R')
library("ggplot2")

x=readShapePoly('bou2_4p.shp')
mydata = read.table("chenjing-pm-5.txt",header=TRUE,sep="\t")
p <- ggplot()
p <- p + geom_polygon( data=x, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )

p <- p + geom_point( data=mydata,aes(x=Long, y=Lat, size = count), color="coral1") + scale_size(name="count")

p = p+geom_text( data=mydata, hjust=0.3, vjust=-0.5, aes(x=Long, y=Lat, label=Place), colour="gold2", size=4 )

