# make a plot of intelligent hospital 

library(treemap)


tmp = write.table(GNI2010,file="gni.txt", col.names = FALSE,row.names = FALSE)

data(GNI2010)
treemap (GNI2010,
index = c("continent", "iso3"),
vSize = "population",
vColor = "GNI",
type = "value",
fontsize.labels = 25,
fontsize.legend = 16)

dat = read.csv("tree_eg_data_1.csv")

attach(dat)
tiff(file = "ping-2.tif", res =300, width = 1000, height = 1400, compression ="lzw") 
treemap (dat,
index = c("Information", "data"),
vSize = "population",
vColor = "GNI",
type = "value",
fontsize.labels = 10)
dev.off()

