name_col = colnames(data)
len = length(name_col)
for (i in 3:len){
tmp = data[,i]
tmp_1=as.numeric(as.character(tmp))
a  = quantile(tmp_1,0.025,na.rm = TRUE)
b  = quantile(tmp_1,0.975,na.rm = TRUE)
condition = tmp_1 >= a & tmp_1 <= b 
tmp_d = tmp_1[condition]
tmp_data = data[condition,]
jpeg(paste(name_col[i],".jpeg"))
boxplot( as.numeric(as.character(get(name_col[i])))~ CollectionData , data = tmp_data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = name_col[i])
dev.off()
}


i
boxplot(  eval(name_col[3]) ~ CollectionData , data = data, col = "lightgray",pch="1", cex=2,xlab = "Time Point", ylab = name_col[3])


jpeg("Age.jpeg")
boxplot( Age~ CollectionData , data = data, col = "lightgray",pch="1", cex=2,xlab = "Time Point", ylab = "Age")
dev.off()

jpeg("Hospitaldays.jpeg")
boxplot(  Hospital.days~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "Days in hospital")
dev.off()


jpeg("TotalCost.jpeg")
boxplot( TotalCost ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "Total Cost")
dev.off()

jpeg("TotalCost.jpeg")
boxplot( as.numeric(as.character(TotalCost)) ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "Total Cost")
dev.off()

jpeg("Drugs.jpeg")
boxplot( Drugs  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "Drugs Cost")
dev.off()

jpeg("TotalProtein.jpeg")
boxplot( TotalProtein  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "TotalProtein")
dev.off()

jpeg("TotalBilirubin.jpeg")
boxplot( TotalBilirubin   ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "TotalBilirubin")
dev.off()

jpeg("Creatinine.jpeg")
boxplot(  Creatinine    ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = " Creatinine ")
dev.off()


jpeg("Potassium.jpeg")
boxplot(  Potassium   ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = " Potassium  ")
dev.off()

jpeg("Sodium.jpeg")
boxplot(  Sodium  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = " Sodium ")
dev.off()

jpeg("Chlorine.jpeg")
boxplot(  Chlorine  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "Chlorine")
dev.off()

jpeg("AlanineAminotransferase.jpeg")
boxplot( AlanineAminotransferase ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "AlanineAminotransferase")
dev.off()

jpeg("AspartateAminotransferase.jpeg")
boxplot( AspartateAminotransferase ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "AspartateAminotransferase")
dev.off()

jpeg("ALP.jpeg")
boxplot( ALP  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "ALP")
dev.off()

jpeg("LDH.jpeg")
boxplot( LDH  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "LDH")
dev.off()

jpeg("RGT.jpeg")
boxplot( r.GT  ~ CollectionData , data = data, col = "lightgray",pch="1", cex=0.5,xlab = "Time Point", ylab = "RGT")
dev.off()


tmp = data[,3]
tmp_1=as.numeric(as.character(tmp))
a  = quantile(tmp_1,0.025)
b  = quantile(tmp_1,0.975)
condition = tmp_1 >= a & tmp_1 <= b 
tmp_d = tmp_1[,condition]
shapiro.test(tmp_d)








