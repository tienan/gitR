library("cummeRbund")
cuff = readCufflinks(dir=getwd(), dbFile = "cuffData.db")

dat = read.table("../cufdiff.txt",head=T,sep="\t")
len = nrow(dat)
df = dat[1,]
j = 1
df[j,] = dat[1,]
for (i in 2:(len)){
	if (as.character(df[j,]$gene)==as.character(dat[i,]$gene)){	
		df[j,]$value_1 = dat[i,]$value_1 + df[j,]$value_1
		df[j,]$value_2 = dat[i,]$value_2 + df[j,]$value_2
		
	}else{
		j=j+1
		df[j,] = dat[i,]
	}	
}

write.table(df,file="4v1.csv",quote=F,sep="\t",row.names=F)

