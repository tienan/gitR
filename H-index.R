require(plyr)
dat= read.table("chenjing-vnta.txt",head=TRUE)
dat_1 = dat[,c(2:9)]
row.names(dat_1) = dat[,1]





num = c(1:26)
alpha = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z" )

for (i in nrow(dat))
====================================================
source("vntn.R")
#92
#dat = c(0.010869565,0.010869565,0.010869565,0.02173913,0.010869565,0.010869565,0.010869565,0.010869565)
#dat = c(0.52173913,0.695652174,0.336956522,0.413043478,0.489130435,0.673913043,0.760869565,0.5)
#142
#dat = c(0.010869565,0.010869565,0.010869565,0.02173913,0.010869565,0.010869565,0.010869565,0.010869565)
dat = c(0.49,0.63,0.36,0.43,0.41,0.68,0.74,0.50)

nam = c("MTUB21","MTUB04","QUB18","QUB26","QUB-11b","MIRU31","MIRU10","MIRU26")
#modify point
fileName="142_dist_vntr_30_max.txt"
#
for (i in 1:8){
	c = combn(8,i)
	for (j in 1:ncol(c)){
		for (k in 1:nrow(c)){
		write.table(nam[c[k,j]],file = fileName,quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="+" )	
		}
#modify point
	write.table(paste("\t",vntn_max_p(dat[c[,j]],30,2)),file = fileName,quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="\r\n" )
#
	write.table(" ",file = fileName,quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="\r\n" )		
	}
}

==============================================
dat_1 = read.table("vntr_all.R",header=TRUE)
dat_2 = dat_1[,c(2:9)]
dat_2 = trans(dat_2)
for (i in 1:8){
	c = combn(8,i)
	for (j in 1:ncol(c)){
		for (k in 1:nrow(c)){
			write.table(nam[c[k,j]],file = "dist_H-index.txt",quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="+" )	
		}
		dat_t = paste_c(as.data.frame(dat_2[,c[,j]]))
		n = rle(sort(dat_t))
		r = 1-sum(as.numeric(n$lengths)*(as.numeric(n$lengths)-1))/(sum(as.numeric(n$lengths))*sum(as.numeric(n$lengths)-1))
		write.table(paste("\t",r),file = "dist_H-index.txt",quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="\r\n" )
		write.table(" ",file = "dist_H-index.txt",quote = FALSE,append = TRUE,row.names = FALSE,col.names = FALSE,eol="\r\n" )		

	}
}
rle(sort(dat_2[,2]))
rle(sort(dat_2[,2]))

rle(dat_1[,c(3)])

=======================2a¡§o?
source("vntn.R")
index_v = c(1,2,3,4,5,6,7,8)
vector = c(4,4,4,4,4,4,4,4)
vntn_distri_n(vector,index_v,20,2)
vector=c(5,4,8,8,6,5,3,8)
vntn_distri_n(vector,index_v,20,2)

