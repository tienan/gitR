library(VennDiagram)

dat = read.table("2to2compare.txt",header=TRUE,sep="\t")


A = 1:150
B = c(121:170,300:320)
C = c(20:40,141:200)
Length_A<-length(A)
Length_B<-length(B)
Length_C<-length(C)
Length_AB<-length(intersect(A,B))
Length_BC<-length(intersect(B,C))
Length_AC<-length(intersect(A,C))
Length_ABC<-length(intersect(intersect(A,B),C))

A = 1:150
B = 1:20
Length_A<-length(A)
Length_B<-length(B)
Length_AB<-length(intersect(A,B))




T<-venn.diagram(list(A=A,B=B),filename=NULL
,lwd=1,lty=2
,col=c('red','green'),fill=c('red','green')
,cat.col=c('red','blue')
,rotation.degree=0)
grid.draw(T)


samples = c("E13.5","E14.5","E16.5","E18.5","PN-1","PN-3","PN-5","PN-7")
samplesId = c("q1","q2","q3","q4","q5","q6","q7","q8")
names=c()
k=1
for (i in (1:7)){
	for (j in (i:7)){
		names[k] = paste(samples[i],"vs",samples[j+1],".tif",sep = "", collapse = NULL)
		name =  paste(samples[i],"vs",samples[j+1],sep = " ", collapse = NULL)


		ab = dat[dat[,2]==samplesId[i]&dat[,3]==samplesId[j+1]&dat[,5]==0,]

		abc = dat[dat[,2]==samplesId[i]&dat[,3]==samplesId[j+1]&dat[,5]==1,]

		a = abc[abc[,6]==0,]

		b = abc[abc[,6]==1,]

		a_name=c(as.character(a[,1]),as.character(ab[,1]))
		b_name=c(as.character(b[,1]),as.character(ab[,1]))
		venn.diagram(list(Down=a_name,Down=b_name)
		,main = name
		,main.cex= 4
		,filename=names[k]
		#,filename=NULL
		#,lwd=5,lty=2
		#,col=c('red','green')
		,height = 3000, width = 3000, resolution = 500, 
		imagetype = "tiff", units = "px", compression = "lzw"
		,scaled=FALSE
		,fill=c('red','green')
		,cat.col=c('red','blue')
		,cat.cex=2
		¡ê?cex= 3
		,rotation.degree=0)

		k = k+1
	}	
}

samplesId = c("q1","q2","q3","q4","q5","q6","q7","q8")

(dat[dat[,2]=='q1'&dat[,3]=='q2',])

#filename = c("cai_Q1 vs Q2.tif","cai_Q2 vs Q3.tif")

ab = dat[dat[,2]=='q1'&dat[,3]=='q2'&dat[,5]==0,]

abc = dat[dat[,2]=='q1'&dat[,3]=='q2'&dat[,5]==1,]

a = abc[abc[,6]==0,]

b = abc[abc[,6]==1,]

a_name=c(as.character(a[,1]),as.character(ab[,1]))
b_name=c(as.character(b[,1]),as.character(ab[,1]))


T<-venn.diagram(list(Q1_down=a_name,Q2_down=b_name)
,filename=filename[1]
#,filename=NULL
#,lwd=5,lty=2
#,col=c('red','green')
,height = 3000, width = 3000, resolution = 500, 
imagetype = "tiff", units = "px", compression = "lzw"
,scaled=FALSE
,fill=c('red','green')
,cat.col=c('red','blue')
,cat.cex=1.5
,rotation.degree=0)
grid.draw(T)


