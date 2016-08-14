dat = read.table("zhoupuJingyi.txt",head=TRUE,sep="\t")
#mean(as.numeric(na.omit(dat[,2])))
#meav = matrix(0,6,11)
#stdv = matrix(0,6,11)
tm = c(1:6)*3.58
tprob = list()
#tprob = new.env()
for (i in (1:6)){
	for (j in (1:11)){
#	meav[i,j]=mean(as.numeric(as.character(na.omit(dat[dat[,1]==i+3,j+1]))))
	freq = rle(sort(as.numeric(as.character(na.omit(dat[dat[,1]==i+3,j+1])))))
	tprob = c(tprob,freq) #the 
#	stdv[i,j]=sd(as.numeric(as.character(na.omit(dat[dat[,1]==i+3,j+1]))))
#	meav[i,j]=mean(dat[dat[,1]==i+3,j+1])
#	stdv[i,j]=mean(dat[dat[,1]==i+3,j+1])
	}
}

p = c(0.11,0.15,0.40,0.13,0.03,0.18)
#Generate the distributio of floor
mento=0
bingqu=matrix(0,10000,6)
for (z in 1:10000){
p = c(0.11,0.15,0.40,0.13,0.03,0.18)
a = sample(c(4:9), 6, replace = FALSE)
bingqu[z,] = a
p = p[a-3]

#Generate the time-used 
m = 435
t =  matrix(0,m,12)
for (i in 1:m){
	s = sample(4:9,1,prob=p)
	t[i,1]=s
	s = s-3
	for (j in 1:11){
#	t[i,j+1] = abs(rnorm(1, mean = meav[s-3,j], sd = stdv[s-3,j]))
	t[i,j+1] = sample(tprob[((s-1)*11+j-1)*2+2]$values,1,prob=tprob[((s-1)*11+j-1)*2+1]$lengths)
	}
}

mento[z] = (sum(t[,9])+ sum(t[,3]))/sum(t[,2:12])
}

write.table(cbind(bingqu,mento), file = "mento", append = FALSE, quote = FALSE, sep = " ",
            row.names = FALSE,
            col.names = FALSE)


dat = t
for (i in (1:6)){
	for (j in (1:11)){
	meav[i,j]=mean(as.numeric(as.character(na.omit(dat[dat[,1]==i+3,j+1]))))

	stdv[i,j]=sd(as.numeric(as.character(na.omit(dat[dat[,1]==i+3,j+1]))))

	}
}


