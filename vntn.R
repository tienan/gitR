vntn_distri <- function(vector,snum,copy){
	# vector = c(4,4,4,4,4,4,4,4)
	vnta_table = read.table("vntn.txt",header=TRUE)
	distri = 1	
	for (i in 1:length(vector)){
		distri = distri*vnta_table[vector[i]+1,i]
	}
	w = choose(snum,copy)
	return(distri^copy*(1-distri)^(snum-copy)*w)
		
}

vntn_distri_n <- function(vector,index_v,snum,copy){
	# index_v = c(1,2,3,4,5,6,7,8)
	# vector = c(4,4,4,4,4,4,4,4)
	vector = vector + 1 
	vnta_table = read.table("vntn.txt",header=TRUE)
	distri = 1	
	for (i in 1:length(vector)){
		distri = distri*vnta_table[vector[i],index_v[i]]
	}
	w = choose(snum,copy-1)
	w1 = choose(snum,copy-2)
	r = 1 - distri^(copy-1)*(1-distri)^(snum-copy+1)*w - (1-distri)^snum
	return(r)
		
}

vntn_distri_m <- function(vector,index_v,snum,copy){
	# index_v = c(1,2,3,4,5,6,7,8)
	# vector = c(4,4,4,4,4,4,4,4)
	vector = vector + 1 
	vnta_table = read.table("vntn.txt",header=TRUE)
	distri = 1	
	for (i in 1:length(vector)){
		distri = distri*vnta_table[vector[i],index_v[i]]
	}
	w = choose(snum,copy-1)
	w1 = choose(snum,copy-2)
	r = 1 - distri^(copy-1)*(1-distri)^(snum-copy+1)*w - distri^(copy-2)*(1-distri)^(snum-copy+2)*w1
	return(r)
		
}



vntn_max_p = function(vector,snum,copy){
	distri = 1	
	for (i in 1:length(vector)){
		distri = distri*vector[i]
	}
#	w = choose(snum,copy)
	w = choose(snum,copy-1)
	w = choose(snum,copy-2)
	r = 1 - distri^(copy-1)*(1-distri)^(snum-copy+1)*w - distri^(copy-2)*(1-distri)^(snum-copy+2)*w
	return(r)
}



vntr_distri_1 = function(snum,copy,vntr_n){
	w = choose(8,vntr_n)
	r=c()
	for (i in 0:w-1){
		r[i]=vntn_distri_n(i,index_v,snum,copy)
	}
	return(r)
}

vntr_distri_1 = function(snum,copy,vntr_n){
	w = choose(8,vntr_n)
	r=c()
	for (i in 0:w-1){
		r[i]=vntn_distri_n(i,index_v,snum,copy)
	}
	return(r)
}

paste_c = function(df){
	r = data.frame() 
	for (i in 1:ncol(df)){
		r = paste(r,df[,i],sep="")
	}
	return(r)
}

v_multi = function(a,b){
	r = c()
	k=1
	for (i in 1:length(a)){
		for(j in 1:length(b)){
			r[k]=a[i]*b[j]
			k=k+1
		}
	}
	return(r)
}
dat_multi = function(dat){
	r = dat[,1]
	for (i in 2:ncol(dat)){
		r = v_multi(r,dat[,i])
		r = r[r>0]
	}
	return (r)
}

dat_multi_1 = function(dat){
	r = dat[,1]
	r = r[r>01]
	for (i in 2:ncol(dat)){
		s = dat[,i]
		r = r[s>0]
		r = v_multi(r,s)
		r = r[r>0]
	}
	return (r)
}
test = function(dat){
	dat = read.table("chenji-paper2.txt",sep="\t")
	r=0
	r = dat[,1]
	r
	r = r[r>0.1]
	for (i in c(2:8)){
		s = dat[,i]
		s = s[s>0.1]
		s
		r = v_multi(r,s)	
	}
	s = sort(r)
	t = s[1]
	d = s[2:length(s)] - s[1:length(s)-1]
	group = cbind(s[2:length(s)],d)
	cl = kmeans(group, 4)
	tiff(file = "chenjin-figure-1a.tif", res =300, width = 1500, height = 1400,compression="lzw")
	plot(s[2:length(s)],d,ylab="Difference between two  adjacent number",xlab="Occurrance of vector")

	dev.off()
#	tiff(file = "chenjin-figure-1b.tif", res =300, width = 1500, height = 1800,compression="lzw")
#	plot(group, ylim=c(0,0.003),xlim=c(0,0.006),pch = cl$clustercex=1,ylab="Difference between two  adjacent number",xlab="Occurrance of vector")
#	points(cl$centers, pch =1:4, cex=3)
#	legend("bottomright",legend=c("Class 1","Class 2","Class 3","Class 4"),pch=c(1:4),lty=0, bty = "n")
#	dev.off()
	
	tiff(file = "chenjin-figure-1c.tif", res =300, width = 2000, height = 1400,compression="lzw")
	aa = c(1,c(1:50)*10)
	r1= 0.0050 
	r2 = 0.0023
	r3 = 0.00029 
	k=1
	rr=0
	for (i in aa){
		rr[k] = p_value(r1,i)
		k=k+1
	}
	
	plot(c(1,c(1:50)*10),rr,type = "o",lwd = 1,pch=21,ylim=c(0,1),xlim=c(0,700),ylab="ES",xlab="Sample number of a batch")
	abline(h=0.05,col="black",lty=3,lwd=2)

	k=1
	rr=0	
	for (i in aa){
		rr[k] = p_value(r2,i)
		k=k+1
	}
	lines(c(1,c(1:50)*10),rr,type = "o",lwd = 1,pch=22)	
	k=1
	rr=0
	for (i in aa){
		rr[k] = p_value(r3,i)
		k=k+1
	}
	lines(c(1,c(1:50)*10),rr,type = "o",lwd = 1,pch=23)	
	legend("topleft",legend=c("Initial Occurrence = 0.005","Initial Occurrence = 0.0023","Initial Occurrence = 0.00029"),pch=c(21:23),lty=0, bty = "n")


	dev.off()
	
	tiff(file = "chenjin-figure-1d.tif", res =300, width = 1500, height = 1400,compression="lzw")
	pe = p_value(s,96)
	plot(s,pe,type = "o",lwd = 2,pch=21,yaxt="n",ylim=c(0,0.1),ylab="ES",xlab="Occurrence rate")
	axis(2,at=seq(0,1,0.05))
	abline(h=0.05,col="black",lty=3,lwd=2)
	pe = p_value(s,192)
	lines(s,pe,type = "o",lwd = 2,pch=22)	
	pe = p_value(s,288)
	lines(s,pe,type = "o",lwd = 2,pch=23)	
	pe = p_value(s,480)
	lines(s,pe,type = "o",lwd = 2,pch=24)	
	legend("bottomright",legend=c("batch = 96","batch = 192","batch = 288","batch = 480"),pch=c(21:24),lty=0, bty = "n")
	dev.off()
	
	
}

p_value = function(t,batch){
	O = 1 - (1-t)^batch - t^1*(1-t)^(batch-1)*batch
	return (O)
}






# Generate one vector
mento_carlo = function(){ 
	# Input the prior possibility	
	vntr_table = read.table("vntn.txt",header=TRUE)
	# Generate the vector
	x = c(1:nrow(vntr_table))
	v=0
	for (i in 1:8){
		prob = vntr_table[,i]
		v[i] = sample(x,1, replace = TRUE,prob)	
	}
	return(v)
}



# Calcualte the occurrance of one vector
prob  <- function(vector){
	vnta_table = read.table("vntn.txt",header=TRUE)
	distri = 1	
	index_v = c(1,2,3,4,5,6,7,8)
	for (i in 1:length(vector)){
		distri = distri*vnta_table[vector[i]+1,index_v[i]]
	}
	return(distri)
}

# Calcualte the ES value of one vector
p_value_n = function(distri,snum,copy){
	# Batch should be larger than copy
	O = 0
	t = distri
	batch = snum
	O = (1-t)^batch
	i = 1
	while  (i <= copy-1){
	O = O + t^i*(1-t)^(batch-i)*choose(batch,i)
	i = i+1
	}	
	return (1-O)
}



sitimulation = function(n){
	r = 0
	for (i in 1:n){
		v = mento_carlo()-1
		dist = prob(v)
		v = c(v,dist)
		write.table(as.numeric(v),"mento_carlo.txt",append = TRUE,
		sep = "\t",eol = "\t", na = "NA", 
		dec = ".", row.names = FALSE,col.names = FALSE)	
		write.table("","mento_carlo.txt",append = TRUE,
		sep = " ",eol = "\r\n", na = "NA",quote = FALSE, row.names = FALSE,col.names = FALSE)		
	}
}

sitimulation_c = function(n){
	r = 0
	v = mento_carlo()-1
	for (i in 1:n){
		v = mento_carlo()-1
		
		s = sample(c(0,1),1,prob=c(0.97,0.03))
		if (s==1){
		v = r
		}
		dist = prob(v)
		v = c(v,dist)
		write.table(c(s,as.numeric(v)),"mento_carlo_c.txt",append = TRUE,
		sep = "\t",eol = "\t", na = "NA", 
		dec = ".", row.names = FALSE,col.names = FALSE)	
		write.table("","mento_carlo_c.txt",append = TRUE,
		sep = " ",eol = "\r\n", na = "NA",quote = FALSE, row.names = FALSE,col.names = FALSE)	
		r = v	
	}
}


distribution = function(dat){
	t = matrix(0,13,8)
	
	for (i in 1:8){
		dist = table(dat[,i])		
		t[c(as.numeric(row.names(dist))+1),i] = as.numeric(dist)/sum(as.numeric(dist))
	}
	return(t)
}
# Each
change = function(dat,batch){
	x = distribution(dat)
	d = 0
	p_max = 0
	for (i in 1:50){
		a = ((i-1)*batch+1)
		b = i*batch
		x1 = 	distribution(dat[c(a:b),])
	d[i] = sum(abs(x1-x))
	p_max[i] = max(abs(x1-x))
	}
	d = cbind(d,p_max)
	return(d)
}

# Cumulation
change_1 = function(dat,batch){
	x = distribution(dat)
	d = 0
	p_max = 0
	for (i in 1:50){
		a = ((i-1)*batch+1)
		b = i*batch
		x1 = 	distribution(dat[c(1:b),])
	d[i] = sum(abs(x1-x))
	p_max[i] = max(abs(x1-x))
	}
	d = cbind(d,p_max)
	return(d)
}



# Calcualte the occurrance of one vector not using distribution file
prob_v  <- function(vector,vnta_table){
	distri = 1	
	index_v = c(1,2,3,4,5,6,7,8)
	for (i in 1:length(vector)){
		distri = distri*vnta_table[vector[i]+1,index_v[i]]
	}
	return(distri)
}


# Find the repeat vector and the corresponding p value
repeat_find = function(dat){	
	r = 0
	batch = nrow(dat)
	c = df2c(dat)
	v = rownames(c)
	m=0
	p_1=0
	p=0
	n = nrow(c)
	for (i in 1:n){
		w=as.numeric(unlist(strsplit(v[i],",")))
		r[i] = prob(w)
		m[i]=1-(1-0.05)^(1/n)
		p[i] = p_value_n(prob(w),batch,as.numeric(c[i]))
		p_1[i] = p_value_n(prob(w),batch,as.numeric(c[i])+1)
	}
	x = rbind(c,r,p,p_1,m)
	rownames(x) = c("repeat","occurrance rate","p value","p value + 1","diff")
	
	return(x)
}



repeat_find_c = function(dat,vnta_table){	
	r = 0
	batch = nrow(dat)
	c = df2c(dat)
	v = rownames(c)
	m=0
	p_1=0
	p=0
	n = nrow(c)
	for (i in 1:n){
		w=as.numeric(unlist(strsplit(v[i],",")))
		r[i] = prob_v(w,vnta_table)
		m[i]=1-(1-0.05)^(1/n)
		p[i] = p_value_n(prob(w),batch,as.numeric(c[i]))
		p_1[i] = p_value_n(prob(w),batch,as.numeric(c[i])+1)
	}
	x = rbind(c,r,p,p_1,m)
	rownames(x) = c("repeat","occurrance rate","p value","p value + 1","diff")
	
	return(x)
}



mento_carlo_sv = function(dat,batch){
	for (i in 1:50){
		a = ((i-1)*batch+1)
		b = i*batch
		vnta_table = distribution(dat[c(1:b),])
		write(i,"distribution_c.txt",append = TRUE, sep="\t")
		write.table(vnta_table,"distribution_c.txt",append = TRUE, sep="\t")
		c = repeat_find_c(dat[c(a:b),],vnta_table)
#		c = c[,c[3,]<c[5,1]]
		write(i,"monte_carlo_cfull.txt",append = TRUE, sep="\t")
		write.table(c,"monte_carlo_cfull.txt",append = TRUE, sep="\t")
#		write(ncol(c[,c[3,]>c[5,1]])/ncol(c),"monte_carlo_cfull.txt",append = TRUE, sep="\t")
#		write(ncol(c[,dat[4,]<c[5,1]])/ncol(c),"monte_carlo_cfull.txt",append = TRUE, sep="\t")

	}
}



#Mento carlo simulation

mento_carlo_s = function(dat,batch){
	for (i in 1:50){
		a = ((i-1)*batch+1)
		b = i*batch
		c = repeat_find(dat[c(a:b),])	
		write(i,"monte_carlo.txt",append = TRUE, sep="\t")
		write.table(c,"monte_carlo.txt",append = TRUE, sep="\t")
		write(ncol(c[,c[3,]>c[5,1]])/ncol(c),"monte_carlo.txt",append = TRUE, sep="\t")
		write(ncol(c[,c[4,]<c[5,1]])/ncol(c),"monte_carlo.txt",append = TRUE, sep="\t")

	}
}






significance = function(n){
	
}



# Cumulation
change_1 = function(dat){
	batch = 1000
	d = 0
	p_max = 0
	for (i in 1:50){
		a = ((i-1)*batch+1)
		b = i*batch
		x1 = 	distribution(dat[c(1:b),])
	}
	d = cbind(d,p_max)
	
	return(d)
}


# df 2 character
df2c = function(dat){
	r = c(" ")
	for (i in 1:nrow(dat)){
		r[i] = paste(dat[i,],sep = "",collapse=",")
	}
	r1 = table(r)
	r1 = r1[r1>=2]
#	r1 = r1[r1<2]
	return(r1)

}




test_1 = function(){
	tiff(file = "chenjin-figure-2.tif", res =300, width = 2500, height = 2000,compression="lzw")
  	plot(d[,2],ylim=c(0,0.05),xaxt = "n",xlim=c(-1,51),
	type="o",lwd=2,pch=24,cex=1.5, yaxs="i",xaxs="i",
	xlab="Weeks",ylab="Difference")
	axis(1,at=seq(1,50,1))
	abline(h=mean(d[,2]),col="black",lty=3,lwd=2)
	dev.off()

	tiff(file = "chenjin-figure-2b.tif", res =300, width = 2500, height = 2000,compression="lzw")
  	plot(d[,2],ylim=c(0,0.05),xaxt = "n",xlim=c(-1,51),
	type="o",lwd=2,pch=24,cex=1.5, yaxs="i",xaxs="i",
	xlab="Weeks of cumulation",ylab="Difference")
	axis(1,at=seq(1,50,1))
	abline(h=mean(d[,2]),col="black",lty=3,lwd=2)
	dev.off()


	tiff(file = "chenjin-figure-2c.tif", res =300, width = 2500, height = 2000,compression="lzw")
  	plot(1-as.matrix(c[,1]),ylim=c(0,0.5),xaxt = "n",xlim=c(-1,51),
	type="o",lwd=2,pch=24,cex=1.5, yaxs="i",xaxs="i",
	xlab="Week",ylab="Rate of unreasonable repeat")
	axis(1,at=seq(1,50,1))
	text(c(1:50),1-as.matrix(c[,1])+0.02,c[,2],cex = 1)
	dev.off()

	tiff(file = "chenjin-figure-5d.tif", res =300, width = 4000, height = 2000,compression="lzw")
  	par(mgp=c(2,0.8,0),mar=c(6, 6, 6, 6))
#	par(mgp=c(1.7,0.6,0.2),mar=c(6, 6, 6, 6))
	plot(r,ylim=c(0.5,1.2),xaxt = "n",xlim=c(-1,51),
	type="o",lwd=2,pch=24,cex=1.5, cex.lab = 1.5, cex.axis=1.3,yaxs="i",cex.axis=1.3,xaxs="i",
	xlab="Week",ylab="Rate of prediction")
	axis(1,at=seq(1,50,1),cex.axis=1.3)
	text(c(1:50),r+0.02,n,cex = 1)
	dev.off()

	dat = read.table("mento_carlo_c.txt")
	dat_1 = dat[dat[,1]==1,][,c(2:9)]
	r=0
	for (i in 1:nrow(dat_1)){
		r[i]=paste(dat_1[i,],sep = "",collapse=",")
	}
	
	
	f = dat_1[as.numeric(rownames(dat_1))<=1000,9]

	con <- file("monte_carlo_c_vector.txt", "r")
	i = 1
	r = 0
	n = 0
	line=readLines(con,n=1)
	while( length(line) != 0 ) {
	b = unlist(strsplit(line, "\t"))
     	f = dat_1[as.numeric(rownames(dat_1))<=i*1000&as.numeric(rownames(dat_1))>(i-1)*1000,9]
	n[i] = length(f)
	r[i] = length(intersect(f,b))/length(f)
	i=i+1
     	line=readLines(con,n=1)
	}
	close(con)

}

	