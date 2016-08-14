greycp_t <- function(vector){
	type = 8
	num = length(vector)/type
	matrix =matrix(vector,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1t
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:3,5:8)){
		delta[,k] = abs(matrix[,i]-matrix[,4])
		k = k+1
	}c()
	min_delta = min(delta)
	max_delta = max(delta)
	lamda = (min_delta+0.5*max_delta) / (delta + 0.5*max_delta)
	r=c()
	for (i in 1:ncol(lamda)){
		r[i] = mean(lamda[,i])
	}
	return(r) 
}
greycp_t1 <- function(vector){
	time_point = 4
	num = length(vector)/time_point
	matrix =matrix(vector,nrow = 4, ncol = 8, byrow = TRUE,)
	max_c = c()
	for (i in 1:num){
		max_c[i] = max(matrix[,i])
		matrix[,i]=matrix[,i]/max_c[i]
	}
	delta = matrix(c(1:28),time_point,7)
	k = 1
	for (i in c(1:3,5:8)){
		delta[,k] = abs(matrix[,i]-matrix[,4])
		k = k+1
	}
	min_delta = min(delta)
	max_delta = max(delta)
	lamda = (min_delta+0.5*max_delta) / (delta + 0.5*max_delta)
	r=c()
	for (i in 1:ncol(lamda)){
		r[i] = mean(lamda[,i])
	}
	return(r) 
}





d <- vegdist(data_2_c,"euclidean")
csin <- hclust(d, method="single")



plot(csin, hang=-1)

d <- vegdist(data_2_t,"euclidean")
csin <- hclust(d, method="single")



plot(csin, hang=-1)


d <- vegdist(high_express)
csin <- hclust(d, methodad="single")
plot(csin, hang=-1)
rect.hclust(csin, 3)
rect.hclust(csin, 40)
rect.hclust(csin, 90)





greycp_frame_1(data_2012[c(1:8),])
greycp_frame_1(data_2012[c(8:16),])
greycp_frame_1(data_2012[c(17:24),])
greycp_frame_1(data_2012[c(25:32),])
greycp_frame_1(data_2012[c(32:41),])

greycp_t_frame_1(data_2012[c(1:8),])
greycp_t_frame_1(data_2012[c(8:16),])
greycp_t_frame_1(data_2012[c(17:24),])
greycp_t_frame_1(data_2012[c(25:32),])
greycp_t_frame_1(data_2012[c(32:41),])


greycp_frame_1(data_2013[c(1:10),])
greycp_frame_1(data_2013[c(11:20),])
greycp_frame_1(data_2013[c(21:30),])
greycp_frame_1(data_2013[c(31:39),])
greycp_frame_1(data_2013[c(40:49),])

greycp_t_frame_1(data_2013[c(1:8),])
greycp_t_frame_1(data_2013[c(9:16),])
greycp_t_frame_1(data_2013[c(17:24),])
greycp_t_frame_1(data_2013[c(25:32),])
greycp_t_frame_1(data_2013[c(32:41),])






greycp_t_frame( data_2012[c(1,10),])
greycp_t_frame( data_2012[c(11,20),])
greycp_t_frame( data_2012[c(21,28),])
greycp_t_frame( data_2012[c(29,38),])
greycp_t_frame( data_2012[c(39,49),])


greycp_frame(data_2013[c(1:10),])
greycp_frame(data_2013[c(11:20),])
greycp_frame(data_2013[c(21:30),])
greycp_frame(data_2013[c(31:39),])
greycp_frame(data_2013[c(40:49),])

greycp_t_frame(data_2013[c(1:10),])
greycp_t_frame(data_2013[c(11:20),])
greycp_t_frame(data_2013[c(21:30),])
greycp_t_frame(data_2013[c(31:39),])
greycp_t_frame(data_2013[c(40:49),])


greycp_frame(data_2013[c(1:10),])
greycp_frame(data_2013[c(11:20),])
greycp_frame(data_2013[c(21:30),])
greycp_frame(data_2013[c(31:39),])
greycp_frame(data_2013[c(40:49),])
================================
data_2011= read.table("2011-cp_wai_1.txt")

greycp_frame(data_2011[c(1:7),])
greycp_frame(data_2011[c(8:15),])
greycp_frame(data_2011[c(16:23),])
greycp_frame(data_2011[c(24:32),])
greycp_frame(data_2011[c(33:41),])

greycp_t_frame(data_2011[c(1:7),])
greycp_t_frame(data_2011[c(8:15),])
greycp_t_frame(data_2011[c(16:23),])
greycp_t_frame(data_2011[c(24:32),])
greycp_t_frame(data_2011[c(33:41),])

data_2011= read.table("2011-cp_nei_1.txt")

greycp_frame_1(data_2011[c(1:5),])
greycp_frame_1(data_2011[c(6:10),])
greycp_frame_1(data_2011[c(11:15),])
greycp_frame_1(data_2011[c(16:19),])
greycp_frame_1(data_2011[c(20:24),])

greycp_t_frame_1(data_2011[c(1:5),])
greycp_t_frame_1(data_2011[c(6:10),])
greycp_t_frame_1(data_2011[c(11:15),])
greycp_t_frame_1(data_2011[c(16:19),])
greycp_t_frame_1(data_2011[c(20:24),])

data_2011= read.table("2011-cp_wai_1.txt")

greycp_frame(data_2011[c(1:7),])
greycp_frame(data_2011[c(8:15),])
greycp_frame(data_2011[c(16:23),])
greycp_frame(data_2011[c(24:32),])
greycp_frame(data_2011[c(33:41),])

greycp_t_frame(data_2011[c(1:7),])
greycp_t_frame(data_2011[c(8:15),])
greycp_t_frame(data_2011[c(16:23),])
greycp_t_frame(data_2011[c(24:32),])
greycp_t_frame(data_2011[c(33:41),])

data_2012= read.table("2012-cp_wai_1.txt")

greycp_frame(data_2012[c(1:10),])
greycp_frame(data_2012[c(11:20),])
greycp_frame(data_2012[c(21:28),])
greycp_frame(data_2012[c(29:38),])
greycp_frame(data_2012[c(29:49),])

greycp_t_frame(data_2012[c(1:10),])
greycp_t_frame(data_2012[c(11:20),])
greycp_t_frame(data_2012[c(21:28),])
greycp_t_frame(data_2012[c(29:38),])
greycp_t_frame(data_2012[c(29:49),])

data_2012= read.table("2012-cp_nei_1.txt")

greycp_frame_1(data_2012[c(1:8),])
greycp_frame_1(data_2012[c(9:16),])
greycp_frame_1(data_2012[c(17:24),])
greycp_frame_1(data_2012[c(25:32),])
greycp_frame_1(data_2012[c(33:41),])

greycp_t_frame_1(data_2012[c(1:8),])
greycp_t_frame_1(data_2012[c(9:16),])
greycp_t_frame_1(data_2012[c(17:24),])
greycp_t_frame_1(data_2012[c(25:32),])
greycp_t_frame_1(data_2012[c(33:41),])

data_2013= read.table("2013-cp_wai_1.txt")

greycp_frame(data_2013[c(1:10),])
greycp_frame(data_2013[c(11:20),])
greycp_frame(data_2013[c(21:30),])
greycp_frame(data_2013[c(31:39),])
greycp_frame(data_2013[c(40:49),])

greycp_t_frame(data_2013[c(1:10),])
greycp_t_frame(data_2013[c(11:20),])
greycp_t_frame(data_2013[c(21:30),])
greycp_t_frame(data_2013[c(31:39),])
greycp_t_frame(data_2013[c(40:49),])

data_2013= read.table("2013-cp_nei_1.txt")

greycp_frame_1(data_2013[c(1:7),])
greycp_frame_1(data_2013[c(8:15),])
greycp_frame_1(data_2013[c(16:23),])
greycp_frame_1(data_2013[c(24:31),])
greycp_frame_1(data_2013[c(32:39),])

greycp_t_frame_1(data_2013[c(1:7),])
greycp_t_frame_1(data_2013[c(8:15),])
greycp_t_frame_1(data_2013[c(16:23),])
greycp_t_frame_1(data_2013[c(24:31),])
greycp_t_frame_1(data_2013[c(32:39),])











