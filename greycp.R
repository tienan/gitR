greycp_frame_1 <- function(df){
	type = 8 # 
	num = nrow(df)
	matrix =as.matrix(df,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:6,8)){
		delta[,k] = abs(matrix[,i]-matrix[,7])
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

greycp_t_frame_1 <- function(df){
	type = 7
	num = nrow(df)
	matrix =as.matrix(df,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:2,4:7)){
		delta[,k] = abs(matrix[,i]-matrix[,3])
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


greycp_frame <- function(df){
	type = 9 
	num = nrow(df)
	matrix =as.matrix(df,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:8)){
		delta[,k] = abs(matrix[,i]-matrix[,9])
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

greycp_t_frame <- function(df){
	type = 10
	num = nrow(df)
	matrix =as.matrix(df,nrow = num, ncol = type, byrow = TRUE)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:5,7:10)){
		delta[,k] = abs(matrix[,i]-matrix[,6])
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




greycp <- function(vector){
	type = 8
	num = length(vector)/type
	matrix =matrix(vector,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:4,6:8)){
		delta[,k] = abs(matrix[,i]-matrix[,5])
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

greycp_1 <- function(vector){
	type = 7
	num = length(vector)/type
	matrix =matrix(vector,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:3,5:7)){
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


greycp_c <- function(vector){
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
	for (i in c(1:4,6:8)){
		delta[,k] = abs(matrix[,i]-matrix[,5])
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
batch_c = function(df){

	for (i in 1:nrow(df)){
		print((greycp(as.matrix(df[i,]))))
		
	}
	for (i in 1:nrow(df)){
		print((greycp_t(as.matrix(df[i,]))))
	}
	
}

batch_c_1 = function(df){

	for (i in 1:nrow(df)){
		print((greycp_1(as.matrix(df[i,]))))
		
	}
	for (i in 1:nrow(df)){
		print((greycp_t_1(as.matrix(df[i,]))))
	}
	
}

greycp_t_1 <- function(vector){
	type = 7
	num = length(vector)/type
	matrix =matrix(vector,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
	k = 1
	for (i in c(1:3,5:7)){
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

greycp_t <- function(vector){
	type = 8
	num = length(vector)/type
	matrix =matrix(vector,nrow = num, ncol = type, byrow = TRUE,)
	for (i in 1:type){
		matrix[,i]=matrix[,i]/max(matrix[,i])
	}
	number = num*(type-1)
	delta = matrix(c(1:number),num,type-1)
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
