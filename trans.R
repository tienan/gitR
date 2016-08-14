trans = function(df){
	r = df
	alpha = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z" )
	for (i in 1:nrow(df)){
		for(j in 1:ncol(df)){
			r[i,j] = alpha[df[i,j]+1]
		} 
	}
 	return(r)
}
H_index=function(vector,n){
	return(1 - sum(r$freq*(r$freq-1))/(n*(n-1)))

}