confi95<- function(df,col){
	
	for (i in col){
	a = sort(df[,i],index.return=TRUE)
	l=floor(0.025*length(a$ix));
	u=ceiling(0.975*length(a$ix));
	logic = a$ix>l&a$ix<u
	index = a$ix	
	index = index[logic]
	df=df[index,];
	}
	return(df)
}