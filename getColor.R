getColor=function(mapdata,provname,provcol,othercol)
{
	f=function(x,y) ifelse(x %in% y,which(y==x),0);
	colIndex=sapply(mapdata@data$NAME,f,provname);
	col=c(othercol,provcol)[colIndex+1];
	return(col);
}