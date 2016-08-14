library("RSQLite")
library(rgdal)
library(sp)
library(maptools)
library(maps)
library(mapdata)
source('getColor.R')
conn = dbConnect(dbDriver("SQLite"), dbname="aqi-20130326.sqlite")
year=as.character(c(2001:2012))
season=c('-01-01','-03-31','-04-01','-06-30','-07-01','-09-30','-10-01','-12-31')
city=c('上海','北京','大连','广州','哈尔滨','杭州','沈阳','天津','武汉')
a=c()
l=1
for (i in 1:length(city)){
	object = iconv(city[i],from='',to="UTF-8")
	
	for (j in 1:length(year)){
	currentYear = year[j]
	k=1
		while (k<8){
			up = paste(currentYear,season[k],sep='')
			k=k+1
			down = paste(currentYear,season[k],sep='')
			sqlterm=paste("select  avg(value) from aqi where  datetime(recordDate,'unixepoch')>'",up,"' and datetime(recordDate,'unixepoch')  <'",down,"' and areaName = '",object,"'",sep='')
			result = dbGetQuery(conn,sqlterm)
			a[l]=as.numeric(result)	
			l=l+1
			k=k+1
	
		}
	}
}

matrix(a,756/4,4)
write.table(matrix(a,432/4,4,byrow=TRUE),"cite_season.txt",row.names=FALSE,col.names=FALSE)


result = dbGetQuery(conn,sqlterm)



sql <- "select areaName, value,datetime(recordDate,'unixepoch') as from aqi" 
sqlterm=paste("select  avg(value) from aqi where  datetime(recordDate,'unixepoch')>'",up,"' and datetime(recordDate,'unixepoch')  <'",down,"' and areaName = '",object,"'",sep='')
sql = paste(sql,sqlterm,sep=' ')
result = dbGetQuery(conn,sql)



conn = dbConnect(dbDriver("SQLite"), dbname="aqi-20130326.sqlite")


time
city=c('北京','天津','上海','广州','武汉','杭州','沈阳','大连','哈尔滨')
for (i in 1:length(city)
	object = iconv(city[i],from='',to="UTF-8")
	sql <- "select areaName, value from aqi where areaName='",object,"'"
	up = paste(year[1],season[1],sep='')
	down = paste(year[1],season[2],sep='')
end



result = dbSendQuery



datacity = read.table('city.txt')
row.names(datacity)=c('上海','北京','大连','广州','哈尔滨','杭州','沈阳','天津','武汉')
d <- dist(datacity, method = "euclidean") 
fit <- hclust(d, method="complete") 
plot(fit) # display dendogram 
