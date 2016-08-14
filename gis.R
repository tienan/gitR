install.packages("rgdal")
install.packages("sp")
install.packages("maptools")
install.packages("maps")
install.packages("mapdata")
install.packages("ggplot2")

library(rgdal)
library(sp)
library(maptools)
library(maps)
library(mapdata)
source('getColor.R')
library("ggplot2")


x=readShapePoly('bou2_4p.shp')
provname=c('¨|?o?ê¨oD','?à?à??¨oD','¨￠¨|?t¨o?','1???¨o?','o¨2¨￠¨2?-¨o?','???-¨o?','?a¨￠?¨o?','¨?¨??¨°¨oD','ot?à?à¨o?')
provname=c('¨|?o?ê¨oD','?à?à??¨oD','¨￠¨|?t¨o?','1???¨o?','o¨2¨￠¨2?-¨o?','???-¨o?','?a¨￠?¨o?','¨?¨??¨°¨oD','ot?à?à¨o?')

#AQI=c(69.51,94.65,65.03,64.5,78.77,78.59,86.53,81.49,83.97)
#AQI=c(29.19,29.23,47.31,46.41,,31.76,,45.28)
provcol=rgb(red=1,green=0,blue=1-(AQI-min(AQI))/max(max(AQI)-min(AQI)));
plot(x,col=getColor(x,provname,provcol,"white"),xlab="",ylab="");

provname=c("北京市","天津市","河北省","山西省","内蒙古自治区",
		"辽宁省","吉林省","黑龙江省","上海市","江苏省",
		"浙江省","安徽省","福建省","江西省","山东省",
		"河南省","湖北省","湖南省","广东省",
		"广西壮族自治区","海南省","重庆市","四川省","贵州省",
		"云南省","西藏自治区","陕西省","甘肃省","青海省",
		"宁夏回族自治区","新疆维吾尔自治区","台湾省",
		"香港特别行政区");



provname=c("Beijing", "Tianjin", "Hebei province", "Shanxi Province", "Inner Mongolia autonomous region",
"Liaoning province", "Jilin province", "Heilongjiang province", "Shanghai", "Jiangsu province",
"Zhejiang province", "Anhui province", "Fujian province", "Jiangxi province", "Shandong province",
"Henan province", "Hubei province", "Hunan province", "Guangdong province",
"Guangxi Zhuang autonomous region", "Hainan province", "Chongqing", "Sichuan province", "Guizhou",
"Yunnan province", "Tibet autonomous region", "Shaanxi province", "Gansu province", "Qinghai",
"Ningxia Hui autonomous region", "Xinjiang Uygur autonomous region", "Taiwan",
"Hong Kong");
pop=c(0,0,0,0,1,
		3,0,1,113,43,
		28,23,1,16,1,
		2,3,0,1,
		1,0,1,3,1,
		5,0,1,1,1,
		1,5,0,0);




provname=c("Beijing", "Tianjin", "Hebei province", "Shanxi Province", "Inner Mongolia autonomous region",
"Liaoning province", "Jilin province", "Heilongjiang province", "Shanghai", "Jiangsu province",
"Zhejiang province", "Anhui province", "Fujian province", "Jiangxi province", "Shandong province",
"Henan province", "Hubei province", "Hunan province", "Guangdong province",
"Guangxi Zhuang autonomous region", "Hainan province", "Chongqing", "Sichuan province", "Guizhou",
"Yunnan province", "Tibet autonomous region", "Shaanxi province", "Gansu province", "Qinghai",
"Ningxia Hui autonomous region", "Xinjiang Uygur autonomous region", "Taiwan",
"Hong Kong");
pop=c(1,0,0,1,1,
		9,4,3,152,62,
		52,39,29,19,8,
		140,3,7,8,
		1,1,5,8,7,
		3,0,2,3,1,
		1,6,0,0);
pop[pop>=100]=-3
pop[pop>=19]=-2
pop[pop>=1]=-1
pop=abs(pop)

provcol=rgb(red=1,green=(1-pop/max(pop)),blue=(1-pop/max(pop)));

tiff(file = "distribution.tif", res =400, width = 3000, height =2500)
plot(x,col=getColor(x,provname,provcol,"white"),xlab="",ylab="");
text(Dat$jd, Dat$wd, Dat[, 1], cex = 0.5, col = rgb(0,
    0, 0, 0.7), pos = c(2, 1, 1, 1, 3, 1, 2, 3, 1, 2, 1, 2, 2,
    4, 1, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
dev.off()
text(dat$jd, dat$wd, dat[, 1], cex = 0.5, col = rgb(0,
    0, 0, 0.7), pos = c(2, 1, 1, 1, 1, 1, 2, 3, 1, 2, 4, 2, 2,
    4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 4, 4, 4, 2))

Dat = read.csv(text = "city, jd, wd
Beijing 0.1%, 116.4666667 , 39.9
Shanghai 26.3%, 121.4833333 , 31.23333333
Tianjin 0%, 117.1833333 , 39.15
Chongqing 0.3%, 106.5333333 , 30.53333333
Heilongjiang 0.5%, 126.6833333 , 45.75
Jilin 0.7%, 125.3166667 , 43.86666667
Liaoning 2.0%, 125.8 , 41.23333333
Inner Mongolia 0.2%, 111.8 , 40.81666667
Hebei 0%, 114.4666667 , 38.03333333
Shanxi 0.2%, 113.5666667 , 37.86666667
Shaong 1%, 117,36.63333333
Henan 24.4%, 116.7 , 33.8
Shaanxi 0.3%, 115.9 , 34.26666667
Gansu 0.5%, 100.8166667 , 36.05
Ningxia 0.2%, 106.2666667 , 38.33333333
Qinghai 0.2%, 101.75 , 36.63333333
Xinjiang 1%, 87.6 , 43.8
Anhui 7%, 117.3 , 31.85
Jiangsu 11%, 119.3333333 , 33.03333333
Zhejiang 9%, 120.15 , 30.23333333
Hunan 1%, 117,27.18333333
Jiangxi 3%, 115.8666667 , 26.88333333
Hubei 0.5%, 114.35 , 30.61666667
Sichuan 1%, 104.0833333 , 30.65
Guizhou 1%, 106.7 , 26.58333333
Fujian 5%, 122.3 , 26.08333333
Guangdong 1%, 113.25 , 23.13333333
Hainan 0.1%, 110.3333333 , 19.03333333
Guangxi 0.1%, 108.3333333 , 22.8
Yunnan 0.5%, 102.6833333, 25
Tibet 0%, 91.16666667, 29.66666667")



Dat = read.csv(text = "city, jd, wd
Beijing 0.1%, 116.4666667 , 39.9
Shanghai 26.3%, 121.4833333 , 31.23333333
Tianjin 0%, 117.1833333 , 39.15
Chongqing 0.3%, 106.5333333 , 30.53333333
Heilongjiang province 0.5%, 126.6833333 , 45.75
Jilin province 0.7%, 125.3166667 , 43.86666667
Liaoning province 2.0%, 125.8 , 41.23333333
Inner Mongolia autonomous region 0.2%, 111.8 , 40.81666667
Hebei province 0%, 114.4666667 , 38.03333333
Shanxi Province 0.2%, 113.5666667 , 37.86666667
Shaong province 1%, 117,36.63333333
Henan province 24.4%, 116.7 , 33.8
Shaanxi province 0.3%, 115.9 , 34.26666667
Gansu province 0.5%, 100.8166667 , 36.05
Ningxia Hui autonomous region 0.2%, 106.2666667 , 38.33333333
Qinghai province 0.2%, 101.75 , 36.63333333
Xinjiang uygur autonomous region 1%, 87.6 , 43.8
Anhui province 7%, 117.3 , 31.85
Jiangsu province 11%, 119.3333333 , 33.03333333
Zhejiang province 9%, 120.15 , 30.23333333
Hunan province 1%, 117,27.18333333
Jiangxi province 3%, 115.8666667 , 26.88333333
Hubei province 0.5%, 114.35 , 30.61666667
Sichuan province 1%, 104.0833333 , 30.65
Guizhou province 1%, 106.7 , 26.58333333
Fujian province 5%, 122.3 , 26.08333333
Guangdong province 1%, 113.25 , 23.13333333
Hainan province 0.1%, 110.3333333 , 19.03333333
Guangxi Zhuang autonomous region 0.1%, 108.3333333 , 22.8
Yunnan province 0.5%, 102.6833333, 25
Tibet autonomous region 0%, 91.16666667, 29.66666667")








Dat = read.csv(text = "city, jd, wd
Beijing, 116.4666667, 39.9
Shanghai, 121.4833333, 31.23333333
Tianjin, 117.1833333, 39.15
Chongqing, 106.5333333, 29.53333333
Heilongjiang province, 126.6833333, 45.75
Jilin province, 125.3166667, 43.86666667
Liaoning province, 123.4, 41.83333333
Inner Mongolia autonomous region, 111.8, 40.81666667
Hebei province, 114.4666667, 38.03333333
Shanxi Province, 112.5666667, 37.86666667
Shandong province, 117,36.63333333
Henan province, 113.7, 34.8
Shaanxi, 111.9, 34.26666667
Gansu province, 103.8166667, 36.05
Ningxia hui autonomous region, 106.2666667, 38.33333333
Qinghai province, 101.75, 36.63333333
Xinjiang uygur autonomous region, 87.6, 43.8
In anhui province, 117.3, 31.85
Jiangsu province, 118.8333333, 3.03333333
Zhejiang province, 120.15, 30.23333333
Hunan province, 113,28.18333333
Jiangxi province, 115.8666667, 28.68333333
Hubei province, 114.35, 30.61666667
Sichuan province, 104.0833333, 30.65
Guizhou province, 106.7, 26.58333333
Fujian, 119.3, 26.08333333
Guangdong province, 113.25, 23.13333333
Hainan province, 110.3333333, 20.03333333
Guangxi zhuang autonomous region, 108.3333333, 22.8
Yunnan province, 102.6833333, 25
Tibetan autonomous region of Tibet, 91.16666667, 29.66666667 ")
text(Dat$jd, Dat$wd, Dat[, 1], cex = 0.5, col = rgb(0,
    0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
    4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))


dat = read.csv(text = "3?¨oD,jd,wd
    ?à?à??,116.4666667,39.9
    ¨|?o?ê,121.4833333,31.23333333
    ¨?¨??¨°,117.1833333,39.15
    ???¨?,106.5333333,29.53333333
    o¨2¨￠¨2?-¨o?,126.6833333,45.75
    ?a¨￠?¨o?,125.3166667,43.86666667
    ¨￠¨|?t¨o?,123.4,41.83333333
    ?¨2?¨|1??á?????,111.8,40.81666667
    o¨??à?à¨o?,114.4666667,38.03333333
    ¨|????¨o?,112.5666667,37.86666667
    ¨|???¨o?,117,36.63333333
    o¨???¨o?,113.7,34.8
    ¨|????¨o?,108.9,34.26666667
    ?¨o?¨¤¨o?,103.8166667,36.05
    ?t?????á??á?????,106.2666667,38.33333333
    ?¨¤o?ê¨o?,101.75,36.63333333
    D??????¨￠???á?????,87.6,43.8
    ??2??¨o?,117.3,31.85
    ?-??¨o?,118.8333333,32.03333333
    ???-¨o?,120.15,30.23333333
    ot??¨o?,113,28.18333333
    ?-???¨o?,115.8666667,28.68333333
    ot?à?à¨o?,114.35,30.61666667
    ?????§¨o?,104.0833333,30.65
    1¨??Y¨o?,106.7,26.58333333
    ??ê??§¨o?,119.3,26.08333333
    1???¨o?,113.25,23.13333333
    o?ê??¨o?,110.3333333,20.03333333
    1?????á3?á??á?????,108.3333333,22.8
    ????¨o?,102.6833333,25
    ???2?2??á??á?????,91.16666667,29.66666667")


dat = read.csv(text = "3?¨oD,jd,wd
    ?à?à?? 0.1%,116.4666667,39.9
    ¨|?o?ê 26.3%,121.4833333,31.23333333
    ¨?¨??¨° 0%,117.1833333,39.15
    ???¨? 0%,106.5333333,29.53333333
    o¨2¨￠¨2?-¨o? 0.5%,126.6833333,45.75
    ?a¨￠?¨o? 0.7%,125.3166667,43.86666667
    ¨￠¨|?t¨o? 2.0%,123.4,41.83333333
    ?¨2?¨|1??á????? 0.2%,111.8,40.81666667
    o¨??à?à¨o? 0%,114.4666667,38.03333333
    ¨|????¨o? 0.2%,112.5666667,37.86666667
    ¨|???¨o? 1%,117,36.63333333
    o¨???¨o? 24.4%,113.7,34.8
    ¨|????¨o? 0.3%,108.9,34.26666667
    ?¨o?¨¤¨o? 0.5%,103.8166667,36.05
    ?t?????á??á????? 0.2%,106.2666667,38.33333333
    ?¨¤o?ê¨o? 0.2%,101.75,36.63333333
    D??????¨￠???á????? 1%,87.6,43.8
    ??2??¨o? 7%,117.3,31.85
    ?-??¨o? 11%,118.8333333,32.03333333
    ???-¨o? 9%,120.15,30.23333333
    ot??¨o? 1%,113,28.18333333
    ?-???¨o? 3%,115.8666667,28.68333333
    ot?à?à¨o? 0.5%,114.35,30.61666667
    ?????§¨o? 1%,104.0833333,30.65
    1¨??Y¨o? 1%,106.7,26.58333333
    ??ê??§¨o? 5%,119.3,26.08333333
    1???¨o? 1%,113.25,23.13333333
    o?ê??¨o? 0.1%,110.3333333,20.03333333
    1?????á3?á??á????? 0.1%,108.3333333,22.8
    ????¨o? 0.5%,102.6833333,25
    ???2?2??á??á????? 0%,91.16666667,29.66666667)"

text(dat$jd, dat$wd, dat[, 1], cex = 0.5, col = rgb(0,
    0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
    4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))

points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))


axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)