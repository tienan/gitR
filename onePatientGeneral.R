library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
#con=dbConnect(drv, host = "10.200.187.228", port = 5432, user= "postgres", dbname = "postgres")
con=dbConnect(drv, host = "10.200.187.191", port = 5432, user= "postgres", dbname = "postgres_1")

dbCutoff=function(){
dbDisconnect(con)
}
patientGenerationSeriseTime<-function(n,a){
for (i in 1:n){
patientGeneration()
Sys.sleep(abs(floor(rnorm(1,mean=a,sd=3))))
}
}

patientGenerationTime = function(){
start=Sys.time()
patientGeneration()
Sys.time()-start
}


patientGenerationSerise=function(n){
for (i in 1:n){
patientGeneration()
}
}

noDetailPatientGeneration <- function(){

#drv <- dbDriver("PostgreSQL")
#con=dbConnect(drv, host = "10.200.187.228", port = 5432, user= "postgres", dbname = "postgres")

alphaSet=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','1','2','3','4','5','6','7','8','9','0');
nameSet = read.table("namesSet.txt")
genderSet=c('男','女')
typeJobSet=c('教师','外来务工人员','公务员','事业单位人员','企业员工')
ICDSet=c('C50','D25','D27','E04','G45','H25','H40','I26','I61','I74','I83','J20','J44','J45','J47','J93','K35','K40','N80','O00','O42','O80')
diseaseSet=c('乳腺癌','子宫肌瘤','卵巢瘤','甲状腺肿','基底动脉疾病','白内障','青光眼','肺栓塞','脑出血','动脉栓塞','静脉曲张','支气管炎','阻塞性肺疾病','哮喘','支气管扩张','气胸','阑尾炎','腹股沟疝','子宫内膜异位','异位妊娠','胎膜早破','顺产')
levelSet=c('3','3','3','3','3','2','2','2','2','2','1','1','1')
hopitalSet=c('第1人民医院','第2人民医院','第3人民医院','第4人民医院','第5人民医院','第6人民医院','第7人民医院','第8人民医院','第9人民医院','第10人民医院','千佛山社区医院','同济社区医院','武进社区医院')
itemSet=read.table(file='item.txt',header=TRUE,sep=',')
year=c('2013','2014')
month=c(1:12)
day=c(1:28)
hour=c(0:23)
minute = c(0:59)
second = c(0:59)
dataYibao = read.table(file="YibaoData.txt",head=TRUE)
names = levels(dataYibao$JCD)
scale = c(0,40,100)
k=1
name=c()
for(j in 1:length(names)){
temData = dataYibao[dataYibao$JCD==names[j],]
if(nrow(temData)>2) {
x= temData$LOS
y = temData$TC-temData$OC
lm.sol = lm( y ~ x)
assign(paste("model_",names[k],sep=''), lm.sol)
name[k]=names[j]

k=k+1
}
}



#patientID
patientID=paste(alphaSet[sample(0:36,20)])
patientID=as.character(paste(patientID,collapse = ''))
#patientSecurityNumber
patientSecurityNumber=paste(alphaSet[sample(0:36,12)])
patientSecurityNumber= as.character(paste(patientSecurityNumber,collapse=''))
#patientName
patientName=as.character(paste(nameSet[sample(1:500,1),1],nameSet[sample(1:500,1),2],sep = ""))
patientName = iconv(patientName,from='',to="UTF-8")
#gender
gender=as.character(genderSet[sample(1:2,1)])
gender = iconv(gender,from='',to="UTF-8")
#typeJob
typeJob=as.character(typeJobSet[sample(1:5,1)])
typeJob = iconv(typeJob,from='',to="UTF-8")
#age
age = floor(rnorm(1,50,20))
#ICD&disease
number=sample(1:length(ICDSet),1)
ICD=as.character(ICDSet[number])
ICD = iconv(ICD,from='',to="UTF-8")
disease=as.character(diseaseSet[number])
disease = iconv(disease,from='',to="UTF-8")
#level&hopital
number=sample(1:length(levelSet),1)
level=as.character(levelSet[number])
level=iconv(level,from='',to="UTF-8")
hopital=as.character(hopitalSet[number])
hopital=iconv(hopital,from='',to="UTF-8")
#los
los = abs(floor(rnorm(1,10,20)))
#subitdate
time1=paste(year[sample(1:2,1)],as.character(month[sample(1:12,1)]),as.character(day[sample(1:28,1)]),sep="-")
time2=paste(as.character(hour[sample(1:24,1)]),as.character(minute[sample(1:60,1)]),as.character(second[sample(1:60,1)]),sep=":")
time = paste(time1,time2,sep=' ')
subitdate = as.character(as.POSIXct(time))
#tc,	lc	mc	cc	oc
model=get(ls(pattern=paste('_',ICD,sep='')))
ex=data.frame(x=los)
tc=abs(predict(model,ex,interval="prediction",level=0.95))
tc=tc[1]
lc = tc[1]*0.06*abs(rnorm(1))
mc= tc[1]*0.43*abs(rnorm(1))
cc = tc[1]*0.15*abs(rnorm(1))
oc = tc[1]*0.09*abs(rnorm(1))

#修改提交时间
#
subitdate=as.character(Sys.time())

#timeCost
unit= abs(1+rnorm(1,sd=0.3))

timeCost=sample(0:1,1)*unit+sample(c(0,5:10),1)*unit+3*unit+5*unit+sample(0:1,1)*(sample(0:1,1)*unit+sample(c(0,5:10),1)*unit+3*unit)+20*abs(rnorm(1))*unit

#timeSubmitionResult

timeSubmitionResult=as.POSIXct(subitdate)+timeCost
timeSubmitionResult=as.character(timeSubmitionResult)

#costAbnormal(a1) 费用异常 

costAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#ruleAbnormal(a2) 规程异常

ruleAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#habitAbnormal(a3) 就医习惯异常

habitAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#totalAbnormal(a4) 总体异常

totalAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)/2

#filterAbnormal(a5) 关键环节异常
filterAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)/2

#customizeAbnormal(a6) 用户自定异常
customizeAbnormal = 2*atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)
#checkResult
checkResult=max(costAbnormal,ruleAbnormal,habitAbnormal,totalAbnormal,filterAbnormal,customizeAbnormal)
#checkDecide
checkDecide='未人工判断'
checkDecide=iconv(checkDecide,from='',to="UTF-8")
#checkRemark
checkRemark='无'
checkRemark=iconv(checkRemark,from='',to="UTF-8")
#checkUser
checkUser="机器"
checkUser=iconv(checkUser,from='',to="UTF-8")




sql <- "insert into T_MEDICAL_INFO"
sqltem=paste("values ('",patientID,"','",patientSecurityNumber,"','",patientName,"','",gender,"','",typeJob,"','",age,"','",ICD,"','",disease,"','",level,"','",hopital,"','",los,"','",subitdate,"','",tc,"','",lc,"','",mc,"','",cc,"','",oc,"','",timeCost,"','",timeSubmitionResult,"','",costAbnormal,"','",ruleAbnormal,"','",habitAbnormal,"','",totalAbnormal,"','",filterAbnormal,"','",customizeAbnormal,"','",checkResult,"','",checkDecide,"','",checkRemark,"','",checkUser,"')",sep='')
sql=paste(sql,sqltem)
dbGetQuery(con,sql)
dbDisconnect(con)


}
patientGeneration <- function(){


#con=dbConnect(drv, host = "10.200.187.228", port = 5432, user= "postgres", dbname = "postgres")

alphaSet=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','1','2','3','4','5','6','7','8','9','0');
nameSet = read.table("namesSet.txt")
genderSet=c('男','女')
typeJobSet=c('教师','外来务工人员','公务员','事业单位人员','企业员工')
ICDSet=c('C50','D25','D27','E04','G45','H25','H40','I26','I61','I74','I83','J20','J44','J45','J47','J93','K35','K40','N80','O00','O42','O80')
diseaseSet=c('乳腺癌','子宫肌瘤','卵巢瘤','甲状腺肿','基底动脉疾病','白内障','青光眼','肺栓塞','脑出血','动脉栓塞','静脉曲张','支气管炎','阻塞性肺疾病','哮喘','支气管扩张','气胸','阑尾炎','腹股沟疝','子宫内膜异位','异位妊娠','胎膜早破','顺产')
levelSet=c('3','3','3','3','3','2','2','2','2','2','1','1','1')
hopitalSet=c('第1人民医院','第2人民医院','第3人民医院','第4人民医院','第5人民医院','第6人民医院','第7人民医院','第8人民医院','第9人民医院','第10人民医院','千佛山社区医院','同济社区医院','武进社区医院')
itemSet=read.table(file='item.txt',header=TRUE,sep=',')
year=c('2013','2014')
month=c(1:12)
day=c(1:28)
hour=c(0:23)
minute = c(0:59)
second = c(0:59)
dataYibao = read.table(file="YibaoData.txt",head=TRUE)
names = levels(dataYibao$JCD)
scale = c(0,40,100)
k=1
name=c()
for(j in 1:length(names)){
temData = dataYibao[dataYibao$JCD==names[j],]
if(nrow(temData)>2) {
x= temData$LOS
y = temData$TC-temData$OC
lm.sol = lm( y ~ x)
assign(paste("model_",names[k],sep=''), lm.sol)
name[k]=names[j]

k=k+1
}
}



#patientID
patientID=paste(alphaSet[sample(0:36,5)])
patientID=as.character(paste(patientID,collapse = ''))
#patientSecurityNumber
patientSecurityNumber=paste(alphaSet[sample(0:36,12)])
patientSecurityNumber= as.character(paste(patientSecurityNumber,collapse=''))
#patientName
patientName=as.character(paste(nameSet[sample(1:500,1),1],nameSet[sample(1:500,1),2],sep = ""))
patientName = iconv(patientName,from='',to="UTF-8")
#gender
gender=as.character(genderSet[sample(1:2,1)])
gender = iconv(gender,from='',to="UTF-8")
#typeJob
typeJob=as.character(typeJobSet[sample(1:5,1)])
typeJob = iconv(typeJob,from='',to="UTF-8")
#age
age = floor(rnorm(1,50,20))
#ICD&disease
number=sample(1:length(ICDSet),1)
ICD=as.character(ICDSet[number])
ICD = iconv(ICD,from='',to="UTF-8")
disease=as.character(diseaseSet[number])
disease = iconv(disease,from='',to="UTF-8")
#level&hopital
number=sample(1:length(levelSet),1)
level=as.character(levelSet[number])
level=iconv(level,from='',to="UTF-8")
hopital=as.character(hopitalSet[number])
hopital=iconv(hopital,from='',to="UTF-8")
#los
los = abs(floor(rnorm(1,10,20)))
#subitdate
time1=paste(year[sample(1:2,1)],as.character(month[sample(1:12,1)]),as.character(day[sample(1:28,1)]),sep="-")
time2=paste(as.character(hour[sample(1:24,1)]),as.character(minute[sample(1:60,1)]),as.character(second[sample(1:60,1)]),sep=":")
time = paste(time1,time2,sep=' ')
subitdate = as.character(as.POSIXct(time))
#tc,	lc	mc	cc	oc
model=get(ls(pattern=paste('_',ICD,sep='')))
ex=data.frame(x=los)
tc=abs(predict(model,ex,interval="prediction",level=0.95))
tc=tc[1]
lc = tc[1]*0.06*abs(rnorm(1))
mc= tc[1]*0.43*abs(rnorm(1))
cc = tc[1]*0.15*abs(rnorm(1))
oc = tc[1]*0.09*abs(rnorm(1))

#timeCost
unit= abs(1+rnorm(1,sd=0.3))
timeCost=sample(0:1,1)*unit+sample(c(0,5:10),1)*unit+3*unit+5*unit+sample(0:1,1)*(sample(0:1,1)*unit+sample(c(0,5:10),1)*unit+3*unit)+20*abs(rnorm(1))*unit

#修改提交时间
subitdate=as.character(Sys.time())
#修改患者ID
IDSuffix=unlist(strsplit(subitdate,' '))
IDSuffix1=unlist(strsplit(IDSuffix[1],'-'))
IDSuffix2=unlist(strsplit(IDSuffix[2],':'))
patientID=paste(patientID,paste(IDSuffix1,collapse=''),paste(IDSuffix2,collapse=''),sep='')


#timeSubmitionResult

timeSubmitionResult=as.POSIXct(subitdate)+timeCost
timeSubmitionResult=as.character(timeSubmitionResult)

#costAbnormal(a1) 费用异常 

costAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#ruleAbnormal(a2) 规程异常

ruleAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#habitAbnormal(a3) 就医习惯异常

habitAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)

#totalAbnormal(a4) 总体异常

totalAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)/2

#filterAbnormal(a5) 关键环节异常
filterAbnormal = atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)/2

#customizeAbnormal(a6) 用户自定异常
customizeAbnormal = 2*atan(abs(rnorm(1,sd=0.1))+sample(c(0,0,0,0,0,1),1)*0.2)
#checkResult
checkResult=max(costAbnormal,ruleAbnormal,habitAbnormal,totalAbnormal,filterAbnormal,customizeAbnormal)
#checkDecide
checkDecide='未人工判断'
checkDecide=iconv(checkDecide,from='',to="UTF-8")
#checkRemark
checkRemark='无'
checkRemark=iconv(checkRemark,from='',to="UTF-8")
#checkUser
checkUser="机器"
checkUser=iconv(checkUser,from='',to="UTF-8")

sql <- "insert into T_MEDICAL_INFO"
sqltem=paste("values ('",patientID,"','",patientSecurityNumber,"','",patientName,"','",gender,"','",typeJob,"','",age,"','",ICD,"','",disease,"','",level,"','",hopital,"','",los,"','",subitdate,"','",tc,"','",lc,"','",mc,"','",cc,"','",oc,"','",timeCost,"','",timeSubmitionResult,"','",costAbnormal,"','",ruleAbnormal,"','",habitAbnormal,"','",totalAbnormal,"','",filterAbnormal,"','",customizeAbnormal,"','",checkResult,"','",checkDecide,"','",checkRemark,"','",checkUser,"')",sep='')
sql=paste(sql,sqltem)
dbGetQuery(con,sql)
#dbDisconnect(con)

#================================================诊断行为表
itemNames=colnames(itemSet)


for(i in 1:los){
#operationCount=abs(floor(rnorm(1,mean=10,sd=10)))+5
operationCount=2
for(j in 1:operationCount){
#con_1=dbConnect(drv, host = "10.200.187.228", port = 5432, user= "postgres", dbname = "postgres")
itemOne=itemSet[sample(1:length(itemSet),1),]
#patientID 同上
operationID=paste(alphaSet[sample(0:36,36)])
operationID=as.character(paste(operationID,collapse = ''))
#print(operationID)
#item
item=iconv(itemOne[1,1],from='',to="UTF-8")
#itemLevel
itemLevel=itemOne[1,2]
#combined
if(itemOne[1,3]==0){
	combined="非复方"
	
}else{
	combined="复方"
}
combined=iconv(combined,from='',to="UTF-8")
#quantity
quantity=iconv(itemOne[1,4],from='',to="UTF-8")
#unit
unit=iconv(itemOne[1,5],from='',to="UTF-8")
#specifications
specifications=iconv(itemOne[1,6],from='',to="UTF-8")
#price 
price=itemOne[1,7]
#number
number=itemOne[1,8]
#tieshu
tieshu=itemOne[1,9]
#cost
cost=itemOne[1,10]
#subitdateTime
subitdateTime=as.Date(as.POSIXct(subitdate))-los+i
subitdateTime = paste(subitdateTime,time2,sep=' ')
#index
index=iconv(itemOne[1,11],from='',to="UTF-8")
#修改operationID
IDSuffix=unlist(strsplit(subitdateTime,' '))
IDSuffix1=unlist(strsplit(IDSuffix[1],'-'))
IDSuffix2=unlist(strsplit(IDSuffix[2],':'))
operationID=paste(operationID,paste(IDSuffix1,collapse=''),paste(IDSuffix2,collapse=''),sep='')
sql <- "insert into t_medical_info_detail "
sqlItem=paste("values('",patientID,"','",operationID,"','",item,"','",itemLevel,"','",combined,"','",quantity,"','",unit,"','",specifications,"','",price,"','",number,"','",tieshu,"','",cost,"','",subitdateTime,"','",index,"')",sep='')
sql=paste(sql,sqlItem)
dbGetQuery(con,sql)
#dbDisconnect(con_1)
}
}


}


#sql="insert into T_MEDICAL_INFO (ID, NAME, SEX, DIAGONSE_RESULT, FEE, IS_CHECK, CHECK_RESULT, CHECK_REMARKS, CHECK_TYPE, DATA_TYPE, MEDICAL_NUMBER, DISEASE_TYPE, NO_PASS_REASON, CREATE_TIME, CHECK_TIME, FEE_TIME,PEOPLE_TYPE,HOSPITAL_TYPE,CHECK_DECIDE,ABNORAML_RESULT) values ('123', '123', 'man', '123', 123, '1', '通过', '1', '1', '1', '123', '1', '123', null, null, null,'1','3',null,'不通过');"
#sql <- paste("insert into T_MEDICAL_INFO ", "values ('",patientName,"');", sep="")
#dbGetQuery(con,sql)
