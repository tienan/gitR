library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(dbDriver("MySQL"), host = "xxx", port = 3306, username = "xxx", password = "xxx", dbname = "xxx")
#=======================一般信息表
#patientID
sampleQuantity=sample(0:9,9)
for (i in 1:9){
	sampleQuantity=c(SampleQuantity,sample(0:9,9))
}
sampleQuantity=matrix(SampleQuantity,9,9)
#patientSecurityNumber
alphaSet=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z);
#patientName
nameSet = read.table("namesSet.txt")
paste(nameSet[sample(1:500,1),1],nameSet[sample(1:500,1),2],sep = "")
#gender
genderSet=c('男','女')
#typeJob
typeJobSet=c('教师','外来务工人员','公务员','事业单位人员','企业员工')
#age
floor(rnorm(1,50,20))
#ICD
ICDSet=c('C50','D25','D27','E04','G45','H25','H40','I26','I61','I74','I83','J20','J44','J45','J47','J93','K35','K40','N80','O00','O42','O80')
#disease
diseaseSet=c('乳腺癌','子宫肌瘤','卵巢瘤','甲状腺肿','基底动脉疾病','白内障','青光眼','肺栓塞','脑出血','动脉栓塞','静脉曲张','支气管炎','阻塞性肺疾病','哮喘','支气管扩张','气胸','阑尾炎','腹股沟疝','子宫内膜异位','异位妊娠','胎膜早破','顺产')
#level
level=c(3','3','3','3','3','2','2','2','2','2','1','1','1')
#hopitalName
hopitalName=c('第1人民医院','第2人民医院','第3人民医院','第4人民医院','第5人民医院','第6人民医院','第7人民医院','第8人民医院','第9人民医院','第10人民医院','千佛山社区医院','同济社区医院','武进社区医院')
#subitdate 2013年-2014年，2年的数据
year=c('2013','2014')
month=c(1:12)
day=c(1:28)
hour=c(0:23)
minute = c(0:59)
second = c(0:59)
 
time1=paste(year[sample(1:2,1)],as.character(month[sample(1:12,1)]),as.character(day[sample(1:28,1)]),sep="-")
time2=paste(as.character(hour[sample(1:24,1)]),as.character(minute[sample(1:60,1)]),as.character(second[sample(1:60,1)]),sep=":")
time = paste(time1,time2,sep=' ')
time = as.POSIXct(time)
#los
los = abs(floor(rnorm(1,10,20)))
#tc,tc, lc, mc, cc, oc; input diseaseName, los,age,index; output tc, lc, mc, cc, oc 
dataYibao = read.table(file="YibaoData.txt",head=TRUE)
names = levels(dataYibao$JCD)
scale = c(0,40,100)
for(j in 1:length(names)){
for (i in 1:2){
temData = dataYibao[dataYibao$JCD==names[j]&dataYibao$Age<scale[i+1]&dataYibao$Age>=scale[i],]
if(nrow(temData)>2) {
x = temData$LOS
y = temData$TC-temData$OC
lm.sol = lm(formula = y ~ x)
model$name[j]=names[j]
model$model[j]=lm.sol
}
}
}
ex=data.frame(x=los)
tc = predict(model$model[model$name=="C50"],ex,interval="prediction",level=0.95) 
lc = tc*0.06*abs(rnorm(1))
mc= tc*0.43*abs(rnorm(1))
cc = tc*0.15*abs(rnorm(1))
oc = tc*0.09*abs(rnorm(1))
#
#================================================诊断行为表
#operationID
alphaSet=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z);
#patientID
alphaSet=c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,1,2,3,4,5,6,7,8,9,0);
#ItemName
item=read.table(file='item.txt',header=TRUE,sep=',')
item[sample(1,nrow(item),1)]
#date; 
day = floor(abs(20*rnorm(1)))
for (i in 1:day){
	date=as.Date(subtime)-day+1
}
#===============================================审核数据库
#个人ID、社保号、姓名、性别、人员类型、年龄、ICD编码、具体病种名称、年龄、就诊医院等级、就诊医院名字、总费用，同一般信息表

#mechineResult

#dealTime

#resultSubmitTime

#cost_abnormal 费用异常

#rule_abnormal 规程异常

#habit_abnormal 就医习惯异常

#total_abnormal 总体异常

#filter_abnormal 关键环节异常

#customize_abnormal 用户自定异常











