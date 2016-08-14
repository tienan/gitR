library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(dbDriver("MySQL"), host = "xxx", port = 3306, username = "xxx", password = "xxx", dbname = "xxx")
#=======================һ����Ϣ��
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
genderSet=c('��','Ů')
#typeJob
typeJobSet=c('��ʦ','��������Ա','����Ա','��ҵ��λ��Ա','��ҵԱ��')
#age
floor(rnorm(1,50,20))
#ICD
ICDSet=c('C50','D25','D27','E04','G45','H25','H40','I26','I61','I74','I83','J20','J44','J45','J47','J93','K35','K40','N80','O00','O42','O80')
#disease
diseaseSet=c('���ٰ�','�ӹ�����','�ѳ���','��״����','���׶�������','������','�����','��˨��','�Գ�Ѫ','����˨��','��������','֧������','�����Էμ���','����','֧��������','����','��β��','���ɹ���','�ӹ���Ĥ��λ','��λ����','̥Ĥ����','˳��')
#level
level=c(3','3','3','3','3','2','2','2','2','2','1','1','1')
#hopitalName
hopitalName=c('��1����ҽԺ','��2����ҽԺ','��3����ҽԺ','��4����ҽԺ','��5����ҽԺ','��6����ҽԺ','��7����ҽԺ','��8����ҽԺ','��9����ҽԺ','��10����ҽԺ','ǧ��ɽ����ҽԺ','ͬ������ҽԺ','�������ҽԺ')
#subitdate 2013��-2014�꣬2�������
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
#================================================�����Ϊ��
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
#===============================================������ݿ�
#����ID���籣�š��������Ա���Ա���͡����䡢ICD���롢���岡�����ơ����䡢����ҽԺ�ȼ�������ҽԺ���֡��ܷ��ã�ͬһ����Ϣ��

#mechineResult

#dealTime

#resultSubmitTime

#cost_abnormal �����쳣

#rule_abnormal ����쳣

#habit_abnormal ��ҽϰ���쳣

#total_abnormal �����쳣

#filter_abnormal �ؼ������쳣

#customize_abnormal �û��Զ��쳣










