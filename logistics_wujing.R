data$stat=data$stat-1
data$stat
output=glm(stat~Age,family=binomial,data=data)
output=glm(formula = stat~Gender+Age+BMI,family=binomial,data=data)
logistic.regression.or.ci(output)
summary(output)
predict(output)
predict(output,newdata=data)
predict(output,newdata=data,type="stat")
predict(output,newdata=data,type="reponse")
predict(output,newdata=data,type="response")
?predict
predict(output,newdata=data[data$stat=1,],type="response")
predict(output,newdata=data[data$stat==1,],type="response")
?write.table
write.table(predict(output,newdata=data[data$stat==1,],type="response"),file="fat_heart_result.txt"
)
write.table(predict(output,newdata=data,type="response"),file="fat_heart_result.txt")
data = read.table(file='increase_heart.txt',header=TRUE)
head(data)
data$stat=data$stat-1
output=glm( stat~Gender+Age+BMI,family=binomial,data=data)
predict(output,newdata=data,type="response")
write.table(predict(output,newdata=data,type="response"),file="increase_heart_result.txt")
history