d.x <- seq(-1,1,0.01) 
cord.y <- dnorm(seq(-1,1,0.01)) 
curve(-dnorm(x,0,1),xlim=c(-3,3),main='Standard Normal') 
polygon(cord.x,-cord.y,col='black')
polygon(cord.x,-cord.y,col='skyblue')


install.packages("foreign")
install.packages("nnet")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("pROC")


library(e1071)
library(rpart)


library(pROC)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

dat = read.table("yifuying_wu.txt",header=TRUE,sep="\t")
cdplot(factor(GDM)~TPOAB,data=dat,main='?¡ì???t??¡ì1??¡ì?¡ì?¡ìa?',ylab='2??¡ì?¡ì??¡ì?????¡ê¡è',xlab='??¡ì|?¡§¡é?¡ìa??¡ì?¨¦?¡ì?¨¦?')

lm_1=glm(GDM~Age+Weight+Height+Systolic+Diastolic+Smoke+TSH+FT4+TPOAB,family=binomial(link='logit'),data=dat)


y=predict(lm_1,dat[,c(2:ncol(dat))],type='response')

roc1 <- plot.roc(dat$GDM, y, percent=TRUE, col="1")
rocobj <- roc(dat$GDM, y)
# Full AUC:
auc(rocobj)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

==================
dat = read.table("yifuying_wu.txt",header=TRUE,sep="\t")
data = na.omit(dat)
data = data[,c(2:7,9:12)]

x <- subset(data, select = -GDM)
y <- data$GDM


rpart.model <- rpart(GDM ~ ., data = head(data))

rpart.pred <- predict(rpart.model, head(x))





model <- svm(GDM ~ ., data = data)

rpart.model <- rpart(GDM ~ ., data = data)

rpart.pred <- predict(rpart.model, x)

table(rpart.pred, true = data[,10])

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
table(pred, y)


obj <- tune.svm(GDM ~ ., data = data, gamma = 2^(-1:1), cost = 2^(2:4))

=========================





library(e1071)
library(rpart)

index = 1:nrow(dat)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(dat[testindex,])
trainset <- na.omit(dat[-testindex,])

svm.model <- svm(GDM~Age+Weight+Height+Systolic+Diastolic+Smoke+TSH+FT4+TPOAB, data = na.omit(dat), kernel ="radial")

pred <- predict(svm.model,data[,c(2:7,9:11)])
data = na.omit(dat)
table(pred, data[,12])










data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)


