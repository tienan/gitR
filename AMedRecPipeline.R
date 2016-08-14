library('rmongodb')
mongo = mongo.create(host="localhost",db="med_rec")
buf = mongo.bson.buffer.create()
query = mongo.bson.from.buffer(buf)
cursor = mongo.find(mongo,"med_rec.hiv",query)

result.counter = mongo.count(mongo,"med_rec.hiv",query)

inhospital_id  =vector("character",result.counter)
category =vector("character",result.counter)
sepsis =vector("character",result.counter)
cirrhosis =vector("character",result.counter)
age =vector("numeric",result.counter)
gender =vector("character",result.counter)
hiv_st =vector("character",result.counter)
symptom =vector("character",result.counter)
diagnosis =vector("character",result.counter)
operation_way =vector("character",result.counter)
CD4_min =vector("numeric",result.counter)
CD4_bef_ope =vector("numeric",result.counter)
CD8_bef_ope =vector("numeric",result.counter)
CD42CD8_bef_ope =vector("numeric",result.counter)
wc_bef_ope =vector("numeric",result.counter)
hgb_bef_ope =vector("numeric",result.counter)
bp_bef_ope =vector("numeric",result.counter)
protein_bef_ope =vector("numeric",result.counter)
CD4_aft_ope =vector("numeric",result.counter)
CD8_aft_ope =vector("numeric",result.counter)
CD42CD8_aft_ope =vector("numeric",result.counter)
wc_aft_ope =vector("numeric",result.counter)
hgb_aft_ope =vector("numeric",result.counter)
bp_aft_ope =vector("numeric",result.counter)
protein_aft_ope =vector("numeric",result.counter)
CD4_lh =vector("numeric",result.counter)
CD8_lh =vector("numeric",result.counter)
CD42CD8_lh =vector("numeric",result.counter)
wc_lh =vector("numeric",result.counter)
hgb_lh =vector("numeric",result.counter)
bp_lh =vector("numeric",result.counter)
protein_lh =vector("numeric",result.counter) 


i = 1
while (mongo.cursor.next(cursor)){
cval = mongo.cursor.value(cursor)
inhospital_id[i] = mongo.bson.value(cval,"inhospital_id")
category[i] = mongo.bson.value(cval,"category")
sepsis[i] = mongo.bson.value(cval,"sepsis")
cirrhosis[i] = mongo.bson.value(cval,"cirrhosis")
age[i] = mongo.bson.value(cval,"age")
gender[i] = mongo.bson.value(cval,"gender")
hiv_st[i] = mongo.bson.value(cval,"hiv_st")
symptom[i] = mongo.bson.value(cval,"symptom")
diagnosis[i] = mongo.bson.value(cval,"diagnosis")
operation_way[i] = mongo.bson.value(cval,"operation_way")
CD4_min[i] = mongo.bson.value(cval,"CD4_min")
CD4_bef_ope[i] = mongo.bson.value(cval,"CD4_bef_ope")
CD8_bef_ope[i] = mongo.bson.value(cval,"CD8_bef_ope")
CD42CD8_bef_ope[i] = mongo.bson.value(cval,"CD42CD8_bef_ope")
wc_bef_ope[i] = mongo.bson.value(cval,"wc_bef_ope")
hgb_bef_ope[i] = mongo.bson.value(cval,"hgb_bef_ope")
bp_bef_ope[i] = mongo.bson.value(cval,"bp_bef_ope")
protein_bef_ope[i] = mongo.bson.value(cval,"protein_bef_ope")
CD4_aft_ope[i] = mongo.bson.value(cval,"CD4_aft_ope")
CD8_aft_ope[i] = mongo.bson.value(cval,"CD8_aft_ope")
CD42CD8_aft_ope[i] = mongo.bson.value(cval,"CD42CD8_aft_ope")
wc_aft_ope[i] = mongo.bson.value(cval,"wc_aft_ope")
hgb_aft_ope[i] = mongo.bson.value(cval,"hgb_aft_ope")
bp_aft_ope[i] = mongo.bson.value(cval,"bp_aft_ope")
protein_aft_ope[i] = mongo.bson.value(cval,"protein_aft_ope")
CD4_lh[i] = mongo.bson.value(cval,"CD4_lh")
CD8_lh[i] = mongo.bson.value(cval,"CD8_lh")
CD42CD8_lh[i] = mongo.bson.value(cval,"CD42CD8_lh")
wc_lh[i] = mongo.bson.value(cval,"wc_lh")
hgb_lh[i] = mongo.bson.value(cval,"hgb_lh")
bp_lh[i] = mongo.bson.value(cval,"bp_lh")
protein_lh[i] = mongo.bson.value(cval,"protein_lh")
i = i + 1
}

df = as.data.frame(list(inhospital_id=inhospital_id,category=category,sepsis=sepsis,cirrhosis=cirrhosis,age=age,gender=gender,hiv_st=hiv_st,symptom=symptom,diagnosis=diagnosis,operation_way=operation_way,CD4_min=CD4_min,CD4_bef_ope=CD4_bef_ope,CD8_bef_ope=CD8_bef_ope,CD42CD8_bef_ope=CD42CD8_bef_ope,wc_bef_ope=wc_bef_ope,hgb_bef_ope=hgb_bef_ope,bp_bef_ope=bp_bef_ope,protein_bef_ope=protein_bef_ope,CD4_aft_ope=CD4_aft_ope,CD8_aft_ope=CD8_aft_ope,CD42CD8_aft_ope=CD42CD8_aft_ope,wc_aft_ope=wc_aft_ope,hgb_aft_ope=hgb_aft_ope,bp_aft_ope=bp_aft_ope,protein_aft_ope=protein_aft_ope,CD4_lh=CD4_lh,CD8_lh=CD8_lh,CD42CD8_lh=CD42CD8_lh,wc_lh=wc_lh,hgb_lh=hgb_lh,bp_lh=bp_lh,protein_lh=protein_lh))

data_CD4_bef_ope = as.data.frame(list(category=df$category, CD4_min=as.numeric(as.character(df$CD4_bef_ope))))
boxplot( CD4_bef_ope ~  category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "cd4")

data_1 = as.data.frame(list(category=df$category, CD4_min=as.numeric(as.character(df$CD4_min))))
boxplot( CD4_min ~  category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "cd4")



data_1 = as.data.frame(list(category=df$category, CD4_bef_ope=as.numeric(as.character(df$CD4_bef_ope))))
boxplot( CD4_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "CD4_bef_ope")

data_1 = as.data.frame(list(category=df$category, CD8_bef_ope=as.numeric(as.character(df$CD8_bef_ope))))
boxplot( CD8_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = " CD8_bef_ope")

data_1 = as.data.frame(list(category=df$category, CD42CD8_bef_ope=as.numeric(as.character(df$CD42CD8_bef_ope))))
boxplot(  CD42CD8_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = " CD42CD8_bef_ope")

data_1 = as.data.frame(list(category=df$category, wbc_bef_ope=as.numeric(as.character(df$wc_bef_ope))))
boxplot( wbc_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "wbc_bef_ope")

data_1 = as.data.frame(list(category=df$category, hgb_bef_ope=as.numeric(as.character(df$hgb_bef_ope))))
boxplot( hgb_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "hgb_bef_ope")

data_1 = as.data.frame(list(category=df$category, bp_bef_ope=as.numeric(as.character(df$bp_bef_ope))))
boxplot( bp_bef_ope~category , data = data_1, col = "lightgray",pch="1", cex=0.6,xlab = "category", ylab = "bp_bef_ope")



data_2 = as.data.frame(list(category=df$category, CD42CD8_bef_ope=as.numeric(df$CD42CD8_bef_ope)))
boxplot( CD42CD8_bef_ope ~  category , data = data_1, col = "lightgray",pch="1", cex=2,xlab = "category", ylab = "cd4")

 cor.test(as.numeric(as.character(df$CD4_bef_ope)),as.numeric(as.character(df$wc_bef_ope)))


qplot(CD4_bef_opde, wc_bef_ope, data = df, geom = c("point", "smooth"),span = 0.2)

boxplot( CD4_min ~  category , data = data_1, col = "lightgray",pch="1", cex=2,xlab = "category", ylab = "cd4")


bos

data_1 = as.data.frame(list(category=df$category, CD4_min=factor(df$CD4_min)))


##·½²î·ÖÎö
data_1[!is.na(data_1$CD4_bef_ope),]
aov_1 = aov(CD4_bef_ope~category,data=data_1)dat
summary(aov)
