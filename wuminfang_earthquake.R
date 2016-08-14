library(mgcv)
data=read.csv("wuminfang_earthquake.csv",header=TRUE)

data$Time <- relevel(as.factor(data$Time ), ref = "1")
data$Income_level <- relevel(as.factor(data$Income_level), ref = "1")

data$Houses_destoryed <- relevel(as.factor(data$Houses_destoryed), ref = "4")
data$Houses_damaged <- relevel(as.factor(data$Houses_damaged), ref = "4")
data$Economic_losses_level <- relevel(as.factor(data$Economic_losses_level), ref = "4")

data$Casualties=log(data$Casualties)



lm1=gam(Casualties~Number + Time + Income_level +  Houses_destoryed  + Houses_damaged + Economic_losses_level + Depth_km+Magnitude+Intensity+ s(GDP_per_capita)+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)

lm1=gam(Casualties~ Time+Magnitude+Intensity+ Income_level+Number+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)
lm1=gam(Deaths~ Time+Magnitude+Intensity+ Income_level+Number+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)
lm1=gam(Injuries~ Time+Magnitude+Intensity+ Income_level+Number+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)
lm1=gam(Injuries~ Time+Magnitude+Intensity+ Income_level+Number+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)




lm1=gam(Casualties~ s(GDP_per_capita)+s(Population_density)+s(Economic_losses),family=poisson(link = "log"),data=data)


par(mfrow=c(3,1))

plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam
