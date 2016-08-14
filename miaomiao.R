library(msm)


beta2 <- coef(o)["I(bp^2)"]
beta1 <- coef(o)["bp"]
estmax <- unname(-beta1/(2 * beta2))

coef(o)[1] + coef(o)[2] * dat$bp + coef(o)[3] * dat$bp^2