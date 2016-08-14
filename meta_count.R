dat = escalc(measure="RR",ai=tpos,bi=tneg,ci=cpos,di=cneg,data=dat.bcg,append=TRUE)


res = rma(yi,vi,data=dat)

confint(res)

data_eye = read.table("C:/users/tienan/Documents/Data/eye_tired.txt",header=TRUE)

dat_eye=escalc(measure="RR",ai=TP,bi=TN,ci=FP,di=FN,data=data_eye,append=TRUE)

