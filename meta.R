t1=metacont(s1,m1,sd1,s2,m2,sd2,studlab=study,data=d1[1:8,],sm="MD",comb.random=FALSE,comb.fixed=TRUE,label.e="CMR",label.c="2DTTE")
t2=metacont(s1,m1,sd1,s2,m2,sd2,studlab=study,data=d1[12:16,],sm="MD",comb.random=FALSE,comb.fixed=TRUE,label.e="CMR",label.c="3DE")


t1=metacont(s1,m1,sd1,s2,m2,sd2,studlab=study,data=d1[1:8,],sm="MD",label.e="CMR",label.c="2DTTE")
t2=metacont(s1,m1,sd1,s2,m2,sd2,studlab=study,data=d1[10:16,],sm="MD",label.e="CMR",label.c="3DE")

study = read.table('c://users/tienan/Documents/Data/meta_crm.txt',header=TRUE)



study = read.table('meta_crm.txt',header=TRUE)

forest(t2)
t2=metacont(s1,m1,sd1,s2,m2,sd2,studlab=study,data=study,sm="MD",comb.random=TRUE,comb.fixed=FALSE,label.e="CMR",label.c="2DTTE")
