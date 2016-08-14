risk10 = function(Sex,Age,Race,TC,HC,SBP,THBP,Diabetes,Smoker){
#Race Sex 
#1 0 AAW
#1 1 AAM
#0 0 WHW
#0 1 WHM
#Sex = 'M';Age = 45;Race = 'AA';TC = 135;HC=46;SBP=100;THBP=0;Diabetes=1;Smoker=1
	THBP_0=abs(THBP-1)
	lnage=log(Age)
	lntot=log(TC)
	lnhdl=log(HC)
	trlnsbp=log(SBP)*THBP
	ntlnsbp = log(SBP)*THBP_0
	age2=lnage*lnage
	agetc=lnage*lntot
	agehdl=lnage*lnhdl
	agetsbp=lnage*trlnsbp
	agentsbp=lnage*ntlnsbp
	agesmoke=lnage*Smoker
	agedm=lnage*Diabetes
	
	
	B16 = lnage
	B17 = lntot
	B18 = lnhdl
	B19 = trlnsbp
	B20 = ntlnsbp 
	B21 = age2
	B22 = agetc
	B23 = agehdl
	B24 = agetsbp
	B25 = agentsbp
	B26 = agesmoke
	B27 = agedm 
	E12 = Smoker
	E11 = Diabetes
	s0_10 = c(0.95334,0.96652,0.89536,0.91436)
	mnxb = c(86.6081,-29.1817,19.5425,61.1816)
	AAW = 17.1141*B16+0.9396*B17+(-18.9196*B18)+4.4748*B23+29.2907*B19+(-6.4321*B24)+27.8197*B20+(-6.0873*B25)+0.6908*E12+0.8738*E11
	WHW = (-29.799*lnage)+4.884*age2+13.54*lntot+(-3.114*agetc)+(-13.578*lnhdl)+3.149*agehdl+2.019*trlnsbp+1.957*ntlnsbp+7.574*Smoker+(-1.665*agesmoke)+0.661*Diabetes
	AAM = 2.469*lnage+0.302*lntot+(-0.307*lnhdl)+1.916*trlnsbp+1.809*ntlnsbp+0.549*Smoker+0.645*Diabetes
	WHM = 12.344*lnage+11.853*lntot+(-2.664*agetc)+(-7.99*lnhdl)+1.769*agehdl+1.797*trlnsbp+1.764*ntlnsbp+7.837*Smoker+(-1.795*agesmoke)+0.658*Diabetes
	predict = c(AAW,WHW,AAM,WHM)
	type = paste(Race,Sex,sep = "")
	cvd_predict = 0
	for (i in 1:4){
		cvd_predict[i] = 1-s0_10[i]^(exp(predict[i]-mnxb[i]))
	}
	r = cvd_predict[predict==eval(parse(text=type))]
	return (r)
}

asignif = function(x,cutoff,a,b){
	if (x>cutoff){
		y= a
	}else{
		y=b
	}
	return(y)
}