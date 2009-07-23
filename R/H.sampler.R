`H.sampler` <-
function(x="community matrix (spp=col,obs=row)",n="sample size vector",nit="number of iterations to use",base=exp(1)){ 

out=array(NA,c(nrow(x),length(n)))
p=x*0 #probability matrix
for (i in 1:nrow(x)){
	p[i,]=x[i,]/apply(x,1,sum)[i]
	}
sv=1:ncol(x) #species vector

for (h in 1:nrow(x)){
	iout=numeric(length(n))
	for (i in 1:length(n)){
		jout=numeric(nit)
		if (n[i]==0){iout[i]=0}
		else {
		for (j in 1:nit){
obs=sample(sv,n[i],replace=TRUE,prob=p[h,])
obs=count(sv,obs)
obs=obs/sum(obs)
lnobs=obs*0
for (k in 1:length(obs)){
	if (obs[k]==0){lnobs[k]=0}else{lnobs[k]=log(obs[k],base)}
	}
H=-sum(obs*lnobs)
jout[j]=H
			}	
iout[i]=mean(jout)}
	}
out[h,]<-iout
	}

names=numeric(length(n))
for (i in 1:length(n)){
	names[i]=paste("N",n[i],sep="")
	}
colnames(out)<-names
return(out)
}

