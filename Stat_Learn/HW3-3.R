setwd('/Users/StanleyLIn/Downloads/upload 3a')
load('phonetrain.rdata')
load('phonetest1.rdata')

pgm_train=function(class,x)
{
 	return_value=list()
 	for(aclass in class)
 	{
 		ret=list(mu1=0,sigma1=matrix(0,ncol=3,nrow=3),prec1=matrix(0,ncol=3,nrow=3),detsig_log=c(),N1=0)
 		ret$mu1=colMeans(x[[aclass]])
 		ret$sigma1=cov(x[[aclass]])
 		ret$prec1=solve(cov(x[[aclass]]))
 		ret$detsig_log=	determinant(cov(x[[aclass]]))$modulus[1]
 		names(ret$detsig_log)="modulus"
 		ret$N1=nrow(x[[aclass]])
 		return_value[[aclass]]=ret
 	}
 	return(return_value)
 }

pgm_predict=function(model,test)
{
	if(ncol(test)!=3)
	{
		return(NULL)
	}
	class=names(model)
	#print(model[[class[1]]])
	test=as.matrix(test)
	ret=rep(0,nrow(test))
	for(j in 1:nrow(test))
	{
		q=rep(0,6)
		for(i in 1:6)
		{
			#print(model[1]$Phoneonback$mu1)
			A=-0.5*t((test[j,]-model[[class[i]]]$mu1))%*%model[[class[i]]]$prec1%*%(test[j,]-model[[class[i]]]$mu1)
			A=as.numeric(A)
			q[i]=(1/((2*pi)^(1.5)))*(1/(abs(model[[class[i]]]$detsig_log)^(0.5)))*A
			#print(q[i])
		}
		max=q[1]
		ret[j]=1
		for(k in 2:6)
		{
			if(q[k]>=max)
			{
				ret[j]=k
				print(k)
				max=q[k]
			}
		}
		#print(ret[j])
	}
	return(ret)
}

model1=pgm_train(outclass, traindata)
pred1=pgm_predict(model1, testds1_feature)
print(pred1[1:50])
