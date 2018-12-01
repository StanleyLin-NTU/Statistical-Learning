options(scipen=10)

gpredict=function(dftrain=NULL,dftest=NULL)
{
	if(missing(dftest))
	{
		nfeature=(ncol(dftrain)-1)
		ntest=nrow(dftrain)
	}
	else
	{	
		if(is.null(dftest))
		{
			nfeature=(ncol(dftrain)-1)
			ntest=nrow(dftrain)
		}
		else
		{
			nfeature=ncol(dftest)
			ntest = nrow(dftest)
			if(ncol(dftrain)!=(nfeature+1))
			{
				return(NULL)
			}
		}
	}
	ret=list(mua=0, mub=rep(0,nfeature),s_ab = matrix(0, nrow=1, ncol=nfeature),s_bb=matrix(0, nrow=nfeature, ncol=nfeature),predict=rep(0, ntest))
	x<-c(dftrain[,1])
	ret$mua=mean(x)

	for(i in 2:(nfeature+1))
	{
		z<-c(dftrain[,i])
		u<-mean(z)
		ret$mub[i-1]<-u
	}
	cov_matrix<-cov(dftrain)
	for(i in 2:(nfeature+1))
	{
		ret$s_ab[1,i-1]<-cov_matrix[1,i]	
	}
	ret$s_bb=cov_matrix[2:(nfeature+1),2:(nfeature+1)]
	if(is.null(dftest)||missing(dftest))
	{
		ret$predict<-NULL
		return(ret)
	}
	else
	{
		a1=ret$s_ab%*%solve(ret$s_bb)
		a2=matrix(0,nrow=ntest,ncol=nfeature)
		for (i in 1:ntest)
		{
			for(j in 1:nfeature)
			{
				a2[i,j]<-dftest[i,j]-ret$mub[j]
			}
		}
		a3=matrix(0,nrow=1,ncol=ntest)
		a3= a1%*%t(a2)
		for(i in 1:ntest){
			ret$predict[i]<-(ret$mua+a3[i])
		}
		return(ret)
	}	
}
