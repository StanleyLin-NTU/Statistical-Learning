options(scipen=10)

gpredict=function(dftrain=NULL,dftest=NULL)
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
	x<-c(dftrain[,1])
	mua=c()
	mua<-append(mua,mean(x))
	mub=c()
	for(i in 2:(nfeature+1))
	{
		z<-c(dftrain[,i])
		u<-mean(z)
		mub <- append(mub, u, i-1)
	}
	cov_matrix<-cov(dftrain)
	s_ab=c()
	for(i in 2:(nfeature+1))
	{
		s_ab<-append(s_ab,cov_matrix[1,i],i-1)	
	}
	s_bb=cov_matrix[2:(nfeature+1),2:(nfeature+1)]
	if(!is.null(dftest))
	{
		a1=matrix(s_ab,nrow=1,ncol=nfeature)%*%solve(s_bb)
		a2=matrix(0,nrow=ntest,ncol=nfeature)
		for (i in 1:ntest)
		{
			for(j in 1:nfeature)
			{
				a2[i,j]<-dftest[i,j]-mub[j]
			}
		}
		a3=matrix(0,nrow=1,ncol=ntest)
		a3= a1%*%t(a2)
		predict=c()
		for(i in 1:ntest){
			predict<-append(predict,mua+a3[i])
		}
		#mae1a=mean(abs(df1_test1y[,1]-predict))
		#print(mae1a)
	}
	else
	{
		predict=NULL
	}
	ret=list(mua,mub,s_ab,s_bb,predict)
	return(ret)
}
