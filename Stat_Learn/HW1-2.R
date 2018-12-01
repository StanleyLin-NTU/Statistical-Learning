mle_update=function(mu1,cov1,nrow,xn)
{
	result=list(mu=rep(0,length(mu1)),s=matrix(0,nrow=(nrow(cov1)),ncol=ncol(cov1)),n=nrow+1)
	#print(dim(new_s))
	for(i in 1:length(mu1))
	{
		result$mu[i]=mu1[i]+((1/(nrow+1))*(xn[i]-mu1[i]))
	}
	result$s=(1/(nrow+1))*(xn-result$mu)%*%t(xn-result$mu)+(nrow/(nrow+1))*cov1+(nrow/((nrow+1)^3))*(xn-mu1)%*%t(xn-mu1)
	return(result)
}
