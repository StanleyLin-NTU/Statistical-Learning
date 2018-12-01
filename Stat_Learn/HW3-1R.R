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
