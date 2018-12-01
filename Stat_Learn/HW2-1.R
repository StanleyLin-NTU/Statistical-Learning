setwd("/Users/StanleyLIn/Downloads/uploads")
load(file="rtb2_train.rdata")
rtb2_train=rtb2_train

lm_evmax=function(y=NULL,xmat=NULL)
{
	N=nrow(xmat)
	lamda=0.001*N
	MAT=t(xmat)%*%xmat
	z= lamda*diag(ncol(MAT))
	w=(MAT+z)%*%t(xmat)%*%y
	e0=y-(xmat%*%w)
	beta=N/(t(e0)%*%e0)
	print(beta)
	beta=as.double(beta)

	print(beta)
	alpha=lamda*beta
	print(alpha)
	alpha=as.double(alpha)
	print(alpha)
	#print(alpha)
	A=alpha*diag(ncol(MAT))+beta*MAT
	#print(solve(A))
	MN=beta*solve(A)%*%t(xmat)%*%y
	eigens=eigen(beta*MAT)
	value<-eigens$values
	gama=0
	for(i in 1:nrow(A))
	{
		gama<-gama+(value[i]/(alpha+value[i]))
	}
	A_new=A
	MN_new=MN
	gama_new=gama
	e_new=y-(xmat%*%MN_new)
	alpha_new=gama_new/(t(MN_new)%*%MN_new)
	alpha_new=as.double(alpha_new)
	temp=(t(e_new)%*%e_new)/(N-gama_new)
	beta_new=(temp^(-1))
	beta_new=as.double(beta_new)
	while((abs(alpha-alpha_new)+abs(beta-beta_new))>=10^(-5))
	{
		print("RRR")
		A_new=alpha_new*diag(nrow(MAT))+beta_new*MAT		
		MN_new=beta_new*solve(A_new)%*%t(xmat)%*%y
		eigens_new=eigen(beta_new*MAT)
		value_new<-eigens_new$values
		gama_new=0
		for(i in 1:nrow(A_new))
		{
			gama_new<-gama_new+(value_new[i]/(alpha_new+value_new[i]))
		}
		e_new=y-(xmat%*%MN_new)
		alpha<-alpha_new
		alpha_new=gama_new/(t(MN_new)%*%MN_new)
		alpha_new=as.double(alpha_new)
		temp=(t(e_new)%*%e_new)/(N-gama_new)
		beta<-beta_new
		beta_new=(temp^(-1))
		beta_new=as.double(beta_new)
	}
	MNr=rep(0,nrow(solve(A_new)))
	names(MNr)=colnames(A_new)
	for(i in 1:nrow(A_new))
	{
		x<-solve(A_new)
		MNr[i]<-sqrt(x[i,i])
	}
	ret=list(mN=MN_new,mNsd=MNr,alpha=alpha,beta=beta)
	return(ret)
}
nfeat=20
rtb3 = rtb2_train[1:(nfeat+1)]
y=as.matrix(rtb3[,1])
xmat = model.matrix(paying_price~., data=rtb3)
lmev1 = lm_evmax(y, xmat)
lmev1
 
#print(rtb2_train)