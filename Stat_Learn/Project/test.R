nfeature=ncol(dftest)
ntest = nrow(dftest)
ret=list(mua=0, mub=rep(0,nfeature),
s_ab = matrix(0, nrow=1, ncol=nfeature),
s_bb=matrix(0, nrow=nfeature, ncol=nfeature),
predict=rep(0, ntest))
return(ret)

nfeature=43
ntest=50
options(scipen=10)
df1_train= read.csv('/Users/StanleyLIn/Downloads/hw1_wdata/df1_train.csv')
df1_test1= read.csv('/Users/StanleyLIn/Downloads/hw1_wdata/df1_test1.csv')
df1_test1y=read.csv('/Users/StanleyLIn/Downloads/hw1_wdata/df1_test1y.csv')
print(df1_train[1:200,1])
x<-c(df1_train[1:200,1])
print(x)
mua=c()
mua<-append(mua,mean(x))
print(mua)
typeof(mua)
try<-df1_train[1:200,]
cov_try=cov(try)
mub=c()
dim(cov_try)
for(i in 2:(nfeature+1)){
	z<-c(df1_train[1:200,i])
	print(z)
	u<-mean(z)
	mub <- append(mub, u, i-1)
	print(u)
}
print(mub)
cov_matrix<-cov(df1_train)
#print (cov_matrix)
s_ab=c()
for(i in 2:(nfeature+1)){
	s_ab<-append(s_ab,cov_try[1,i],i-1)	
}
print (s_ab)
s_bb=cov_try[2:(nfeature+1),2:(nfeature+1)]
print(s_bb)

pred=c()
a1=matrix(s_ab,nrow=1,ncol=43)%*%solve(s_bb)
#print(a1)
a2=matrix(0,nrow=ntest,ncol=43)
for (i in 1:ntest){
	#print(i)
	for(j in 1:nfeature){
		a2[i,j]<-df1_test1[i,j]-mub[j]
	}
}
dim(a2)
a3=matrix(0,nrow=1,ncol=50)
a3= a1%*%t(a2)
#print(a3)
pred=c()
for(i in 1:ntest){
	pred<-append(pred,mua+a3[i])
}
print(pred)
mae1a=mean(abs(df1_test1y[,1]-pred))
print(mae1a)
'''
