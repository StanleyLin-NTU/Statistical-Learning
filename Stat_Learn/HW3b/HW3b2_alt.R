setwd("/Users/StanleyLIn/Downloads/upload 3b")
load(file="o_cost_train.rdata")

logicreg_l2_train=function(tall,xmat,debuglevel=0)
{
	print(dim(xmat))
	h=c()
	lamda=nrow(xmat)*0.0005
	w=solve(lamda*diag(nrow(t(xmat)%*%xmat))+t(xmat)%*%xmat)%*%t(xmat)%*%tall
	param_tol=10^(-5)
	#print(param_tol)
	for(i in 1:50)
	{
		R=c()
		#gama=lamda*t(w)%*%w
		count=0
		#print(i)
		for(j in 1:20)
		{
			#print(j)
			y=c()
			print(dim(t(w)))
			#print((xmat[1,]))
			for(z in 1:nrow(xmat))
			{
				y=append(y,1/(1+exp(-t(w)%*%xmat[z,])))
			}
			#print(y)
			R_temp=c()
			for(i in 1:length(y))
			{
				R_temp=append(R_temp,y[i]*(1-y[i]))
			}
			R=diag(R_temp)
			count=count+1
			h_temp=t(xmat)%*%R%*%xmat
			h=h_temp+lamda*diag(nrow(h_temp))
			#print(dim(h))
			gradient=lamda*w+t(xmat)%*%(y-tall)
			w_new=w-(solve(h)%*%gradient)
			print(sum(abs(w_new-w))/length(w))
			if(sum(abs(w_new-w))/length(w)<=param_tol)
			{
				w=w_new
				break
			}
			w=w_new
		}
		for(k in 1:20)
		{
			gama=0
			#print(k)
			temp_eigen = eigen(t(xmat)%*%R%*%xmat)
	        lambda_i = temp_eigen$values
	        for (i in 1:length(lambda_i)) {
		        gama = gama + (lambda_i[i]/ (lamda+lambda_i[i]) )
	        }

       		new_lamda = gama/(t(w) %*% w)
       		print(dim(new_lamda))
       		new_lamda = new_lamda[1,1]
			if(abs(new_lamda-lamda)<=param_tol)
			{
				lamda=new_lamda
				break
			}
			#print(abs(new_lamda-lamda))
			lamda=(new_lamda)
			
		}
		if(count<=2)
		{
			break
		}
	}
	w_sd=diag(solve(t(xmat)%*%R%*%xmat+lamda*diag(nrow(t(xmat)%*%R%*%xmat))))
	w_sd=sqrt(w_sd)
	ret=list(w=w,w_sd=w_sd,lambda=lamda,M=length(w),N=nrow(xmat))
}

logicreg_l2_predict=function(model,xmat)
{
	u=c()
	print(length(model$w))
	for(i in 1:nrow(xmat))
	{
		u=append(u,t(model$w)%*%xmat[i,])
	}	
	#print(nrow(xmat))
	#print(length(u))
	R_temp=c()
	for(i in 1:length(u))
	{
		R_temp=append(R_temp,1/(1+exp(-u[i]))*1/(1+exp(-(1-u[i]))))		
	}
	R=diag(R_temp)
	#print(dim(t(xmat)))
	#print(dim(R))
	sigma=c()
	for(k in 1:nrow(xmat))
	{
		sigma=append(sigma,t(xmat[i,])%*%as.matrix(model$w_sd)%*%xmat[i,])
	}
	print(sigma)
	K=(1+(pi*(sigma^2)/8))^0.5
	P=1/(1+exp(-K*u))
	print(P)
}
#Sample 1
dm_train_t = as.numeric(ds4a_train[,1] == "pos")
tall=as.matrix(dm_train_t)
xmat = model.matrix(~f_past+g1+g2+g3+g4+g5+g6+g7+g8+g9+g10,
data=ds4a_train[,-1])
model1 = logicreg_l2_train(tall, xmat, debuglevel=0)
#perform prediction
xmattest1 = model.matrix(~f_past+g1+g2+g3+g4+g5+g6+g7+g8+g9+g10,
data=ds4a_train[,-1])
logicpred1 = logicreg_l2_predict(model1, xmattest1)