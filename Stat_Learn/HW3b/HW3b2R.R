logicreg_l2_train=function(tall,xmat,debuglevel=0)
{
	#print(dim(xmat))
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
			#print(dim(t(w)))
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
			#print(sum(abs(w_new-w))/length(w))
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
       		#print(dim(new_lamda))
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
	return(ret)
}

logicreg_l2_predict=function(model,xmat)
{
	y=matrix(1,nrow=nrow(xmat),ncol=1)
	rownames(y)=rownames(xmat)
	#print(length(model$w))
	for(z in 1:nrow(xmat))
	{
		y[z,1]=1/(1+exp(-t(model$w)%*%xmat[z,]))	
	}
	#print(nrow(xmat))
	class=c()
	for(j in 1:nrow(y))
	{
		if(y[j,1]<=0.5)
		{
			class<-append(class,0)
		}
		else
		{
			class<-append(class,1)
		}
	}
	ret=list(prob=y,class=class)
	return(ret)
}
