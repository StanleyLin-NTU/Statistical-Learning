setwd("/Users/StanleyLIn/Downloads/upload-1")
load(file="rtb1_train.rdata")
rtb1_train = rtb1_train[1:1500,]
 
t_value=function(y,x)
{
	y=matrix(y, ncol=1)
	xmat=matrix(1, ncol=2, nrow=length(y))
	xmat[,2] = x
	bhead = solve(t(xmat)%*%xmat, t(xmat)%*%y)
	yhead = xmat %*% bhead
	e1=y-yhead
	var1 = sum(e1 * e1) / (length(e1)-2)
	sigma2 = solve(t(xmat)%*%xmat) * var1
	t1=bhead[2]/sqrt(sigma2[2,2])
	return(t1)
}

gen_utagmat=function(user_agent=NULL,price=NULL)
{
	utagstr=user_agent
	pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
	list2=regmatches(utagstr, gregexpr(pattern, utagstr))
 	list2=lapply(list2, unique)
 	print(list2)
	#print(price)
	if(is.null(user_agent))
	{
		answer=matrix(1,ncol=1,nrow=length(price))
		return(answer)
	}

	#print(b)
	#print(a)
	temp<-c()
	temp<-unlist(list2)
	#print(temp)
	count = sort(table(temp),decreasing=TRUE)
	ind = count>=10
	indic= count<=0.5*(length(list2))
	print(0.5*length(list2))
	count=count[ind&indic]
	featname = names(count)
	if(length(featname)==0)
	{
		answer=matrix(1,ncol=1,nrow=length(price))
		return(answer)
	}
	#print(featname
	y=price
	#print(y)
	xmat=matrix(1, ncol=length(featname), nrow=length(y))
	colnames(xmat)=featname
	#colnames(xmat)=paste("user_",featname,sep="")

	for(i in 1:length(featname))
	{
		for(j in 1:length(y))
		{
			#print(featname[i])
			temporary=unlist(list2[j])
			#print(temporary)
			if(!featname[i]%in%temporary)
			{
				xmat[j,i]=0
			}
			#print(i)
			#print(j)
		}
	}

	result=rep(NA,length(featname))
	names(result)=featname
	for(z in 1:length(featname))
	{
		tem_col=xmat[,z]
		result[z]=t_value(y,tem_col)
	}
	#print(result)
	o1 = order(abs(result), decreasing=TRUE)
	result_2=result[o1]
	ind2= abs(result_2)> 1
	result_3=result_2[ind2]
	#print(result_3)
	column_x=colnames(xmat)
	constant=matrix(1, ncol=1,nrow=length(y))
	result_mat=constant
	colnames(result_mat)="constant"
	col_count=1
	for(l in 1:length(result_3))
	{
		for (k in 1:length(featname))
		{
			#print(names(result_3[l]))
			if(column_x[k]==names(result_3[l]))
			{
				result_mat=cbind(result_mat,xmat[,k])
				col_count=col_count+1
				colnames(result_mat)[col_count]=paste("agent_",names(result_3[l]),sep="")
			}
		}
		
	}
	#print(result_mat)
	return(result_mat)
}

umat1=gen_utagmat(rtb1_train$user_agent,rtb1_train$paying_price)
print(head(umat1))
