setwd("/Users/StanleyLIn/Downloads/upload_hw4")
load('hw4ds1.rdata')

filter_chisq=function(dstrain,ypos="pos",min_count=5,chi_threshold=10^(-5))
{
	train=dstrain
	pos=as.matrix(train[,1])
	feat=as.matrix(train[,2:ncol(train)])
	#print(ypos)
	#print(train[,1])
	for(i in 1:nrow(pos))
	{
		#print(as.character(train[i,1]))
		#print(ypos)
		if(as.character(pos[i,1])==ypos)
		{
		#	print("RRR")
			pos[i,1]=0
		}
		else
		{
			#print("pos")
			pos[i,1]=1
		}
	}
	feat[feat>0]<-1
	#print(feat)
	#print(train)
	count=colSums(train[,2:ncol(train)]!=0)
	chi_value=0
	#print(count)
	colpos=c()
	colname_chi=c()
	chis=c()
	#print(chis)
	for(i in 4111:4111)
	{
		#print(i)
		if(count[i-1]>min_count)
		{
			pos_appear=0
			pos_absent=0
			neg_appear=0			
			neg_absent=0
			for(j in 1:nrow(train))
			{
				if(pos[j,1]==1 && feat[j,i-1]==1)
				{
					pos_appear=pos_appear+1
				}
				if(pos[j,1]==1 && feat[j,i-1]==0)
				{
					pos_absent=pos_absent+1
				}
				if(pos[j,1]==0 && feat[j,i-1]==1)
				{
					neg_appear=neg_appear+1
				}
				if(pos[j,1]==0 && feat[j,i-1]==0)
				{
					neg_absent=neg_absent+1
				}
			}	
			mat=matrix(0,nrow=2,ncol=2)
			mat[1,1]=pos_appear
			mat[2,1]=neg_appear			
			mat[1,2]=pos_absent
			mat[2,2]=neg_absent
			R=chisq.test(mat)
			chi_value=R$statistic
			if(is.na(chi_value))
			{
				chi_value=0
			}
			if(chi_value>=chi_threshold)
			{
				colpos=append(colpos,i)
				temp_colname=colnames(train)				
				colname_chi=append(colname_chi,temp_colname[i])
				chis=append(chis,chi_value)
			}
		}
	}
	if(length(colpos)==0)
	{
		ret=list(colpos=NULL,colname=NULL,chistat=NULL)
		return(ret)
	}
	#print(chis)
	#print(typeof(chis))
	chis=unname(chis)
	colname_chi=colname_chi[order(chis,decreasing=TRUE)]
	colpos=colpos[order(chis,decreasing=TRUE)]
	chis=sort(chis,decreasing=TRUE)
	colpos=c(colpos)
	colname_chi=c(colname_chi)
	chis=c(chis)
	ret=list(colpos=colpos,colname=colname_chi,chistat=chis)
	return(ret)
}





testfold = 1
dstrain1 = hw4ds1[-folds[[testfold]],]
#print(nrow(dstrain1))
#print(head(dstrain1[,1],10000))
out1=filter_chisq(dstrain1)
print(head(out1$colname, n=15))
 
