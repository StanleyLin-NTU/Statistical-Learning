filter_ig=function(dstrain,ypos="pos",min_count=5,ig_threshold=10^(-5))
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
	ig_value=0
	#print(count)
	colpos=c()
	colname_ig=c()
	igs=c()
	#print(chis)
	for(i in 2:ncol(train))
	{
		ig_value=0
		ig_pn=0
		ig_minus=0
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

			#prob_pos_appear=pos_appear/nrow(train)
		#	print(prob_pos_appear)
			#prob_pos_absent=pos_absent/nrow(train)
		#	print(prob_pos_absent)
			#prob_neg_appear=neg_appear/nrow(train)
		#	print(prob_neg_appear)
			#prob_neg_absent=neg_absent/nrow(train)
		#	print(prob_neg_absent)
			posi=(pos_appear+pos_absent)/nrow(train)
			neg=(neg_appear+neg_absent)/nrow(train)
			appear=(pos_appear+neg_appear)/nrow(train)
			absent=(pos_absent+neg_absent)/nrow(train)
			prob_pos_appear=pos_appear/(pos_appear+neg_appear)
			prob_pos_absent=pos_absent/(pos_absent+neg_absent)
			prob_neg_appear=neg_appear/(pos_appear+neg_appear)
			prob_neg_absent=neg_absent/(pos_absent+neg_absent)
			
			prob_all=c(appear,absent,appear,absent)
			probs=c(prob_pos_appear,prob_pos_absent,prob_neg_appear,prob_neg_absent)
			probs_pos_neg=c(posi,neg)
			#print(probs)
			for(g in 1:length(probs_pos_neg))
			{
				if(probs_pos_neg[g]>=10^(-6))
				{
					ig_pn=ig_pn+(-probs_pos_neg[g]*log2(probs_pos_neg[g]))
				}
			}
			for(k in 1:length(probs))
			{
				if(probs[k]>=10^(-6))
				{
					ig_minus=ig_minus+prob_all[k]*(-probs[k]*log2(probs[k]))
				}
			}
			ig_value=ig_pn-ig_minus
			#print(ig_value)
			if(ig_value>=ig_threshold)
			{
				colpos=append(colpos,i)
				temp_colname=colnames(train)				
				colname_ig=append(colname_ig,temp_colname[i])
				igs=append(igs,ig_value)
			}
		}
	}
	if(length(colpos)==0)
	{
		ret=list(colpos=NULL,colname=NULL,igvalue=NULL)
		return(ret)
	}
	#print(chis)
	#print(typeof(chis))
	igs=unname(igs)
	colname_ig=colname_ig[order(igs,decreasing=TRUE)]
	colpos=colpos[order(igs,decreasing=TRUE)]
	igs=sort(igs,decreasing=TRUE)
	colpos=c(colpos)
	colname_ig=c(colname_ig)
	igs=c(igs)
	ret=list(colpos=colpos,colname=colname_ig,igvalue=igs)
	return(ret)
}
