rf_carton=function(dsall,folds,testfold,ypos="pos",chi_threshold=0.1,grid_length=20,grid_type="loglinear",rfntree=500,debuglevel=0)
{
	pos_present=ypos	
	
	#print((rownames(dsall)))
	t_fold=folds[[testfold]]
	tune_fold_index=0
	if(testfold==1)
	{
		tune_fold_index=10
	}
	else
	{
		tune_fold_index=testfold-1
	}
	print(tune_fold_index)
	test_data <- dsall[folds[[testfold]],]
  	tune_data <- dsall[folds[[tune_fold_index]],]
  	train_data <- dsall[-folds[[testfold]],]
  	sub_train_data <- dsall[-c(folds[[tune_fold_index]],folds[[testfold]]),]
	#tune_fold=folds[[tune_fold_index]]
	#print(tune_fold)
	#print(sub_train_fold)
#	test_data=dsall[t_fold,]
#	tune_data=dsall[tune_fold,]
#	sub_train_data=dsall[-c(tune_fold,t_fold),]
#	print(nrow(sub_train_data))
#	train_data=rbind(sub_train_data,tune_data)
	
	print(nrow(train_data))
#	test_data[,1]=as.numeric(test_data[,1] == ypos)
#	tune_data[,1]=as.numeric(tune_data[,1] == ypos)
#	sub_train_data[,1]=as.numeric(sub_train_data[,1] == ypos)
	chi_limit=chi_threshold
	filters=filter_chisq(sub_train_data,ypos=pos_present,chi_threshold=chi_limit)
	filter=filters$colpos
	print(nrow(dsall))
	print(nrow(test_data))
	print(nrow(tune_data))
	print(nrow(sub_train_data))
	sub_train_x=sub_train_data[,filter]
  	sub_train_y=as.numeric(sub_train_data[,1] == ypos)
  	tune_xtest=tune_data[,filter]
  	tune_ytest=as.numeric(tune_data[,1] == ypos)
  	print(sub_train_x)
	#print(filter$colpos)
	m_min=2
	m_max=length(filter)

	grids=c()
	#print(sub_train_fold)
	if(grid_type=="equal")
	{
		grids=unique(round(seq(m_min,m_max,length=grid_length)))
	}
	if(grid_type=="loglinear")
	{
		grids=unique(round(exp(seq(log(m_min), log(m_max),length=grid_length))))
	}
	#print(grids)
	
	library(randomForest)
	tune_F=c()
	best_F=-1
	best_mtry=0
	result=0
	#print(length(grids))
	#print(tune_data[,1])
	for(i in 1:length(grids))
	{
		#print("RRRRR")
		a=randomForest(sub_train_x,y=as.factor(sub_train_y),xtest=tune_xtest,ytest=as.factor(tune_ytest),mtry=grids[i],ntree=rfntree,type=classification)
		con=a$test$confusion
		#print(con)
		#print(con[1,1])
		#print(con[1,2])
		#print(con[2,1])
		recall=con[2,2]/(con[2,2]+con[2,1])
		precision=con[2,2]/(con[2,2]+con[1,2])
		F=(2*precision*recall)/(precision+recall)
		print(F)
		tune_F=append(tune_F,F)
		if(F>best_F)
		{
			best_F=F
			best_mtry=grids[i]
		}
		#print(">>>>")
	}
	filter_trains=filter_chisq(train_data,ypos=pos_present,chi_threshold=chi_limit)
	filter_train=filter_trains$colpos
	print(train_data)
	train_x=train_data[,filter_train]
  	train_y=as.numeric(train_data[,1] == ypos)
  	test_xtest=test_data[,filter_train]
  	test_ytest=as.numeric(test_data[,1] == ypos)
	result=randomForest(train_x,y=as.factor(train_y),xtest=test_xtest,ytest=as.factor(test_ytest),mtry=best_mtry,ntree=rfntree,type=classification)
	con=result$test$confusion
	print(con)
	recall=con[2,2]/(con[2,2]+con[2,1])
	precision=con[2,2]/(con[2,2]+con[1,2])
	print(precision)
	print(recall)
	F=(2*precision*recall)/(precision+recall)
	print(F)
	temp=list(precision=precision, recall=recall, f1=F)
 	 #return the ans
    ret=list(mgrids=grids,f1_all=tune_F,best_m=best_mtry,test=temp,fselect=filter_trains)
}



filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 1e-5) {
    nugrams = ncol(dstrain) #number of unigram+1
    chiall = rep(-1, nugrams) #the first column is always -1, and will not be selected.
    yvec = as.numeric(dstrain[,1]==ypos)
    options(warn = -1) #silence the warning
    for(ii in 2:nugrams) {
        tmp1=cbind(yvec, as.numeric(dstrain[,ii]>0))
        tmp1a=table(tmp1[,1], tmp1[,2])
        
        if(nrow(tmp1a)<2 | ncol(tmp1a)<2) {
            #stop("tmp1a table dimension too small!")
            chiall[ii] = 0
        } else if(sum(tmp1[,2])<=min_count) {
            chiall[ii] = 0
            #cat("feature", ii, "count too low, skip\n")
        } else {
            tmp2=chisq.test(tmp1a, correct=FALSE)
            chiall[ii] = tmp2$statistic
        }    
    }
    options(warn = 0) #turn the warnings back on
    o1 = order(chiall, decreasing=TRUE)
    
    tmpind1 = chiall[o1] > chi_threshold
    if(sum(tmpind1) ==0) {
        #cat("We have not features selected. The maximum value of chisq test is ", max(chiall), "\n")
        return(list(colpos = NULL, colname=NULL, chistat=NULL))
    } else {
        o2=o1[tmpind1]
        retname = names(dstrain)[o2]
        return(list(colpos = o2, colname=retname, chistat=chiall[o2]))
    }
}


setwd('/Users/StanleyLIn/Downloads/upload_hw5-1')
load('hw5ds1.rdata')
     
set.seed(5555)
rftest=rf_carton(ds1, cvfold, testfold=1, debuglevel=0)
print(rftest)
