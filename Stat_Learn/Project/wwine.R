set.seed(9487)
data=read.table("/Users/StanleyLIn/Downloads/winequality-white.csv",sep=";",header=TRUE)
library(randomForest)
train=data[,]
print(train)
numFold = 5
RMSE=c()
MAE=c()
R_sqr=c()
for(i in 1:numFold){
	start=(nrow(data)/5)*(i-1)+1
	end=(nrow(data)/5)*i
	testing = data[start:end,]
	train=data[-c(start:end),]

	a=randomForest(quality~.,data=train,ntree=30,type=regression)
	pred = predict(a, newdata = testing[,1:ncol(testing)-1])
	rmses = rmse(testing[,ncol(testing)],pred)
	MAEs=mae(testing[,ncol(testing)],pred)
	RMSE = append(RMSE,rmses)
	MAE=append(MAE,MAEs)
	print(sum((testing[,ncol(testing)]-mean(testing[,ncol(testing)]))^2))
	R_sqrs=1-(sum((testing[,ncol(testing)]-pred)^2)/sum((testing[,ncol(testing)]-mean(testing[,ncol(testing)]))^2))
	R_sqr=append(R_sqr,R_sqrs)
}
write.csv(data,file="wwine.csv",row.names=FALSE)
print(RMSE)
print(MAE)
print(R_sqr)
print(mean(RMSE))
print(mean(MAE))
print(mean(R_sqr))
print(proc.time())