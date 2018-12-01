set.seed(9487)
data=read.csv("/Users/StanleyLIn/Downloads/forestfires.csv")
library(randomForest)
library(Metrics)
data=data[,-c(3,4)]
numFold = 5
RMSE=c()
MAE=c()
R_sqr=c()
R_cor=c()
for(i in 1:numFold){
	start=(nrow(data)/5)*(i-1)+1
	end=(nrow(data)/5)*i
	testing = data[start:end,]
	train=data[-c(start:end),]
	#print(train)
	a=randomForest(area~.,data=train,ntree=30,type=regression)
	pred = predict(a, newdata = testing[,1:ncol(testing)-1])
	#print(pred)
	rmses = rmse(testing[,ncol(testing)],pred)
	MAEs=mae(testing[,ncol(testing)],pred)
	RMSE = append(RMSE,rmses)
	MAE=append(MAE,MAEs)
	#print(sum((testing[,ncol(testing)]-mean(testing[,ncol(testing)]))^2))
	R_sqrs=1-(sum((testing[,ncol(testing)]-pred)^2)/sum((testing[,ncol(testing)]-mean(testing[,ncol(testing)]))^2))
	R_sqr=append(R_sqr,R_sqrs)
	R_cors=cor(testing[,ncol(testing)],pred)
	print(R_cors)
	R_cor=append(R_cor,R_cors)
}
print(RMSE)
print(MAE)
print(R_sqr)
print(mean(RMSE))
print(mean(MAE))
print(mean(R_sqr))
print(R_cors)
print(mean(R_cors))
print(proc.time())