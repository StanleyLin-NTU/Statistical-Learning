set.seed(9487)
data=read.csv("/Users/StanleyLIn/Downloads/kc_house_data.csv",header=TRUE)
library(randomForest)
data=data[,-c(1,2,15,16,17,18,19,20,21)]
print(data)
numFold = 5
RMSE=c()
MAE=c()
R_sqr=c()
cors=c()
for(i in 1:numFold)
{
	y_index=1
	start=(nrow(data)/5)*(i-1)+1
	end=(nrow(data)/5)*i
	testing = data[start:end,]
	train=data[-c(start:end),]
	a=randomForest(price~.,data=train,ntree=30,type=regression)
	pred = predict(a, newdata = testing[,-y_index])
	rmses = rmse(testing[,y_index],pred)
	MAEs=mae(testing[,y_index],pred)
	RMSE = append(RMSE,rmses)
	MAE=append(MAE,MAEs)
	print(sum((testing[,y_index]-mean(testing[,y_index]))^2))
	R_sqrs=1-(sum((testing[,y_index]-pred)^2)/sum((testing[,y_index]-mean(testing[,y_index]))^2))
	R_sqr=append(R_sqr,R_sqrs)
	corss=cor(testing[,y_index],pred)
	cors=append(cors,corss)
}

print(RMSE)
print(MAE)
print(R_sqr)
print(cors)
print(mean(RMSE))
print(mean(MAE))
print(mean(R_sqr))
print(mean(cors))
print(proc.time())