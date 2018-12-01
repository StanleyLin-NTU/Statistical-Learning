library(ggplot2)
#Generate the data
A=read.table("/Users/StanleyLIn/Desktop/專題研究/freq_h")
print(A)
names(A)<-NULL
print(A)
df <- data.frame(gp = 1:24,y = A)
#Set Aesthetic attributes and assign Geometric objects
ggplot(df, aes(x = gp, y = A))+geom_line(color="#CAA661")+ scale_x_discrete(limits=c(1:24))+ggtitle("2013_Hours", subtitle = NULL)
