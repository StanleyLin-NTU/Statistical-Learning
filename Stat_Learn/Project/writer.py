import numpy as np
f1=open("month.txt","r")
f2=open("day.txt","r")
f3=open("hour.txt","r")
f7=open("weekday.txt","r")
A=f1.read().splitlines()
B=f2.read().splitlines()
C=f3.read().splitlines()
D=f7.read().splitlines()
f4=open("freq_m","w")
f5=open("freq_d","w")
f6=open("freq_h","w")
f8=open("freq_wd","w")
random_x = [1,2,3,4,5,6,7,8,9,10,11,12]
month_freq=np.repeat(0,12)

day_freq=np.repeat(0,365)
hour_freq=np.repeat(0,24)
week_freq=np.repeat(0,7)
month_freq=month_freq.tolist()
day_freq=day_freq.tolist()
hour_freq=hour_freq.tolist()
week_freq=week_freq.tolist()
for i in range(0,len(A)):
	month_freq[int(A[i])-1]+=1
for i in range(0,len(B)):
	day_freq[int(B[i])-1]+=1
for i in range(0,len(C)):
	hour_freq[int(C[i])-1]+=1
for i in range(0,len(D)):
	week_freq[int(D[i])-1]+=1
for i in range(0,12):
	month_freq[i]=str(month_freq[i])
for i in range(0,365):
	day_freq[i]=str(day_freq[i])
for i in range(0,24):
	hour_freq[i]=str(hour_freq[i])
for i in range(0,7):
	week_freq[i]=str(week_freq[i])
for item in month_freq:
	f4.write(item)
	f4.write("\n")
for item in day_freq:
	f5.write(item)
	f5.write("\n")
for item in hour_freq:
	f6.write(item)
	f6.write("\n")
for item in week_freq:
	f8.write(item)
	f8.write("\n")				
print(month_freq)
print(day_freq)
print(hour_freq)
