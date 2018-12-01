import csv
import time
from datetime import datetime
f1=open("/Users/StanleyLIn/Downloads/大數據期末報告/bda2017_ec_dataset/SalesOrder.csv")
f2=open("pair",'w')
f3=open("count",'w')
#print(f1)
User_IDs=[]
PayType=[]
DeliverType=[]
Promotion_Discount=[]
Coupon_Discount=[]
SalePage_ID=[]
Times=[]
Money=[]
Source=[]
ID_No_Duplicate=[]
Item_No_Duplicate=[]
pair=[]
R=[]
F=[]
M=[]	
count=0
counts=[]
for row in csv.DictReader(f1):
	count+=1
	print(count)
	if((row["MemberId"],row["SalePageId"]) not in pair):
		pair.append((row["MemberId"],row["SalePageId"]))
		counts.append(1)
	else:
		index=pair.index((row["MemberId"],row["SalePageId"]))
		counts[index]+=1	
	if(row["MemberId"] not in User_IDs):
		User_IDs.append(row["MemberId"])
	if(row["SalePageId"] not in SalePage_ID):
		SalePage_ID.append(row["SalePageId"])
for i in range(0,len(pair)):
	f2.write(pair[i])
	f2.write('\n')
	f3.write(counts[i])
	f3.write('\n')



