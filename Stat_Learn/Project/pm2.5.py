from sklearn import linear_model
import pandas as pd
import numpy as np
from math import sqrt
dat = pd.read_csv('/Users/StanleyLIn/Downloads/PRSA_data_2010.1.1-2014.12.31.csv')
import time
start_time=time.time()
A=list(dat)
delete=[0,1,2,3,4,9]
for i in range(0,10):
	if(i in delete):
		dat=dat.drop(A[i],axis=1)

#dat.drop(names[A],axis=1)
dat=dat.dropna(axis=0)
dat=np.asarray(dat)

print(dat.shape)
#print(dat)
s=dat[:,range(1,7)]
y=dat[:,0]
print(s)
#print(s[:,0])
reg = linear_model.LinearRegression()
reg.fit (s,y)
print(sqrt(np.mean((reg.predict(s) - y) ** 2)))
print("--- %s seconds ---"%(time.time()-start_time))

