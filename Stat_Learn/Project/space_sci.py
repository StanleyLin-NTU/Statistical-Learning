from sklearn import linear_model
import numpy as np
from math import sqrt
import time
start_time=time.time()
s=np.genfromtxt('/Users/StanleyLIn/space.csv', delimiter=',',skip_header=1,usecols=[0,2,3])
print(s)
y=np.genfromtxt('/Users/StanleyLIn/space.csv', delimiter=',',skip_header=1,usecols=1)
print(y)
#print(s[:,0])
reg = linear_model.LinearRegression()
reg.fit (s,y)
print(sqrt(np.mean((reg.predict(s) - y) ** 2)))
print("--- %s seconds ---"%(time.time()-start_time))