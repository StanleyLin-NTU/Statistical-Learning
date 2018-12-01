from sklearn import linear_model
import numpy as np
from math import sqrt
import time
start_time=time.time()
s=np.genfromtxt('/Users/StanleyLIn/wwine.csv', delimiter=',',skip_header=1)
#y=np.genfromtxt('/Users/StanleyLIn/Downloads/forestfires.csv', delimiter=',',skip_header=1,usecols=12)
print(s[:,0:-1])
reg = linear_model.LinearRegression()
reg.fit (s[:,0:-1],s[:,-1])
print(sqrt(np.mean((reg.predict(s[:,0:-1]) - s[:,-1]) ** 2)))
print("--- %s seconds ---"%(time.time()-start_time))