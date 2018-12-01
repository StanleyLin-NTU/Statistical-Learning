from sklearn import linear_model
import numpy as np
from math import sqrt
import time
start_time=time.time()
s=np.genfromtxt('/Users/StanleyLIn/Downloads/forestfires.csv', delimiter=',',skip_header=1,usecols=(0,1,4,5,6,7,8,9,10,11))
y=np.genfromtxt('/Users/StanleyLIn/Downloads/forestfires.csv', delimiter=',',skip_header=1,usecols=12)
reg = linear_model.LinearRegression()
reg.fit (s,y)
print(sqrt(np.mean((reg.predict(s) - y) ** 2)))
print("--- %s seconds ---"%(time.time()-start_time))