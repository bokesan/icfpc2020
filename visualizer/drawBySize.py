import sys
import numpy as np
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from ast import literal_eval
import re

f=open(sys.argv[1], "r")
contents = f.read()
lines = re.findall("\((.*?)\)", contents)
alpha = 1
for line in lines:
    points = re.findall("\[[\- \d\,]{1,}\]", line)
    index = 0
    if points:
        temp = []
        for point in points:
            temp.append(literal_eval(point))
        data = np.array(temp)
        plt.figure("image " + str(index))
        x, y = data.T        
        plt.scatter(x,y, None,None,1,None,None,None,None,alpha=alpha, linewidths=5)
        alpha = alpha*0.66
        index = index+1


plt.gca().invert_yaxis()
plt.gca().set_facecolor('xkcd:dark blue')
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))
plt.gca().xaxis.set_major_locator(MaxNLocator(integer=True))
plt.show()
