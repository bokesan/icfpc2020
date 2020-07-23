import sys
import numpy as np
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from ast import literal_eval
import re


f=open(sys.argv[1], "r")
contents = f.read()

lines = re.findall("\((.*?)\)",contents)

for line in lines:
    points = re.findall("\[[\- \d\,]{1,}\]", line)
    index = 0
    if points:
        temp = []
        for point in points:
            temp.append(literal_eval(point))
        data = np.array(temp)
        plt.figure(index)
        x, y = data.T
        plt.scatter(x,y)
        plt.gca().invert_yaxis()
        plt.gca().invert_xaxis()
       
        index = index+1
plt.gca().yaxis.set_major_locator(MaxNLocator(integer=True))
plt.gca().xaxis.set_major_locator(MaxNLocator(integer=True))
plt.show()
