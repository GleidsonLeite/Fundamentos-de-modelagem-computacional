import numpy as np
from matplotlib import pyplot as plt

f = lambda x: np.sin(x * np.cos(x) - np.sin(x))

x = np.linspace(-10, 10, num=500)
y = f(x)
plt.plot(x, y, label=r"$f(x) = sin(x*cos(x)-sin(x))$")
plt.grid()
plt.xlabel(r"$x$")
plt.ylabel(r"$y=f(x)$")
plt.legend(loc="upper right")
plt.show()
