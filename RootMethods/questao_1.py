import numpy as np
from matplotlib import pyplot as plt

f = lambda x: np.sin(x * np.cos(x) - np.sin(x))

x = np.linspace(-7, -7.5, num=500)
y = f(x)

root = -7.275
f_root = f(root)

plt.plot(x, y, label=r"$f(x)$", linewidth=2.5)
plt.plot(
    root,
    f_root,
    "ms",
    label=rf"Approximate Function root $x={root:.03f}$, $f({root:.03f})={f_root:.03f}$",
    markersize=10,
)
plt.grid()
plt.title(r"$f(x) = sin(x.cos(x)-sin(x)$")
plt.xlabel(r"$x$")
plt.ylabel(r"$y=f(x)$")
plt.legend(loc="upper right")
plt.show()
