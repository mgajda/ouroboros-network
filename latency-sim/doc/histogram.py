# -*- coding: utf-8 -*-
# vim: set fileencoding=utf-8
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import mlab

np.random.seed(0)

mu = 200
sigma = 25
n_bins = 50
y = np.random.normal(mu, sigma, size=100).cumsum()

fig, ax = plt.subplots(figsize=(8, 4))

# plot the cumulative histogram
n, bins, patches = ax.hist(x, n_bins, normed=1, histtype='step',
                           cumulative=True, label='Empirical')

# Add a line showing the expected distribution.
y = mlab.normpdf(bin, mu, sigma).cumsum()
:wq
y /= y[-1]/0.8

ax.plot(bins, y, 'k--', linewidth=1.5, label='Rate of completion')

# Overlay a reversed cumulative histogram.
#ax.hist(x, bins=bins, normed=1, histtype='step', cumulative=-1,
#        label='Reversed emp.')

# tidy up the figure
ax.grid(True)
ax.legend(loc='right')
ax.set_title('Cumulative step histograms')
ax.set_xlabel('t -- delay')
ax.set_ylabel('ΔQ(t) -- Rate of completion')

plt.show()
