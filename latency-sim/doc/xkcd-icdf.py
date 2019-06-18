#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# vim: set fileencoding=utf-8

from matplotlib.patches import ConnectionPatch
from matplotlib import pyplot as plt
import numpy as np
import scipy
import scipy.stats
from scipy.stats import norm
 
with plt.xkcd():
    first= -3
    last =  3
    success_rate = 0.8
    x_axis = np.arange(first, last, 0.001)
    mu = 0
    variance = 1
    cdf = norm.cdf(x_axis,mu,variance)
    cdf /= np.max(cdf)/success_rate
    x_axis -= first
    fig = plt.figure()
    ax = fig.add_subplot(111)
    # Arrow to mark the error rate
    #ax.arrow(last, success_rate, 0, 1.0-success_rate)
    #ax.arrow(last, 1.0,          0, success_rate-1.0)
    con=ConnectionPatch(xyA=(last-first, success_rate),
                        xyB=(last-first, 1.0         ),
                        coordsA='data', coordsB='data',
                        axesA=ax, axesB=ax,
                        arrowstyle='<->', mutation_scale=20)
    ax.add_artist(con)
    ax.text(last-first-0.5, (success_rate+1.0)/2, "lost", rotation=90)
    #ax.annotate
    ax.plot(x_axis, cdf)
    ax.set_ylim(0, 1.1)
    ax.set_xlim(0, last-first+0.5)
    plt.xlabel('delay $t$')
    plt.ylabel('rate of messages arrived $ΔQ$')
    plt.title('Connection quality')
    fig.subplots_adjust(bottom=0.2)
    plt.savefig('completion-rate.png')
    plt.show()
