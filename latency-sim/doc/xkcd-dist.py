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
    print("dir of norm:", dir(norm))
    cdf = norm.pdf(x_axis,mu,variance)
    cdf /= np.max(cdf)/success_rate
    x_axis -= first
    #fig = plt.figure()
    fig, (ax, bx) = plt.subplots(1, 2, sharey=True, facecolor='w', gridspec_kw={'width_ratios': [3, 1]})
    #ax = fig.add_subplot(111)
    # Arrow to mark the error rate
    #ax.arrow(last, success_rate, 0, 1.0-success_rate)
    #ax.arrow(last, 1.0,          0, success_rate-1.0)
    #ax.text(last-first-0.5, (success_rate+1.0)/2, "lost", rotation=90)
    #ax.annotate
    #ax.title('Connection quality')
    #ax.bar(x_axis, cdf, width=1.0, align='center')
    ax.plot(x_axis, cdf)
    ax.set_ylim(0, 1.1)
    ax.set_xlim(0, last-first+0.5)
    ax.spines['right'].set_visible(False)
    #plt.ylabel('probability')
    #print(dir(ax.yaxis))
    #plt.xlabel('arrival time $t$')
    #ax.yaxis.tick_right()
    ax.yaxis.set_label("probability")
    ax.yaxis.set_label_position("left")
    bx.bar("lost", 1.0-success_rate, width=0.5)
    bx.spines['left'].set_visible(False)
    bx.yaxis.tick_right()
    #bx = fig.add_subplot(222)
    #bx.plot(x_axis, cdf)
    fig.subplots_adjust(bottom=0.2)
    plt.savefig('latency-distribution.png')
    plt.show()
