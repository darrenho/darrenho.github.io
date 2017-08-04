import sys
sys.path.append('/usr/local/lib/python2.6/site-packages')  
sys.path.reverse()

from mpl_toolkits.mplot3d import Axes3D, axes3d
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
import matplotlib.pyplot as plt
import numpy as np

##########################
# Calling Functions
##########################
def gauss2(_x1,_x2,_y1,_y2,_sigma):
    from numpy import log,pi,exp,sqrt
    tmp = _sigma
    det_sigma = tmp[0,0]*tmp[1,1] - tmp[0,1]*tmp[1,0]
    if det_sigma <= 0:
        print "(x1,x2) =", (_x1,x2)
    sigma_inv = 1/det_sigma * np.matrix( [tmp[1,1],-tmp[0,1]
                            ,-tmp[1,0],tmp[0,0]]).reshape(2,2)
    log_ret = -log(2*pi*sqrt(det_sigma)) - (
            sigma_inv[0,0]*(_x1-_y1) **2 + sigma_inv[1,0] * (_x2-_y2)*(_x1-_y1) +
            sigma_inv[0,1]*(_x1-_y1)*(_x2-_y2) + sigma_inv[1,1]*(_x2-_y2)**2
                            )/2
    return exp(log_ret)

nx=ny=64

X = np.ndarray([nx,1])
X[:,0] = np.arange(nx)  
Y = np.ndarray([1,ny])
Y[0,:] = np.arange(ny) 

X = np.arange(nx)
Y = np.arange(ny)
X, Y = np.meshgrid(X, Y)
mu1a,mu1b    = (ny/2,nx/2)

sigma1 = np.matrix([ [6.0,0.],[0.,6.0] ])
Z = gauss2(X,Y,mu1a,mu1b,sigma1)
Z /= Z.max()
#Note this is for prior to 1.0.0 matplotlib 
plt.close()
fig = plt.figure()
#ax = fig.gca(projection='3d')
ax = Axes3D(fig)
ax.scatter([0,nx],[0,ny],[0,1],c='w',s=.0001)
surf = ax.plot_surface(X, Y, Z,rstride=2,cstride=2)
fig.savefig('inverseProblemEx1.eps')

sigma1 = np.matrix([ [15.0,0.],[0.,15.0] ])
Znew = gauss2(X,Y,mu1a,mu1b,sigma1)
Znew /= Z.max()
#Note this is for prior to 1.0.0 matplotlib 
plt.close()
fig = plt.figure()
#ax = fig.gca(projection='3d')
ax = Axes3D(fig)
ax.scatter([0,nx],[0,ny],[0,1],c='w',s=.0001)
surf = ax.plot_surface(X, Y, Znew,rstride=2,cstride=2)
fig.savefig('inverseProblemEx2.eps')

ZnewNoise = Znew + np.random.normal(0,.05,nx*ny).reshape([nx,ny])
#Note this is for prior to 1.0.0 matplotlib 
plt.close()
fig = plt.figure()
#ax = fig.gca(projection='3d')
ax = Axes3D(fig)
ax.scatter([0,nx],[0,ny],[0,1],c='w',s=.0001)
surf = ax.plot_surface(X, Y, ZnewNoise,rstride=2,cstride=2)
fig.savefig('inverseProblemEx3.eps')





