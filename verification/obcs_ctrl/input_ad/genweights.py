import numpy as np

nx,ny,nr,nt=64,64,8,1

err = 0.1

# single prec ieee-be
wt32 = err*np.ones((nt,nr,ny,nx),dtype='>f4')
wt32.tofile('errorTtot.err')

# double prec ieee-be
wt64 = err*np.ones((nt,nr,ny,nx),dtype='>f8')
wt64.tofile('errorTtot.err64')
