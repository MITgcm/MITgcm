import numpy as np

nx,ny,nr,nt=80,42,3,1

# single prec ieee-be
wt32 = np.ones((nt,nr,ny,nx),dtype='>f4')
wt32.tofile('ones_32b.bin')

# double prec ieee-be
wt64 = np.ones((nt,nr,ny,nx),dtype='>f8')
wt64.tofile('ones.prec64')
