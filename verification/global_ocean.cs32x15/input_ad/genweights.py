import numpy as np

nx,ny,nr,nt=32*6,32,15,1

# double prec ieee-be
wt64 = np.ones((nt,nr,ny,nx),dtype='>f8')
wt64.tofile('ones_64b.bin')
