# generate weights files
import numpy as np

ratio=0.25
terr = np.asarray(
    [0.5201, 0.5199, 0.5201, 0.5142, 0.4917, 0.4707, 0.4324, 0.3782,
     0.3103, 0.2435, 0.1994, 0.1582, 0.1144, 0.0905, 0.0659, 0.0602,
     0.0508, 0.0498, 0.0501, 0.0500, 0.0500, 0.0500, 0.0500])
serr = np.asarray(
    [0.2676, 0.2224, 0.1942, 0.1751, 0.1452, 0.1223, 0.1125, 0.1078,
     0.0884, 0.0785, 0.0777, 0.0702, 0.0710, 0.0599, 0.0510, 0.0408,
     0.0399, 0.0314, 0.0205, 0.0199, 0.0200, 0.0200, 0.0200])

terr = terr/np.sqrt(ratio)
serr = serr/np.sqrt(ratio)

terr.astype('>f4').tofile('t.err')
serr.astype('>f4').tofile('s.err')

# from now deleted "data.err"
# with open('data.err') as f:
#     contents = f.readlines()

# ratio=float(contents[0])
# terr=np.zeros((len(contents)-1,))
# serr=np.zeros((len(contents)-1,))
# for k in range(1,len(contents)):
#     terr[k-1] = float(contents[k].split()[0])/np.sqrt(ratio)
#     serr[k-1] = float(contents[k].split()[1])/np.sqrt(ratio)
