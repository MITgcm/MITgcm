import sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.tri as tri

def contourf(*arguments, **kwargs):
    """Call signatures::

    contourf(X, Y, C, N, **kwargs)
    contourf(X, Y, C, V, **kwargs)
    
    Create a contourf plot of a 2-D llc array (with tricontour).
    
    *C* is the array of color values.

    *N* is the number of levels

    *V* is a list of levels
    
    *X* and *Y*, specify the (*x*, *y*) coordinates of
    the grid points

    **kwargs are passed to tricontour.
    
    """

    arglen = len(arguments)
    h = []
    if arglen >= 3:
        data = np.copy(arguments[2].flatten())
        x = arguments[0].flatten()
        y = arguments[1].flatten()

        # Create the Triangulation; 
        # no triangles so Delaunay triangulation created. 
        triang = tri.Triangulation(x, y)
        ntri = triang.triangles.shape[0]

        # Mask off unwanted triangles.
        mask = np.where(data[triang.triangles].prod(axis=1)==0., 1, 0)
        triang.set_mask(mask)
            
        if arglen == 3:
            h = plt.tricontourf(triang, data, **kwargs)
        elif arglen == 4:
            h = plt.tricontourf(triang, data, arguments[3], **kwargs)
        else:
            print "wrong number of arguments"
            print "need at least 3 or 4 arguments"
            sys.exit(__doc__)

        # show the triangles for debugging
        #plt.triplot(triang, color='0.7')

    else:
        print "wrong number of arguments"
        print "need at least x,y,fld"
        sys.exit(__doc__)

    return h

def contour(*arguments, **kwargs):
    """Call signatures::

    contour(X, Y, C, N, **kwargs)
    contour(X, Y, C, V, **kwargs)
    
    Create a contour plot of a 2-D llc array (with tricontour).
    
    *C* is the array of color values.

    *N* is the number of levels

    *V* is a list of levels
    
    *X* and *Y*, specify the (*x*, *y*) coordinates of
    the grid points

    **kwargs are passed to tricontour.
    
    """

    arglen = len(arguments)
    h = []
    if arglen >= 3:
        data = arguments[2].flatten()
        x = arguments[0].flatten()
        y = arguments[1].flatten()

        # Create the Triangulation; 
        # no triangles so Delaunay triangulation created. 
        triang = tri.Triangulation(x, y)
        ntri = triang.triangles.shape[0]

        # Mask off unwanted triangles.
        mask = np.where(data[triang.triangles].prod(axis=1)==0., 1, 0)
        triang.set_mask(mask)
                        
        if arglen == 3:
            h = plt.tricontour(triang, data, **kwargs)
        elif arglen == 4:
            h = plt.tricontour(triang, data, arguments[3], **kwargs)
        else:
            print "wrong number of arguments"
            print "need at least 3 or 4 arguments"
            sys.exit(__doc__)
        
        # show the triangles for debugging
        #plt.triplot(triang, color='0.7')

    else:
        print "wrong number of arguments"
        print "need at least x,y,fld"
        sys.exit(__doc__)

    return h

def flat(fld, **kwargs):
    """convert mds data into global 2D field
    only fields with 2 to 5 dimensions are allowed"""

    ndims = len(fld.shape)
    if ndims == 2: 
        gfld = _flat2D(fld)
    elif ndims == 3: 
        gfld = [ _flat2D(fld[a,:,:], **kwargs) 
                 for a in range(fld.shape[0]) ] 
    elif ndims == 4: 
        gfld = [ [ _flat2D(fld[a,b,:,:], **kwargs) 
                   for b in range(fld.shape[1]) ] 
                 for a in range(fld.shape[0]) ]
    elif ndims == 5: 
        gfld = [ [ [ _flat2D(fld[a,b,c,:,:], **kwargs) 
                     for c in range(fld.shape[2]) ]
                   for b in range(fld.shape[1]) ]
                 for a in range(fld.shape[0]) ]
    else:
        print "wrong number of dimensions"
        print "only 2 to 5 dimensions are allowed"
        sys.exit(__doc__)

    gfld = np.array(gfld)

    return gfld

def _flat2D(fld, center='Atlantic'):
    """convert mds 2D data into global 2D field"""

    nx = fld.shape[1]
    ny = fld.shape[0]
    n = ny/nx/4
    
    # eastern and western hemispheres
    eastern=np.concatenate((fld[:n*nx,:],fld[n*nx:2*(n*nx)]),axis=1)
    tmp    = fld[2*(n*nx)+nx:,        ::-1]
    western=np.concatenate((tmp[2::n,:].transpose(),
                            tmp[1::n,:].transpose(),
                            tmp[0::n,:].transpose()))
    # Arctic face is special
    arctic  = fld[2*(n*nx):2*(n*nx)+nx,:]
    arctice = np.concatenate((arctic[::-1,:nx/2].transpose(),
                              np.zeros((nx/2,nx))),axis=1)
    arcticw = np.concatenate((arctic[:,nx:nx/2-1:-1].transpose(),
                              np.zeros((nx/2,nx/2)),
                              arctic[nx:nx/2-1:-1,nx/2-1::-1]),axis=1)
    #
    if center == 'Pacific':
        gfld = np.concatenate( ( np.concatenate((eastern,arctice)),
                                 np.concatenate((western,arcticw)) ), axis=1)
    else:
        gfld = np.concatenate( ( np.concatenate((western,arcticw)),
                                 np.concatenate((eastern,arctice)) ), axis=1)
    
    return gfld

def _mds2D(fld,center='Atlantic'):
    """convert global 2D 'flat field' to mds 2D data"""
    
    ni = fld.shape[-1]
    nj = fld.shape[-2]
    nx = ni/4
    ny = nx*(3*4+1)
    n = ny/nx/4

    # arctic face
    arcticw = fld[n*nx:,:nx]
    arctice = fld[n*nx:,2*nx:3*nx]
    arctic = np.concatenate((arctice,arcticw[::-1,::-1]),axis=0)

    # eastern and western hemispheres
    eastern=fld[:n*nx,2*nx:]
    # this is tricky
    western=fld[:n*nx,:2*nx]

    mdsfld = np.concatenate((eastern[:,:nx],
                             eastern[:,nx:],
                             arctic[:,::-1].transpose(),
                             western[::-1,:].transpose().reshape((2*n*nx,nx))),
                             axis=0)
    return mdsfld

def mds(fld,center='Atlantic'):
    """convert global 'flat field in mds data;
    only fields with 2 to 5 dimensions are allowed"""

    ndims = len(fld.shape)
    if ndims == 2: 
        mdsfld = _mds2D(fld)
    elif ndims == 3: 
        mdsfld = [ _mds2D(fld[a,:,:], **kwargs) 
                 for a in range(fld.shape[0]) ] 
    elif ndims == 4: 
        mdsfld = [ [ _mds2D(fld[a,b,:,:], **kwargs) 
                   for b in range(fld.shape[1]) ] 
                 for a in range(fld.shape[0]) ]
    elif ndims == 5: 
        mdsfld = [ [ [ _mds2D(fld[a,b,c,:,:], **kwargs) 
                     for c in range(fld.shape[2]) ]
                   for b in range(fld.shape[1]) ]
                 for a in range(fld.shape[0]) ]
    else:
        print "wrong number of dimensions"
        print "only 2 to 5 dimensions are allowed"
        sys.exit(__doc__)

    mdsfld = np.array(mdsfld)
    
    return mdsfld

def faces(fld):
    """convert mds 2D data into a list with 6 faces"""
    
    nx = fld.shape[-1]
    ny = fld.shape[-2]
    n = ny/nx/4
    
    # divide into faces
    f = []
    f.append(fld[:n*nx,:])
    f.append(fld[n*nx:2*(n*nx),:])
    # arctic face
    f.append(fld[2*(n*nx):2*(n*nx)+nx,:])
    # western hemisphere (why is this so complicated?)
    tmp = fld[2*(n*nx)+nx:,::-1]
    wd = np.concatenate((tmp[2::n,:].transpose(),
                         tmp[1::n,:].transpose(),
                         tmp[0::n,:].transpose())).transpose()
    f.append(wd[:nx,::-1])
    f.append(wd[nx:,::-1])
    # pseudo-sixth face
    f.append(np.zeros((nx,nx)))

    return f


def _sqCoord(a):
    b = np.squeeze(a)
    # it appears to be important, that here we do not mask the array
    # but reset zeros to NaN (only used for coordinate arrays!!!)
#    b = np.ma.masked_where(b==0., b)
    b[b==0.] = np.NaN
    return b

def pcol(*arguments, **kwargs):
    """Call signatures::

    pcol(X, Y, C, **kwargs)
    
    pcol(X, Y, C, m, **kwargs)
    
    Create a pseudo-color plot of a 2-D llc array (with plt.pcolormesh).
    
    *m* if given is the map projection to use
        NOTE: currently not all projections work

    *C* is the array of color values.

    *X* and *Y*, specify the (*x*, *y*) coordinates of
    the grid point corners (G-points)

    **kwargs are passed to plt.pcolormesh.
    
    """

    arglen = len(arguments)
    h = []
    mapit = False
    if arglen < 3:
        print "wrong number of arguments"
        print "need at least x,y,fld"
        sys.exit(__doc__)
    elif arglen > 3:
        mapit = True
        m = arguments[3]

    xg = arguments[0]
    yg = arguments[1]
    data = arguments[2]

    nx = data.shape[-1]
    ny = data.shape[-2]
    n = ny/nx/4
    
    # color range
    cax = [data.min(),data.max()]
    # overwrite if necessary
    if 'vmin' in kwargs: cax[0] = kwargs.pop('vmin','')
    if 'vmax' in kwargs: cax[1] = kwargs.pop('vmax','')
    # divide into faces
    f0 = []
    f0.append(faces(xg))
    f0.append(faces(yg))
    f0.append(faces(data))
    # fill holes in coordinate arrays
#    for t in [0,1,3,4]:
#        inan = f0[2][t]==0 # _sqCoord(f0[2][t])==np.NaN]
#        f0[0][t][inan]=np.NaN
#        f0[1][t][inan]=np.NaN

#    for t in [0,1]:
#        for i in range(nx):
#            for j in range(n*nx):
#                if f0[0][t][j,i]==0:f0[0][t][100,i]
#                if f0[1][t][j,i]==0:f0[1][t][100,i]
#
#    for t in [3,4]:
#        for i in range(n*nx):
#            for j in range(nx):
#                if f0[0][t][j,i]==0:f0[0][t][j,239]
#                if f0[1][t][j,i]==0:f0[1][t][j,239]
        
    # find the missing corners by interpolation
    fo = []
    fo.append( (f0[0][0][-1,0]+f0[0][2][-1,0]+f0[0][4][-1,0])/3. )
    fo.append( (f0[1][2][-1,0]+f0[1][2][-1,0]+f0[1][4][-1,0])/3. )
    fo.append( np.NaN )
    fe = []
    fe.append( (f0[0][1][0,-1]+f0[0][3][0,-1])/2. )
    fe.append( (f0[1][1][0,-1]+f0[1][3][0,-1])/2. )
    fe.append( np.NaN )
    f  = np.copy(f0)
    # fill some gaps at the face boundaries
    for t in [0,2,4]:
        tp = 2*(t/2)
        tpp = tp
        if tp==4: tpp = tp-6
        for k in [0,1,2]:
            tp = min(tp,3)
            f[k][t] = np.concatenate((f0[k][t],f0[k][1+tp][:,:1]),axis=1)
            if k==2: tmp = np.atleast_2d(np.append(f0[k][2+tpp][::-1,:1],fo[k]))
            else:    tmp = np.atleast_2d(np.append(fo[k],f0[k][2+tpp][::-1,:1]))
            f[k][t] = np.concatenate((f[k][t],tmp),axis=0)

    for t in [1,3]:
        tp = 2*(t/2)
        for k in [0,1,2]:
            f[k][t] = np.concatenate((f0[k][t],f0[k][2+tp][:1,:]),axis=0)
            if k==2: tmp = np.atleast_2d(np.append(f0[k][3+tp][:1,::-1],fe[k]))
            else:    tmp = np.atleast_2d(np.append(fe[k],f0[k][3+tp][:1,::-1]))
            f[k][t] = np.concatenate((f[k][t],tmp.transpose()),axis=1)
        
    # make sure that only longitudes of one sign are on individual lateral faces
    i0 = f[0][3]<0.
    f[0][3][i0] = f[0][3][i0]+360.
    # plot the lateral faces
    ph = []
    for t in [0,1,3,4]:
        if mapit: x, y = m(_sqCoord(f[0][t]), _sqCoord(f[1][t]))
        else:     x, y =   _sqCoord(f[0][t]), _sqCoord(f[1][t])
        ph.append(plt.pcolormesh(x,y,f[2][t], **kwargs))
    # plot more lateral faces to be able to select the longitude range later
    f[0][3][i0] = f[0][3][i0]-360.
    i0 = f[0][3]>0.
    f[0][3][i0] = f[0][3][i0]-360.
    f[0][4] = f[0][4]+360.
    for t in [3,4]: 
        if mapit: x, y = m(_sqCoord(f[0][t]), _sqCoord(f[1][t]))
        else:     x, y =   _sqCoord(f[0][t]), _sqCoord(f[1][t])
        ph.append(plt.pcolormesh(x,y,f[2][t], **kwargs))

    # Arctic face is special
    t = 2
    nn = nx/2
    xx = np.copy(f[0][t][:nn,:])
    yy = np.copy(f[1][t][:nn,:])
    zz = np.copy(f[2][t][:nn,:])
    i0 = xx<0.
    xx[i0] = xx[i0]+360.
    if mapit: x, y = m(_sqCoord(xx),_sqCoord(yy))
    else:     x, y =   _sqCoord(xx),_sqCoord(yy)
    ph.append(plt.pcolormesh(x,y,zz, **kwargs))
    #
    nn = nx/2-1
    xx = np.copy(f[0][t][nn:,:])
    yy = np.copy(f[1][t][nn:,:])
    zz = np.copy(f[2][t][nn:,:])
    # need to mask some zz-values so that there is no erroneous wrap-around
    zz = np.ma.masked_where(xx>20.,zz)
    #
    if mapit: x, y = m(_sqCoord(xx),_sqCoord(yy))
    else:     x, y =   _sqCoord(xx),_sqCoord(yy)
    ph.append(plt.pcolormesh(x,y,zz, **kwargs))
    # need to mask some zz-values so that there is no erroneous wrap-around
    zz = np.copy(f[2][t][nn:,:])
    zz = np.ma.masked_where(xx>=0.,zz)
    i0 = xx<0.
    xx[i0] = xx[i0]+360.
    if mapit: x, y = m(_sqCoord(xx),_sqCoord(yy))
    else:     x, y =   _sqCoord(xx),_sqCoord(yy)
    ph.append(plt.pcolormesh(x,y,zz, **kwargs))

    if not mapit:
        plt.xlim([-170,190])
        plt.ylim([-90,90])

    for im in ph:
        im.set_clim(cax[0],cax[1])

    return ph

