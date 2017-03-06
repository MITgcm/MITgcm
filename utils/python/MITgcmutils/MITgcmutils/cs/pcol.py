import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D

def pcol( x, y, data, projection=None, vmin=None, vmax=None, **kwargs):
    """function h=pcol(x,y,v)
    function h=pcol(x,y,v, projection = mp )
    
    plots 2D scalar fields v on the MITgcm cubed sphere grid with pcolormesh.
    x,y are really 'xg', and 'yg', that is, they should be the coordinates
    of the points one half grid cell to the left and bottom, that is
    vorticity points for tracers, etc. 
    
    If present, 'projection' (a basemap instance) is used to transform 
    coordinates. Unfortunatly, cylindrical and conic maps are limited to 
    the [-180 180] range. 
    projection = 'sphere' results in a 3D visualization on the sphere
    without any specific projection. Good for debugging.

    Example script to use pcol.py:

    from mpl_toolkits.basemap import Basemap
    import MITgcmutils as mit
    import matplotlib.pyplot as plt
    from sq import sq

    x=mit.rdmds('XG'); y=mit.rdmds('YG'); e=mit.rdmds('Eta',np.Inf)
    fig = plt.figure();
    mp = Basemap(projection='moll',lon_0 = 0.,
                 resolution = 'l', area_thresh = 1000.)
    plt.clf()
    h = mit.cs.pcol(x,y,sq(e), projection = mp)
    mp.fillcontinents(color = 'grey')
    mp.drawmapboundary()
    mp.drawmeridians(np.arange(0, 360, 30))
    mp.drawparallels(np.arange(-90, 90, 30))
    plt.show()

    """

# pcol first divides the 2D cs-field(6*n,n) into six faces. Then for
# each face, an extra row and colum is added from the neighboring faces in
# order to fool pcolor into drawing the entire field and not just
# (n-1,m-1) data points. There are two corner points that have no explicit
# coordinates so that they have to be found by
# interpolation/averaging. Then each face is divided into 4 tiles,
# assuming cs-geometry, and each tile is plotted individually in
# order to avoid problems due to ambigous longitude values (the jump
# between -180 and 180, or 360 and 0 degrees). As long as the poles
# are at the centers of the north and south faces and the first tile is
# symmetric about its center this should work.

    # get the figure handle
    fig=plt.gcf()

    mapit = 0
    if projection!=None:
        mp = projection
        if mp=='sphere': mapit=-1
        else: mapit = 1

    # convert to [-180 180[ representation
    x = np.where(x>180,x-360.,x)

    ny,nx = data.shape
    # determine range for color range
    cax = [data.min(),data.max()]
    if cax[1]-cax[0]==0: cax = [cax[0]-1,cax[1]+1]

    if vmin!=None: cax[0]=vmin
    if vmax!=None: cax[1]=vmax

    if mapit == -1:
        # set up 3D plot
        if len(fig.axes)>0: 
            # if present, remove and replace the last axis of fig
            geom=fig.axes[-1].get_geometry()
            plt.delaxes(fig.axes[-1])
        else:
            # otherwise use full figure
            geom = ((1,1,1))
        ax = fig.add_subplot(geom[0],geom[1],geom[2],projection = '3d',
                             axisbg='None')
        # define color range
        tmp = data - data.min()
        N = tmp/tmp.max()       
        # use this colormap
        colmap = cm.jet
        colmap.set_bad('w',1.0)
        mycolmap = colmap(N) #cm.jet(N)

    ph=np.array([])
    jc=x.shape[0]//2
    xxf=np.empty((jc+1,jc+1,4))
    yyf=xxf
    ffld=np.empty((jc,jc,4))
    xff=[]
    yff=[]
    fldf=[]
    for k in range(0,6):
        ix = np.arange(0,ny) + k*ny
        xff.append(x[0:ny,ix])
        yff.append(y[0:ny,ix])
        fldf.append(data[0:ny,ix])

    # find the missing corners by interpolation (one in the North Atlantic)
    xfodd = (xff[0][-1,0]+xff[2][-1,0]+xff[4][-1,0])/3.
    yfodd = (yff[0][-1,0]+yff[2][-1,0]+yff[4][-1,0])/3.
    # and one south of Australia
    xfeven= (xff[1][0,-1]+xff[3][0,-1]+xff[5][0,-1])/3.
    yfeven= (yff[1][0,-1]+yff[3][0,-1]+yff[5][0,-1])/3.

    # loop over tiles
    for k in range(0,6):
        kodd  = 2*(k//2)
        kodd2 = kodd
        if kodd==4: kodd2=kodd-6
        keven  = 2*(k//2)
        keven2 = keven
        if keven==4: keven2=keven-6
        fld = fldf[k]
        if np.mod(k+1,2):
            xf = np.vstack( [ np.column_stack( [xff[k],xff[1+kodd][:,0]] ),
                              np.flipud(np.append(xff[2+kodd2][:,0],xfodd))] )
            yf = np.vstack( [ np.column_stack( [yff[k],yff[1+kodd][:,0]] ),
                              np.flipud(np.append(yff[2+kodd2][:,0],yfodd))] )
        else:
            xf = np.column_stack( [np.vstack( [xff[k],xff[2+keven2][0,:]] ),
                                   np.flipud(np.append(xff[3+keven2][0,:],
                                                       xfeven))] )
            yf = np.column_stack( [np.vstack( [yff[k],yff[2+keven2][0,:]] ),
                                   np.flipud(np.append(yff[3+keven2][0,:],
                                                       yfeven))] )

        if mapit==-1:
            ix = np.arange(0,ny) + k*ny
            # no projection at all (projection argument is 'sphere'), 
            # just convert to cartesian coordinates and plot a 3D sphere
            deg2rad=np.pi/180.
            xcart,ycart,zcart = sph2cart( xf*deg2rad, yf*deg2rad )
            ax.plot_surface(xcart,ycart,zcart,rstride=1,cstride=1,
                            facecolors=mycolmap[0:ny,ix],
                            linewidth=2,shade=False)
            ph = np.append(ph, ax)
        else:
            # divide all faces into 4 because potential problems arise at
            # the centers 
            for kf in range(0,4):
                if   kf==0: i0,i1,j0,j1 =  0,  jc+1, 0,  jc+1
                elif kf==1: i0,i1,j0,j1 =  0,  jc+1,jc,2*jc+1
                elif kf==2: i0,i1,j0,j1 = jc,2*jc+1, 0,  jc+1
                elif kf==3: i0,i1,j0,j1 = jc,2*jc+1,jc,2*jc+1
                xx = xf[i0:i1,j0:j1]
                yy = yf[i0:i1,j0:j1]
                ff = fld[i0:i1-1,j0:j1-1]
                if np.median(xx) < 0:
                    xx = np.where(xx>=180,xx-360.,xx)
                else:
                    xx = np.where(xx<=-180,xx+360.,xx)
             
                # if provided use projection
                if mapit==1: xx,yy = mp(xx,yy)
            
                # now finally plot 4x6 tiles
                ph = np.append(ph, plt.pcolormesh(xx, yy, ff,
                                                  vmin=cax[0], vmax=cax[1],
                                                  **kwargs))

    if mapit == -1: 
        ax.axis('image')
        ax.set_axis_off()
#        ax.set_visible=False
        # add a reasonable colormap
        m = cm.ScalarMappable(cmap=colmap)
        m.set_array(data)
        plt.colorbar(m)
    elif mapit == 0:
        ax = fig.axes[-1]
        ax.axis('image')
        plt.grid('on')

    return ph

def sph2cart(azim_sph_coord, elev_sph_coord):
    r = np.cos(elev_sph_coord)
    x = -r * np.sin(azim_sph_coord)
    y = r * np.cos(azim_sph_coord)
    z = np.sin(elev_sph_coord)
    return x, y, z
