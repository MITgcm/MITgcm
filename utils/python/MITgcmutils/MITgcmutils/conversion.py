#
# created EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np

# Global constants
gravity  =  9.810000e+00
rad      =  np.pi/180
def pfromz(rC,rF0=0.0,lat=None,rhoConst=1.027500e+03,eosRefP0=1.013250e+05,
           top_Pres=0.0,surf_pRef=1.013250e+05):
    """
    Computes pressure (dbar) of sea water from depth.

    Parameters
    ----------
    rC    : depth at
          : c point [m]
          : rC may have dims 1x1,
          : kx1, or kxnxm 
    rF0   : depth at rF[k=0] 
          : default=0
    lat   : latitude to compute 
          : unesco gravity
          : lat may have dims 1x1,
          : nx1, nxm or kxnxm 
          : default=None    
    rhoConst  : Density of seawater
              : default=1027.5    
    eosRefP0  : EOS reference pressure (Pa)
              : default=1.013250e+05              
    top_Pres  : reference pressure at the top
               default=0    
    surf_pRef : Surface pressure (Pa)
              : default=1.013250e+05    
    
    Returns
    -------
    p : array_like
        pressure [dbar]

    Example
    -------
    >>> pfromz(-1000)
    1007.9775
    >>> pfromz([-100,-1000])
    100.7978, 1007.9775   
    >>> pfromz([-100,-1000],lat=90)
    101.0256, 1010.2562 
    >>> pfromz(-1000,lat=[70,90])
    1009.6304, 1010.2562
    """
    global gravity
    
    z = np.asfarray(rC)
    
    assert np.any(z<=0),('input_error: values cannot be positive')
       
    if lat is not None:
           
     lat = np.asfarray(lat)
          
     if lat.ndim > 2:
       diml = np.shape(lat)
       dimz = np.shape(z)
       assert len(diml)==len(dimz),('for more than two dimensions, lat and rC'
                                  + ' must have the same number of dimensions') 
       for k in range(0,len(diml)):
         assert diml[k]==dimz[k],('for more than two dimensions, lat and rC'
                                   + ' must have the same dimensions') 
     else:                
       # Check optional shapes for z and lat  

      if z.size!=1 and lat.size!=1:          
        if z.size==1:                    # z is a scalar. Fill to size of lat         
          z = z*np.ones(np.shape(lat))
        elif lat.size==1:                # lat is a scalar. Fill to size of z
          lat = lat*np.ones(np.shape(z))  
        elif z.ndim==1: 
          if lat.ndim==1:
            [lat,z]=np.meshgrid(lat,z)
          else:
            K = len(z) 
            zdim = (K,)+np.shape(lat)   
            z1d=np.copy(z)
            l2d=np.copy(lat)
            z=np.zeros(zdim)
            lat=np.zeros(zdim)
            for k in range(0,K):
              z[k,:,:] = z1d[k]*np.ones(np.shape(l2d))
              lat[k,:,:] = l2d
        
        
     #Redefine gravity (UNESCO Tech. Pap. in Mar. Sci., 1983, eq 27)  
     
     gravity = 9.780318 * (1.0 + (5.2788e-3 + 2.36e-5 * pow(np.sin(lat*rad),2)) 
                             * pow(np.sin(lat*rad),2) )

    pref = surf_pRef - eosRefP0
    dz=z-rF0   
    p=(top_Pres-rhoConst*dz*gravity+pref)*1e-4

    return p

